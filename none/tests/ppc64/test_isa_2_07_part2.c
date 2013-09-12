/*  Copyright (C) 2013 IBM

 Authors: Carl Love  <carll@us.ibm.com>
          Maynard Johnson <maynardj@us.ibm.com>

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
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307, USA.

 The GNU General Public License is contained in the file COPYING.

 This program is based heavily on the test_isa_2_06_part*.c source files.
 */

#include <stdio.h>

#ifdef HAS_ISA_2_07

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <altivec.h>
#include <math.h>

#ifndef __powerpc64__
typedef uint32_t HWord_t;
#else
typedef uint64_t HWord_t;
#endif /* __powerpc64__ */

static int errors;
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");
register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");

static volatile unsigned int cond_reg;

#define True  1
#define False 0

#define ALLCR "cr0","cr1","cr2","cr3","cr4","cr5","cr6","cr7"

#define SET_CR(_arg) \
      __asm__ __volatile__ ("mtcr  %0" : : "b"(_arg) : ALLCR );

#define SET_XER(_arg) \
      __asm__ __volatile__ ("mtxer %0" : : "b"(_arg) : "xer" );

#define GET_CR(_lval) \
      __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) )

#define GET_XER(_lval) \
      __asm__ __volatile__ ("mfxer %0" : "=b"(_lval) )

#define GET_CR_XER(_lval_cr,_lval_xer) \
   do { GET_CR(_lval_cr); GET_XER(_lval_xer); } while (0)

#define SET_CR_ZERO \
      SET_CR(0)

#define SET_XER_ZERO \
      SET_XER(0)

#define SET_CR_XER_ZERO \
   do { SET_CR_ZERO; SET_XER_ZERO; } while (0)

#define SET_FPSCR_ZERO \
   do { double _d = 0.0; \
        __asm__ __volatile__ ("mtfsf 0xFF, %0" : : "f"(_d) ); \
   } while (0)


typedef void (*test_func_t)(void);
typedef struct vsx_logic_test logic_test_t;
typedef struct ldst_test ldst_test_t;
typedef struct xs_conv_test xs_conv_test_t;
typedef struct vx_fp_test vx_fp_test_t;
typedef struct vx_fp_test2 vx_fp_test2_t;
typedef struct test_table test_table_t;

typedef unsigned char Bool;


/* These functions below that construct a table of floating point
 * values were lifted from none/tests/ppc32/jm-insns.c.
 */

#if defined (DEBUG_ARGS_BUILD)
#define AB_DPRINTF(fmt, args...) do { fprintf(stderr, fmt , ##args); } while (0)
#else
#define AB_DPRINTF(fmt, args...) do { } while (0)
#endif

static inline void register_farg (void *farg,
                                  int s, uint16_t _exp, uint64_t mant)
{
   uint64_t tmp;

   tmp = ((uint64_t)s << 63) | ((uint64_t)_exp << 52) | mant;
   *(uint64_t *)farg = tmp;
   AB_DPRINTF("%d %03x %013llx => %016llx %0e\n",
              s, _exp, mant, *(uint64_t *)farg, *(double *)farg);
}

static inline void register_sp_farg (void *farg,
                                     int s, uint16_t _exp, uint32_t mant)
{
   uint32_t tmp;
   tmp = ((uint32_t)s << 31) | ((uint32_t)_exp << 23) | mant;
   *(uint32_t *)farg = tmp;
}


typedef struct fp_test_args {
   int fra_idx;
   int frb_idx;
   int cr_flags;
   unsigned long long dp_bin_result;
} fp_test_args_t;

static int nb_special_fargs;
static double * spec_fargs;
static float * spec_sp_fargs;

static void build_special_fargs_table(void)
{
   /* The special floating point values created below are for
    * use in the ftdiv tests for setting the fe_flag and fg_flag,
    * but they can also be used for other tests (e.g., xscmpudp).
    *
    * Note that fl_flag is 'always '1' on ppc64 Linux.
    *
  Entry  Sign Exp   fraction                  Special value
   0      0   3fd   0x8000000000000ULL         Positive finite number
   1      0   404   0xf000000000000ULL         ...
   2      0   001   0x8000000b77501ULL         ...
   3      0   7fe   0x800000000051bULL         ...
   4      0   012   0x3214569900000ULL         ...
   5      0   000   0x0000000000000ULL         +0.0 (+zero)
   6      1   000   0x0000000000000ULL         -0.0 (-zero)
   7      0   7ff   0x0000000000000ULL         +infinity
   8      1   7ff   0x0000000000000ULL         -infinity
   9      0   7ff   0x7FFFFFFFFFFFFULL         +SNaN
   10     1   7ff   0x7FFFFFFFFFFFFULL         -SNaN
   11     0   7ff   0x8000000000000ULL         +QNaN
   12     1   7ff   0x8000000000000ULL         -QNaN
   13     1   000   0x8340000078000ULL         Denormalized val (zero exp and non-zero fraction)
   14     1   40d   0x0650f5a07b353ULL         Negative finite number
    */

   uint64_t mant;
   uint32_t mant_sp;
   uint16_t _exp;
   int s;
   int j, i = 0;

   if (spec_fargs)
      return;

   spec_fargs = malloc( 20 * sizeof(double) );
   spec_sp_fargs = malloc( 20 * sizeof(float) );

   // #0
   s = 0;
   _exp = 0x3fd;
   mant = 0x8000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #1
   s = 0;
   _exp = 0x404;
   mant = 0xf000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #2
   s = 0;
   _exp = 0x001;
   mant = 0x8000000b77501ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #3
   s = 0;
   _exp = 0x7fe;
   mant = 0x800000000051bULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #4
   s = 0;
   _exp = 0x012;
   mant = 0x3214569900000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* Special values */
   /* +0.0      : 0 0x000 0x0000000000000 */
   // #5
   s = 0;
   _exp = 0x000;
   mant = 0x0000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* -0.0      : 1 0x000 0x0000000000000 */
   // #6
   s = 1;
   _exp = 0x000;
   mant = 0x0000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* +infinity : 0 0x7FF 0x0000000000000  */
   // #7
   s = 0;
   _exp = 0x7FF;
   mant = 0x0000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* -infinity : 1 0x7FF 0x0000000000000 */
   // #8
   s = 1;
   _exp = 0x7FF;
   mant = 0x0000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /*
    * This comment applies to values #9 and #10 below:
    * When src is a SNaN, it's converted to a QNaN first before rounding to single-precision,
    * so we can't just copy the double-precision value to the corresponding slot in the
    * single-precision array (i.e., in the loop at the end of this function).  Instead, we
    * have to manually set the bits using register_sp_farg().
    */

   /* +SNaN     : 0 0x7FF 0x7FFFFFFFFFFFF */
   // #9
   s = 0;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);
   _exp = 0xff;
   mant_sp = 0x3FFFFF;
   register_sp_farg(&spec_sp_fargs[i-1], s, _exp, mant_sp);

   /* -SNaN     : 1 0x7FF 0x7FFFFFFFFFFFF */
   // #10
   s = 1;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);
   _exp = 0xff;
   mant_sp = 0x3FFFFF;
   register_sp_farg(&spec_sp_fargs[i-1], s, _exp, mant_sp);

   /* +QNaN     : 0 0x7FF 0x8000000000000 */
   // #11
   s = 0;
   _exp = 0x7FF;
   mant = 0x8000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* -QNaN     : 1 0x7FF 0x8000000000000 */
   // #12
   s = 1;
   _exp = 0x7FF;
   mant = 0x8000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* denormalized value */
   // #13
   s = 1;
   _exp = 0x000;
   mant = 0x8340000078000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* Negative finite number */
   // #14
   s = 1;
   _exp = 0x40d;
   mant = 0x0650f5a07b353ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* A few positive finite numbers ... */
   // #15
   s = 0;
   _exp = 0x412;
   mant = 0x32585a9900000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #16
   s = 0;
   _exp = 0x413;
   mant = 0x82511a2000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #17
   s = 0;
   _exp = 0x403;
   mant = 0x12ef5a9300000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #18
   s = 0;
   _exp = 0x405;
   mant = 0x14bf5d2300000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   // #19
   s = 0;
   _exp = 0x409;
   mant = 0x76bf982440000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);


   nb_special_fargs = i;
   for (j = 0; j < i; j++) {
      if (!(j == 9 || j == 10))
         spec_sp_fargs[j] = spec_fargs[j];
   }
}

struct test_table
{
   test_func_t test_category;
   char * name;
};


typedef enum {
   SINGLE_TEST,
   DOUBLE_TEST,
   DOUBLE_TEST_SINGLE_RES
} precision_type_t;

typedef enum {
   VX_FP_SMAS,   // multiply add single precision result
   VX_FP_SMSS,   // multiply sub single precision result
   VX_FP_SNMAS,  // negative multiply add single precision result
   VX_FP_SNMSS,  // negative multiply sub single precision result
   VX_FP_OTHER,
   VX_CONV_WORD,
   VX_ESTIMATE,
   VX_CONV_TO_SINGLE,
   VX_CONV_TO_DOUBLE,
   VX_SCALAR_CONV_TO_WORD,
   VX_DEFAULT
} vx_fp_test_type;


struct vx_fp_test2
{
   test_func_t test_func;
   const char *name;
   fp_test_args_t * targs;
   int num_tests;
   precision_type_t precision;
   vx_fp_test_type test_type;
   const char * op;
};

static vector unsigned int vec_out, vec_inB;

static void test_xscvdpspn(void)
{
   __asm__ __volatile__ ("xscvdpspn   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvspdpn(void)
{
   __asm__ __volatile__ ("xscvspdpn  %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}
static vx_fp_test2_t
vsx_one_fp_arg_tests[] = {
                                { &test_xscvdpspn, "xscvdpspn", NULL, 20, DOUBLE_TEST, VX_CONV_TO_SINGLE, "conv"},
                                { &test_xscvspdpn, "xscvspdpn", NULL, 20, SINGLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { NULL, NULL, NULL, 0, 0, 0, NULL}
};


static void test_vsx_one_fp_arg(void)
{
   test_func_t func;
   int k;
   k = 0;
   build_special_fargs_table();

   while ((func = vsx_one_fp_arg_tests[k].test_func)) {
      int idx, i;
      vx_fp_test2_t test_group = vsx_one_fp_arg_tests[k];
      /* size of source operands */
      Bool dp  = ((test_group.precision == DOUBLE_TEST) ||
		  (test_group.precision == DOUBLE_TEST_SINGLE_RES)) ? True : False;
      /* size of result */
      Bool is_sqrt = (strstr(test_group.name, "sqrt")) ? True : False;
      Bool is_scalar = (strstr(test_group.name, "xs")) ? True : False;
      Bool sparse_sp = False;
      int stride = dp ? 2 : 4;
      int loops = is_scalar ? 1 : stride;
      stride = is_scalar ? 1: stride;

      /* For conversions of single to double, the 128-bit input register is sparsely populated:
       *    |___ SP___|_Unused_|___SP___|__Unused__|   // for vector op
       *                     or
       *    |___ SP___|_Unused_|_Unused_|__Unused__|   // for scalar op
       *
       * For the vector op case, we need to adjust stride from '4' to '2', since
       * we'll only be loading two values per loop into the input register.
       */
      if (!dp && !is_scalar && test_group.test_type == VX_CONV_TO_DOUBLE) {
         sparse_sp = True;
         stride = 2;
      }

      for (i = 0; i < test_group.num_tests; i+=stride) {
         unsigned int * pv;
         void * inB;

         pv = (unsigned int *)&vec_out;
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         if (dp) {
            int j;
            unsigned long long * frB_dp, *dst_dp;
            for (j = 0; j < loops; j++) {
               inB = (void *)&spec_fargs[i + j];
               // copy double precision FP into vector element i
               memcpy(((void *)&vec_inB) + (j * 8), inB, 8);
            }
            // execute test insn
            (*func)();
            dst_dp = (unsigned long long *) &vec_out;
            printf("#%d: %s ", i/stride, test_group.name);
            for (j = 0; j < loops; j++) {
               if (j)
                  printf("; ");
               frB_dp = (unsigned long long *)&spec_fargs[i + j];
               printf("%s(%016llx)", test_group.op, *frB_dp);
               vx_fp_test_type type = test_group.test_type;
               switch (type) {
                  case VX_SCALAR_CONV_TO_WORD:
                     printf(" = %016llx", dst_dp[j] & 0x00000000ffffffffULL);
                     break;
                  case VX_CONV_TO_SINGLE:
                     printf(" = %016llx", dst_dp[j] & 0xffffffff00000000ULL);
                     break;
                  default:  // For VX_CONV_TO_DOUBLE and non-convert instructions . . .
                     printf(" = %016llx", dst_dp[j]);
               }
            }
            printf("\n");
         } else {
            int j, skip_slot;
            unsigned int * frB_sp, * dst_sp = NULL;
            unsigned long long * dst_dp = NULL;
            if (sparse_sp) {
               skip_slot = 1;
               loops = 2;
            } else {
               skip_slot = 0;
            }
            for (j = 0; j < loops; j++) {
               inB = (void *)&spec_sp_fargs[i + j];
               // copy single precision FP into vector element i

               if (skip_slot && j > 0)
                  memcpy(((void *)&vec_inB) + ((j + j) * 4), inB, 4);
               else
                  memcpy(((void *)&vec_inB) + (j * 4), inB, 4);
            }
            // execute test insn
            (*func)();
            if (test_group.test_type == VX_CONV_TO_DOUBLE)
               dst_dp = (unsigned long long *) &vec_out;
            else
               dst_sp = (unsigned int *) &vec_out;
            // print result
            printf("#%d: %s ", i/stride, test_group.name);
            for (j = 0; j < loops; j++) {
               if (j)
                  printf("; ");
               frB_sp = (unsigned int *)&spec_sp_fargs[i + j];
               printf("%s(%08x)", test_group.op, *frB_sp);
               if (test_group.test_type == VX_CONV_TO_DOUBLE)
                     printf(" = %016llx", dst_dp[j]);
               else
               /* Special case: Current VEX implementation for fsqrts (single precision)
                * uses the same implementation as that used for double precision fsqrt.
                * However, I've found that for xvsqrtsp, the result from that implementation
                * may be off by the two LSBs.  Generally, even this small inaccuracy can cause the
                * output to appear very different if you end up with a carry.  But for the given
                * inputs in this testcase, we can simply mask out these bits.
                */
                  printf(" = %08x", is_sqrt ? (dst_sp[j] & 0xfffffffc) : dst_sp[j]);
            }
            printf("\n");
         }
      }
      k++;
      printf( "\n" );
   }
}

//----------------------------------------------------------

static test_table_t all_tests[] = {
                                     { &test_vsx_one_fp_arg,
                                       "Test VSX vector and scalar single argument instructions"} ,
                                     { NULL, NULL }
};

#endif

int main(int argc, char *argv[])
{

#ifdef HAS_ISA_2_07
   test_table_t aTest;
   test_func_t func;
   int i = 0;

   while ((func = all_tests[i].test_category)) {
      aTest = all_tests[i];
      printf( "%s\n", aTest.name );
      (*func)();
      i++;
   }
   if (errors)
      printf("Testcase FAILED with %d errors \n", errors);
   else
      printf("Testcase PASSED\n");

#else
   printf("NO ISA 2.07 SUPPORT\n");
#endif
   return 0;
}
