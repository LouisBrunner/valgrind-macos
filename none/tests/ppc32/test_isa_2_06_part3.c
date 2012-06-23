/*  Copyright (C) 2011 IBM

 Author: Maynard Johnson <maynardj@us.ibm.com>

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
 */

#ifdef HAS_VSX

#include <stdio.h>
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

typedef unsigned char Bool;
#define True 1
#define False 0
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");
register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");

static volatile unsigned int div_flags, div_xer;

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
typedef struct test_table test_table_t;


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


typedef struct fp_test_args {
   int fra_idx;
   int frb_idx;
} fp_test_args_t;


fp_test_args_t two_arg_fp_tests[] = {
                                     {8, 8},
                                     {8, 14},
                                     {15, 16},
                                     {8, 5},
                                     {8, 4},
                                     {8, 7},
                                     {8, 9},
                                     {8, 11},
                                     {14, 8},
                                     {14, 14},
                                     {14, 6},
                                     {14, 5},
                                     {14, 4},
                                     {14, 7},
                                     {14, 9},
                                     {14, 11},
                                     {6, 8},
                                     {6, 14},
                                     {6, 6},
                                     {6, 5},
                                     {6, 4},
                                     {6, 7},
                                     {6, 9},
                                     {6, 11},
                                     {5, 8},
                                     {5, 14},
                                     {5, 6},
                                     {5, 5},
                                     {5, 4},
                                     {5, 7},
                                     {5, 9},
                                     {5, 11},
                                     {4, 8},
                                     {4, 14},
                                     {4, 6},
                                     {4, 5},
                                     {4, 1},
                                     {4, 7},
                                     {4, 9},
                                     {4, 11},
                                     {7, 8},
                                     {7, 14},
                                     {7, 6},
                                     {7, 5},
                                     {7, 4},
                                     {7, 7},
                                     {7, 9},
                                     {7, 11},
                                     {10, 8},
                                     {10, 14},
                                     {12, 6},
                                     {12, 5},
                                     {10, 4},
                                     {10, 7},
                                     {10, 9},
                                     {10, 11},
                                     {12, 8 },
                                     {12, 14},
                                     {12, 6},
                                     {15, 16},
                                     {15, 16},
                                     {9, 11},
                                     {11, 11},
                                     {11, 12},
                                     {16, 18},
                                     {17, 16},
                                     {19, 19},
                                     {19, 18}
};


static int nb_special_fargs;
static double * spec_fargs;
static float * spec_sp_fargs;

static void build_special_fargs_table(void)
{
/*
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
   15     0   412   0x32585a9900000ULL         A few more positive finite numbers
   16     0   413   0x82511a2000000ULL         ...
   17  . . . . . . . . . . . . . . . . . . . . . . .
   18  . . . . . . . . . . . . . . . . . . . . . . .
   19  . . . . . . . . . . . . . . . . . . . . . . .
*/

   uint64_t mant;
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

   /* +QNaN     : 0 0x7FF 0x7FFFFFFFFFFFF */
   // #9
   s = 0;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* -QNaN     : 1 0x7FF 0x7FFFFFFFFFFFF */
   // #10
   s = 1;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* +SNaN     : 0 0x7FF 0x8000000000000 */
   // #11
   s = 0;
   _exp = 0x7FF;
   mant = 0x8000000000000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* -SNaN     : 1 0x7FF 0x8000000000000 */
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
      spec_sp_fargs[j] = spec_fargs[j];
   }
}


struct test_table
{
   test_func_t test_category;
   char * name;
};

/*  Type of input for floating point operations.*/
typedef enum {
   SINGLE_TEST,
   DOUBLE_TEST
} precision_type_t;

typedef enum {
   VX_SCALAR_CONV_TO_WORD,
   VX_CONV_TO_SINGLE,
   VX_CONV_TO_DOUBLE,
   VX_ESTIMATE,
   VX_DEFAULT
} vx_fp_test_type;

static vector unsigned int vec_out, vec_inA, vec_inB;

/* This function is for checking the reciprocal and reciprocal square root
 * estimate instructions.
 */
Bool check_estimate(precision_type_t type, Bool is_rsqrte, int idx, int output_vec_idx)
{
   /* Technically, the number of bits of precision for xvredp and xvrsqrtedp is
    * 14 bits (14 = log2 16384).  However, the VEX emulation of these instructions
    * does an actual reciprocal calculation versus estimation, so the answer we get back from
    * valgrind can easily differ from the estimate in the lower bits (within the 14 bits of
    * precision) and the estimate may still be within expected tolerances.  On top of that,
    * we can't count on these estimates always being the same across implementations.
    * For example, with the fre[s] instruction (which should be correct to within one part
    * in 256 -- i.e., 8 bits of precision) . . . When approximating the value 1.0111_1111_1111,
    * one implementation could return 1.0111_1111_0000 and another implementation could return
    * 1.1000_0000_0000.  Both estimates meet the 1/256 accuracy requirement, but share only a
    * single bit in common.
    *
    * The upshot is we can't validate the VEX output for these instructions by comparing against
    * stored bit patterns.  We must check that the result is within expected tolerances.
    */


   /* A mask to be used for validation as a last resort.
    * Only use 12 bits of precision for reasons discussed above.
    */
#define VSX_RECIP_ESTIMATE_MASK_DP 0xFFFFFF0000000000ULL
#define VSX_RECIP_ESTIMATE_MASK_SP 0xFFFFFF00

   Bool result = False;
   Bool dp_test = type == DOUBLE_TEST;
   double src_dp, res_dp;
   float src_sp, res_sp;
   src_dp = res_dp = 0;
   src_sp = res_sp = 0;
#define SRC (dp_test ? src_dp : src_sp)
#define RES (dp_test ? res_dp : res_sp)
   Bool src_is_negative = False;
   Bool res_is_negative = False;
   unsigned long long * dst_dp = NULL;
   unsigned int * dst_sp = NULL;
   if (dp_test) {
      unsigned long long * src_dp_ull;
      dst_dp = (unsigned long long *) &vec_out;
      src_dp = spec_fargs[idx];
      src_dp_ull = (unsigned long long *) &src_dp;
      src_is_negative = (*src_dp_ull & 0x8000000000000000ULL) ? True : False;
      res_is_negative = (dst_dp[output_vec_idx] & 0x8000000000000000ULL) ? True : False;
      memcpy(&res_dp, &dst_dp[output_vec_idx], 8);
   } else {
      unsigned int * src_sp_uint;
      dst_sp = (unsigned int *) &vec_out;
      src_sp = spec_sp_fargs[idx];
      src_sp_uint = (unsigned int *) &src_sp;
      src_is_negative = (*src_sp_uint & 0x80000000) ? True : False;
      res_is_negative = (dst_sp[output_vec_idx] & 0x80000000) ? True : False;
      memcpy(&res_sp, &dst_sp[output_vec_idx], 4);
   }

   // Below are common rules for xvre{d|s}p and xvrsqrte{d|s}p
   if (isnan(SRC))
      return isnan(RES);
   if (fpclassify(SRC) == FP_ZERO)
      return isinf(RES);
   if (!src_is_negative && isinf(SRC))
      return !res_is_negative && (fpclassify(RES) == FP_ZERO);
   if (is_rsqrte) {
      if (src_is_negative)
         return isnan(RES);
   } else {
      if (src_is_negative && isinf(SRC))
         return res_is_negative && (fpclassify(RES) == FP_ZERO);
   }
   if (dp_test) {
      double calc_diff;
      double real_diff;
      double recip_divisor;
      double div_result;
      double calc_diff_tmp;

      if (is_rsqrte)
         recip_divisor = sqrt(src_dp);
      else
         recip_divisor = src_dp;

      div_result = 1.0/recip_divisor;
      calc_diff_tmp = recip_divisor * 16384.0;
      if (isnormal(calc_diff_tmp)) {
         calc_diff = fabs(1.0/calc_diff_tmp);
         real_diff = fabs(res_dp - div_result);
         result = ( ( res_dp == div_result )
                  || ( real_diff <= calc_diff ) );
      } else {
         /* Unable to compute theoretical difference, so we fall back to masking out
          * un-precise bits.
          */
         unsigned long long * div_result_dp = (unsigned long long *) &div_result;
         result = (dst_dp[output_vec_idx] & VSX_RECIP_ESTIMATE_MASK_DP) == (*div_result_dp & VSX_RECIP_ESTIMATE_MASK_DP);
      }
      /* For debug use . . .
         if (!result) {
             unsigned long long * dv = &div_result;
             unsigned long long * rd = &real_diff;
             unsigned long long * cd = &calc_diff;
             printf("\n\t {actual div_result: %016llx; real_diff:  %016llx; calc_diff:  %016llx}\n",
       *dv, *rd, *cd);
          }
       */
   } else {  // single precision test (only have xvrsqrtesp, since xvresp was implemented in stage 2)
      float calc_diff;
      float real_diff;
      float div_result;
      float calc_diff_tmp;
      float recip_divisor = sqrt(src_sp);

      div_result = 1.0/recip_divisor;
      calc_diff_tmp = recip_divisor * 16384.0;
      if (isnormal(calc_diff_tmp)) {
         calc_diff = fabsf(1.0/calc_diff_tmp);
         real_diff = fabsf(res_sp - div_result);
         result = ( ( res_sp == div_result )
                  || ( real_diff <= calc_diff ) );
      } else {
         /* Unable to compute theoretical difference, so we fall back to masking out
          * un-precise bits.
          */
         unsigned int * div_result_sp = (unsigned int *) &div_result;
         result = (dst_sp[output_vec_idx] & VSX_RECIP_ESTIMATE_MASK_SP) == (*div_result_sp & VSX_RECIP_ESTIMATE_MASK_SP);
      }
      /* For debug use . . .
         if (!result) {
             unsigned long long * dv = &div_result;
             unsigned long long * rd = &real_diff;
             unsigned long long * cd = &calc_diff;
             printf("\n\t {actual div_result: %016llx; real_diff:  %016llx; calc_diff:  %016llx}\n",
       *dv, *rd, *cd);
          }
       */
   }
   return result;
}

typedef struct vx_fp_test
{
   test_func_t test_func;
   const char * name;
   fp_test_args_t * targs;
   int num_tests;
   precision_type_t precision;
   vx_fp_test_type type;
   const char * op;
} vx_fp_test_t;


static Bool do_dot;

static void test_xvredp(void)
{
   __asm__ __volatile__ ("xvredp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsredp(void)
{
   __asm__ __volatile__ ("xsredp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrsqrtedp(void)
{
   __asm__ __volatile__ ("xvrsqrtedp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrsqrtedp(void)
{
   __asm__ __volatile__ ("xsrsqrtedp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrsqrtesp(void)
{
   __asm__ __volatile__ ("xvrsqrtesp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xstsqrtdp(void)
{
   __asm__ __volatile__ ("xstsqrtdp   cr1, %x0" : : "wa" (vec_inB));
}

static void test_xvtsqrtdp(void)
{
   __asm__ __volatile__ ("xvtsqrtdp   cr1, %x0" : : "wa" (vec_inB));
}

static void test_xvtsqrtsp(void)
{
   __asm__ __volatile__ ("xvtsqrtsp   cr1, %x0" : : "wa" (vec_inB));
}

static void test_xvsqrtdp(void)
{
   __asm__ __volatile__ ("xvsqrtdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvsqrtsp(void)
{
   __asm__ __volatile__ ("xvsqrtsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvtdivdp(void)
{
   __asm__ __volatile__ ("xvtdivdp   cr1, %x0, %x1" : : "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xvtdivsp(void)
{
   __asm__ __volatile__ ("xvtdivsp   cr1, %x0, %x1" : : "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xscvdpsp(void)
{
   __asm__ __volatile__ ("xscvdpsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvdpuxws(void)
{
   __asm__ __volatile__ ("xscvdpuxws   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvspdp(void)
{
   __asm__ __volatile__ ("xscvspdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvdpsp(void)
{
   __asm__ __volatile__ ("xvcvdpsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvdpuxds(void)
{
   __asm__ __volatile__ ("xvcvdpuxds   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvdpuxws(void)
{
   __asm__ __volatile__ ("xvcvdpuxws   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvspdp(void)
{
   __asm__ __volatile__ ("xvcvspdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvspsxds(void)
{
   __asm__ __volatile__ ("xvcvspsxds   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvspuxds(void)
{
   __asm__ __volatile__ ("xvcvspuxds   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvdpsxds(void)
{
   __asm__ __volatile__ ("xvcvdpsxds   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvspuxws(void)
{
   __asm__ __volatile__ ("xvcvspuxws   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvsxddp(void)
{
   __asm__ __volatile__ ("xvcvsxddp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvuxddp(void)
{
   __asm__ __volatile__ ("xvcvuxddp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvsxdsp(void)
{
   __asm__ __volatile__ ("xvcvsxdsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvuxdsp(void)
{
   __asm__ __volatile__ ("xvcvuxdsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvsxwdp(void)
{
   __asm__ __volatile__ ("xvcvsxwdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvuxwdp(void)
{
   __asm__ __volatile__ ("xvcvuxwdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvsxwsp(void)
{
   __asm__ __volatile__ ("xvcvsxwsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvuxwsp(void)
{
   __asm__ __volatile__ ("xvcvuxwsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrdpic(void)
{
   __asm__ __volatile__ ("xsrdpic   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrdpiz(void)
{
   __asm__ __volatile__ ("xsrdpiz   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrdpi(void)
{
   __asm__ __volatile__ ("xsrdpi   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvabsdp(void)
{
   __asm__ __volatile__ ("xvabsdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvnabsdp(void)
{
   __asm__ __volatile__ ("xvnabsdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvnegdp(void)
{
   __asm__ __volatile__ ("xvnegdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvabssp(void)
{
   __asm__ __volatile__ ("xvabssp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvnabssp(void)
{
   __asm__ __volatile__ ("xvnabssp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrdpi(void)
{
   __asm__ __volatile__ ("xvrdpi   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrdpic(void)
{
   __asm__ __volatile__ ("xvrdpic   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrdpim(void)
{
   __asm__ __volatile__ ("xvrdpim   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrdpip(void)
{
   __asm__ __volatile__ ("xvrdpip   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrdpiz(void)
{
   __asm__ __volatile__ ("xvrdpiz   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrspi(void)
{
   __asm__ __volatile__ ("xvrspi   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrspic(void)
{
   __asm__ __volatile__ ("xvrspic   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrspim(void)
{
   __asm__ __volatile__ ("xvrspim   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrspip(void)
{
   __asm__ __volatile__ ("xvrspip   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvrspiz(void)
{
   __asm__ __volatile__ ("xvrspiz   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static vx_fp_test_t
vsx_one_fp_arg_tests[] = {
                                { &test_xvredp, "xvredp", NULL, 18, DOUBLE_TEST, VX_ESTIMATE, "1/x"},
                                { &test_xsredp, "xsredp", NULL, 18, DOUBLE_TEST, VX_ESTIMATE, "1/x"},
                                { &test_xvrsqrtedp, "xvrsqrtedp", NULL, 18, DOUBLE_TEST, VX_ESTIMATE, "1/x-sqrt"},
                                { &test_xsrsqrtedp, "xsrsqrtedp", NULL, 18, DOUBLE_TEST, VX_ESTIMATE, "1/x-sqrt"},
                                { &test_xvrsqrtesp, "xvrsqrtesp", NULL, 18, SINGLE_TEST, VX_ESTIMATE, "1/x-sqrt"},
                                { &test_xvsqrtdp, "xvsqrtdp", NULL, 18, DOUBLE_TEST, VX_DEFAULT, "sqrt"},
                                { &test_xvsqrtsp, "xvsqrtsp", NULL, 18, SINGLE_TEST, VX_DEFAULT, "sqrt"},
                                { &test_xscvdpsp, "xscvdpsp", NULL, 20, DOUBLE_TEST, VX_CONV_TO_SINGLE, "conv"},
                                { &test_xscvdpuxws, "xscvdpuxws", NULL, 20, DOUBLE_TEST, VX_SCALAR_CONV_TO_WORD, "conv"},
                                { &test_xscvspdp, "xscvspdp", NULL, 20, SINGLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvdpsp, "xvcvdpsp", NULL, 20, DOUBLE_TEST, VX_CONV_TO_SINGLE, "conv"},
                                { &test_xvcvdpuxds, "xvcvdpuxds", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvdpuxws, "xvcvdpuxws", NULL, 20, DOUBLE_TEST, VX_CONV_TO_SINGLE, "conv"},
                                { &test_xvcvspdp, "xvcvspdp", NULL, 20, SINGLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvspsxds, "xvcvspsxds", NULL, 20, SINGLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvdpsxds, "xvcvdpsxds", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvspuxds, "xvcvspuxds", NULL, 20, SINGLE_TEST, VX_CONV_TO_DOUBLE, "conv"},
                                { &test_xvcvspuxws, "xvcvspuxws", NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "conv"},
                                { &test_xsrdpic, "xsrdpic", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xsrdpiz, "xsrdpiz", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xsrdpi, "xsrdpi", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvabsdp, "xvabsdp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "abs"},
                                { &test_xvnabsdp, "xvnabsdp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "nabs"},
                                { &test_xvnegdp, "xvnegdp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "neg"},
                                { &test_xvabssp, "xvabssp", NULL, 20, SINGLE_TEST, VX_DEFAULT, "abs"},
                                { &test_xvnabssp, "xvnabssp", NULL, 20, SINGLE_TEST, VX_DEFAULT, "nabs"},
                                { &test_xvrdpi,  "xvrdpi",  NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvrdpic, "xvrdpic", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvrdpim, "xvrdpim", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvrdpip, "xvrdpip", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvrdpiz, "xvrdpiz", NULL, 20, DOUBLE_TEST, VX_CONV_TO_DOUBLE, "round"},
                                { &test_xvrspi,  "xvrspi",  NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "round"},
                                { &test_xvrspic, "xvrspic", NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "round"},
                                { &test_xvrspim, "xvrspim", NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "round"},
                                { &test_xvrspip, "xvrspip", NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "round"},
                                { &test_xvrspiz, "xvrspiz", NULL, 20, SINGLE_TEST, VX_CONV_TO_SINGLE, "round"},
                                { NULL, NULL, NULL, 0, 0, 0, NULL}
};

static vx_fp_test_t
vx_tdivORtsqrt_tests[] = {
                          { &test_xstsqrtdp, "xstsqrtdp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "test-sqrt"},
                          { &test_xvtsqrtdp, "xvtsqrtdp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "test-sqrt"},
                          { &test_xvtsqrtsp, "xvtsqrtsp", NULL, 20, SINGLE_TEST, VX_DEFAULT, "test-sqrt"},
                          { &test_xvtdivdp, "xvtdivdp", two_arg_fp_tests, 68, DOUBLE_TEST, VX_DEFAULT, "test-div"},
                          { &test_xvtdivsp, "xvtdivsp", two_arg_fp_tests, 68, SINGLE_TEST, VX_DEFAULT, "test-div"},
                          { NULL, NULL, NULL, 0 , 0, 0, NULL}
};

static unsigned long long doubleWord[] = { 0,
                                  0xffffffff00000000LL,
                                  0x00000000ffffffffLL,
                                  0xffffffffffffffffLL,
                                  0x89abcde123456789LL,
                                  0x0102030405060708LL,
                                  0x00000000a0b1c2d3LL,
                                  0x1111222233334444LL
};

static unsigned int singleWord[] = {0,
                                  0xffff0000,
                                  0x0000ffff,
                                  0xffffffff,
                                  0x89a73522,
                                  0x01020304,
                                  0x0000abcd,
                                  0x11223344
};

typedef struct vx_intToFp_test
{
   test_func_t test_func;
   const char * name;
   void * targs;
   int num_tests;
   precision_type_t precision;
   vx_fp_test_type type;
} vx_intToFp_test_t;

static vx_intToFp_test_t
intToFp_tests[] = {
                   { test_xvcvsxddp, "xvcvsxddp", (void *)doubleWord, 8, DOUBLE_TEST, VX_CONV_TO_DOUBLE },
                   { test_xvcvuxddp, "xvcvuxddp", (void *)doubleWord, 8, DOUBLE_TEST, VX_CONV_TO_DOUBLE },
                   { test_xvcvsxdsp, "xvcvsxdsp", (void *)doubleWord, 8, DOUBLE_TEST, VX_CONV_TO_SINGLE },
                   { test_xvcvuxdsp, "xvcvuxdsp", (void *)doubleWord, 8, DOUBLE_TEST, VX_CONV_TO_SINGLE },
                   { test_xvcvsxwdp, "xvcvsxwdp", (void *)singleWord, 8, SINGLE_TEST, VX_CONV_TO_DOUBLE },
                   { test_xvcvuxwdp, "xvcvuxwdp", (void *)singleWord, 8, SINGLE_TEST, VX_CONV_TO_DOUBLE },
                   { test_xvcvsxwsp, "xvcvsxwsp", (void *)singleWord, 8, SINGLE_TEST, VX_CONV_TO_SINGLE },
                   { test_xvcvuxwsp, "xvcvuxwsp", (void *)singleWord, 8, SINGLE_TEST, VX_CONV_TO_SINGLE },
                   { NULL, NULL, NULL, 0, 0 }
};

static Bool do_OE;
typedef enum {
   DIV_BASE = 1,
   DIV_OE = 2,
   DIV_DOT = 4,
} div_type_t;
/* Possible divde type combinations are:
 *   - base
 *   - base+dot
 *   - base+OE
 *   - base+OE+dot
 */
#ifdef __powerpc64__
static void test_divdeu(void)
{
   int divdeu_type = DIV_BASE;
   if (do_OE)
      divdeu_type |= DIV_OE;
   if (do_dot)
      divdeu_type |= DIV_DOT;

   switch (divdeu_type) {
      case 1:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeu %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 3:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeuo %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 5:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeu. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 7:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeuo. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      default:
         fprintf(stderr, "Invalid divdeu type. Exiting\n");
         exit(1);
   }
}
#endif

static void test_divwe(void)
{
   int divwe_type = DIV_BASE;
   if (do_OE)
      divwe_type |= DIV_OE;
   if (do_dot)
      divwe_type |= DIV_DOT;

   switch (divwe_type) {
      case 1:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divwe %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 3:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweo %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 5:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divwe. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 7:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweo. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      default:
         fprintf(stderr, "Invalid divweu type. Exiting\n");
         exit(1);
   }
}


typedef struct simple_test {
   test_func_t test_func;
   char * name;
   precision_type_t precision;
} simple_test_t;


static void setup_sp_fp_args(fp_test_args_t * targs, Bool swap_inputs)
{
   int a_idx, b_idx, i;
   void * inA, * inB;
   void * vec_src = swap_inputs ? &vec_out : &vec_inB;

   for (i = 0; i < 4; i++) {
      a_idx = targs->fra_idx;
      b_idx = targs->frb_idx;
      inA = (void *)&spec_sp_fargs[a_idx];
      inB = (void *)&spec_sp_fargs[b_idx];
      // copy single precision FP  into vector element i
      memcpy(((void *)&vec_inA) + (i * 4), inA, 4);
      memcpy(vec_src + (i * 4), inB, 4);
      targs++;
   }
}

static void setup_dp_fp_args(fp_test_args_t * targs, Bool swap_inputs)
{
   int a_idx, b_idx, i;
   void * inA, * inB;
   void * vec_src = swap_inputs ? (void *)&vec_out : (void *)&vec_inB;

   for (i = 0; i < 2; i++) {
      a_idx = targs->fra_idx;
      b_idx = targs->frb_idx;
      inA = (void *)&spec_fargs[a_idx];
      inB = (void *)&spec_fargs[b_idx];
      // copy double precision FP  into vector element i
      memcpy(((void *)&vec_inA) + (i * 8), inA, 8);
      memcpy(vec_src + (i * 8), inB, 8);
      targs++;
   }
}

#define VX_NOT_CMP_OP 0xffffffff
static void print_vector_fp_result(unsigned int cc, vx_fp_test_t * test_group, int i, Bool print_vec_out)
{
   int a_idx, b_idx, k;
   char * name = malloc(20);
   int dp = test_group->precision == DOUBLE_TEST ? 1 : 0;
   int loops = dp ? 2 : 4;
   fp_test_args_t * targs = &test_group->targs[i];
   unsigned long long * frA_dp, * frB_dp, * dst_dp;
   unsigned int * frA_sp, *frB_sp, * dst_sp;
   strcpy(name, test_group->name);
   printf("#%d: %s%s ", dp? i/2 : i/4, name, (do_dot ? "." : ""));
   for (k = 0; k < loops; k++) {
      a_idx = targs->fra_idx;
      b_idx = targs->frb_idx;
      if (k)
         printf(" AND ");
      if (dp) {
         frA_dp = (unsigned long long *)&spec_fargs[a_idx];
         frB_dp = (unsigned long long *)&spec_fargs[b_idx];
         printf("%016llx %s %016llx", *frA_dp, test_group->op, *frB_dp);
      } else {
         frA_sp = (unsigned int *)&spec_sp_fargs[a_idx];
         frB_sp = (unsigned int *)&spec_sp_fargs[b_idx];
         printf("%08x %s %08x", *frA_sp, test_group->op, *frB_sp);
      }
      targs++;
   }
   if (cc != VX_NOT_CMP_OP)
      printf(" ? cc=%x", cc);

   if (print_vec_out) {
      if (dp) {
         dst_dp = (unsigned long long *) &vec_out;
         printf(" => %016llx %016llx\n", dst_dp[0], dst_dp[1]);
      } else {
         dst_sp = (unsigned int *) &vec_out;
         printf(" => %08x %08x %08x %08x\n", dst_sp[0], dst_sp[1], dst_sp[2], dst_sp[3]);
      }
   } else {
      printf("\n");
   }
   free(name);
}



static void test_vsx_one_fp_arg(void)
{
   test_func_t func;
   int k;
   k = 0;
   build_special_fargs_table();

   while ((func = vsx_one_fp_arg_tests[k].test_func)) {
      int idx, i;
      vx_fp_test_t test_group = vsx_one_fp_arg_tests[k];
      Bool estimate = (test_group.type == VX_ESTIMATE);
      Bool dp = (test_group.precision == DOUBLE_TEST) ? True : False;
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
      if (!dp && !is_scalar && test_group.type == VX_CONV_TO_DOUBLE) {
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
               if (estimate) {
                  Bool res = check_estimate(DOUBLE_TEST, is_sqrt, i + j, j);
                  printf(" ==> %s)", res ? "PASS" : "FAIL");
                  /* For debugging . . .
                   printf(" ==> %s (res=%016llx)", res ? "PASS" : "FAIL", dst_dp[j]);
                   */
               } else {
                  vx_fp_test_type type = test_group.type;
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
            if (test_group.type == VX_CONV_TO_DOUBLE)
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
               if (estimate) {
                  Bool res = check_estimate(SINGLE_TEST, is_sqrt, i + j, j);
                  printf(" ==> %s)", res ? "PASS" : "FAIL");
               } else {
                  if (test_group.type == VX_CONV_TO_DOUBLE)
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
            }
            printf("\n");
         }
      }
      k++;
      printf( "\n" );
   }
}

static void test_int_to_fp_convert(void)
{
   test_func_t func;
   int k;
   k = 0;

   while ((func = intToFp_tests[k].test_func)) {
      int idx, i;
      vx_intToFp_test_t test_group = intToFp_tests[k];
      Bool dp = (test_group.precision == DOUBLE_TEST) ? True : False;
      Bool sparse_sp = False;
      int stride = dp ? 2 : 4;
      int loops = stride;

      /* For conversions of single to double, the 128-bit input register is sparsely populated:
       *    |___ int___|_Unused_|___int___|__Unused__|   // for vector op
       *                     or
       * We need to adjust stride from '4' to '2', since we'll only be loading
       * two values per loop into the input register.
       */
      if (!dp && test_group.type == VX_CONV_TO_DOUBLE) {
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
            unsigned long long  *dst_dw, * targs = test_group.targs;
            for (j = 0; j < loops; j++) {
               inB = (void *)&targs[i + j];
               // copy doubleword into vector element i
               memcpy(((void *)&vec_inB) + (j * 8), inB, 8);
            }
            // execute test insn
            (*func)();
            dst_dw = (unsigned long long *) &vec_out;
            printf("#%d: %s ", i/stride, test_group.name);
            for (j = 0; j < loops; j++) {
               if (j)
                  printf("; ");
               printf("conv(%016llx)", targs[i + j]);

               if (test_group.type == VX_CONV_TO_SINGLE)
                  printf(" = %016llx", dst_dw[j] & 0xffffffff00000000ULL);
               else
                  printf(" = %016llx", dst_dw[j]);
            }
            printf("\n");
         } else {
            int j, skip_slot;
            unsigned int * dst_sp = NULL;
            unsigned int * targs = test_group.targs;
            unsigned long long * dst_dp = NULL;
            if (sparse_sp) {
               skip_slot = 1;
               loops = 2;
            } else {
               skip_slot = 0;
            }
            for (j = 0; j < loops; j++) {
               inB = (void *)&targs[i + j];
               // copy single word into vector element i
               if (skip_slot && j > 0)
                  memcpy(((void *)&vec_inB) + ((j + j) * 4), inB, 4);
               else
                  memcpy(((void *)&vec_inB) + (j * 4), inB, 4);
            }
            // execute test insn
            (*func)();
            if (test_group.type == VX_CONV_TO_DOUBLE)
               dst_dp = (unsigned long long *) &vec_out;
            else
               dst_sp = (unsigned int *) &vec_out;
            // print result
            printf("#%d: %s ", i/stride, test_group.name);
            for (j = 0; j < loops; j++) {
               if (j)
                  printf("; ");
               printf("conv(%08x)", targs[i + j]);
               if (test_group.type == VX_CONV_TO_DOUBLE)
                  printf(" = %016llx", dst_dp[j]);
               else
                  printf(" = %08x", dst_sp[j]);
            }
            printf("\n");
         }
      }
      k++;
      printf( "\n" );
   }
}



// The div doubleword test data
signed long long div_dw_tdata[13][2] = {
                                       { 4, -4 },
                                       { 4, -3 },
                                       { 4, 4 },
                                       { 4, -5 },
                                       { 3, 8 },
                                       { 0x8000000000000000ULL, 0xa },
                                       { 0x50c, -1 },
                                       { 0x50c, -4096 },
                                       { 0x1234fedc, 0x8000a873 },
                                       { 0xabcd87651234fedcULL, 0xa123b893 },
                                       { 0x123456789abdcULL, 0 },
                                       { 0, 2 },
                                       { 0x77, 0xa3499 }
};
#define dw_tdata_len (sizeof(div_dw_tdata)/sizeof(signed long long)/2)

// The div word test data
unsigned int div_w_tdata[6][2] = {
                              { 0, 2 },
                              { 2, 0 },
                              { 0x7abc1234, 0xf0000000 },
                              { 0xfabc1234, 5 },
                              { 77, 66 },
                              { 5, 0xfabc1234 },
};
#define w_tdata_len (sizeof(div_w_tdata)/sizeof(unsigned int)/2)

typedef struct div_ext_test
{
   test_func_t test_func;
   const char *name;
   int num_tests;
   div_type_t div_type;
   precision_type_t precision;
} div_ext_test_t;

static div_ext_test_t div_tests[] = {
#ifdef __powerpc64__
                                   { &test_divdeu, "divdeu", dw_tdata_len, DIV_BASE, DOUBLE_TEST },
                                   { &test_divdeu, "divdeuo", dw_tdata_len, DIV_OE, DOUBLE_TEST },
#endif
                                   { &test_divwe, "divwe", w_tdata_len, DIV_BASE, SINGLE_TEST },
                                   { &test_divwe, "divweo", w_tdata_len, DIV_OE, SINGLE_TEST },
                                   { NULL, NULL, 0, 0, 0 }
};

static void test_div_extensions(void)
{
   test_func_t func;
   int k;
   k = 0;

   while ((func = div_tests[k].test_func)) {
      int i, repeat = 1;
      div_ext_test_t test_group = div_tests[k];
      do_dot = False;

again:
      for (i = 0; i < test_group.num_tests; i++) {
         unsigned int condreg;

         if (test_group.div_type == DIV_OE)
            do_OE = True;
         else
            do_OE = False;

         if (test_group.precision == DOUBLE_TEST) {
            r14 = div_dw_tdata[i][0];
            r15 = div_dw_tdata[i][1];
         } else {
            r14 = div_w_tdata[i][0];
            r15 = div_w_tdata[i][1];
         }
         // execute test insn
         (*func)();
         condreg = (div_flags & 0xf0000000) >> 28;
         printf("#%d: %s%s: ", i, test_group.name, do_dot ? "." : "");
         if (test_group.precision == DOUBLE_TEST) {
            printf("0x%016llx0000000000000000 / 0x%016llx = 0x%016llx;",
                   div_dw_tdata[i][0], div_dw_tdata[i][1], (signed long long) r17);
         } else {
            printf("0x%08x00000000 / 0x%08x = 0x%08x;",
                   div_w_tdata[i][0], div_w_tdata[i][1], (unsigned int) r17);
         }
         printf(" CR=%x; XER=%x\n", condreg, div_xer);
      }
      printf("\n");
      if (repeat) {
         repeat = 0;
         do_dot = True;
         goto again;
      }
      k++;
      printf( "\n" );
   }
}


static void test_vx_tdivORtsqrt(void)
{
   test_func_t func;
   int k, crx;
   unsigned int flags;
   k = 0;
   do_dot = False;
   build_special_fargs_table();

   while ((func = vx_tdivORtsqrt_tests[k].test_func)) {
      int idx, i;
      vx_fp_test_t test_group = vx_tdivORtsqrt_tests[k];
      Bool dp = (test_group.precision == DOUBLE_TEST) ? True : False;
      Bool is_scalar = (strstr(test_group.name, "xs")) ? True : False;
      Bool two_args = test_group.targs ?  True : False;
      int stride = dp ? 2 : 4;
      int loops = is_scalar ? 1 : stride;
      stride = is_scalar ? 1: stride;

      for (i = 0; i < test_group.num_tests; i+=stride) {
         unsigned int * pv;
         void * inB;

         pv = (unsigned int *)&vec_out;
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         if (dp) {
            int j;
            unsigned long long * frB_dp;
            if (two_args) {
               setup_dp_fp_args(&test_group.targs[i], False);
            } else {
               for (j = 0; j < loops; j++) {
                  inB = (void *)&spec_fargs[i + j];
                  // copy double precision FP into vector element i
                  memcpy(((void *)&vec_inB) + (j * 8), inB, 8);
               }
            }
            // execute test insn
            // Must do set/get of CRs immediately before/after calling the asm func
            // to avoid CRs being modified by other instructions.
            SET_FPSCR_ZERO;
            SET_CR_XER_ZERO;
            (*func)();
            GET_CR(flags);
            // assumes using CR1
            crx = (flags & 0x0f000000) >> 24;
            if (two_args) {
               print_vector_fp_result(crx, &test_group, i, False/*do not print vec_out*/);
            } else {
               printf("#%d: %s ", i/stride, test_group.name);
               for (j = 0; j < loops; j++) {
                  if (j)
                     printf("; ");
                  frB_dp = (unsigned long long *)&spec_fargs[i + j];
                  printf("%s(%016llx)", test_group.op, *frB_dp);
               }
               printf( " ? %x (CRx)\n", crx);
            }
         } else {
            int j;
            unsigned int * frB_sp;
            if (two_args) {
               setup_sp_fp_args(&test_group.targs[i], False);
            } else {
               for (j = 0; j < loops; j++) {
                  inB = (void *)&spec_sp_fargs[i + j];
                  // copy single precision FP into vector element i
                  memcpy(((void *)&vec_inB) + (j * 4), inB, 4);
               }
            }
            // execute test insn
            SET_FPSCR_ZERO;
            SET_CR_XER_ZERO;
            (*func)();
            GET_CR(flags);
            crx = (flags & 0x0f000000) >> 24;
            // print result
            if (two_args) {
               print_vector_fp_result(crx, &test_group, i, False/*do not print vec_out*/);
            } else {
               printf("#%d: %s ", i/stride, test_group.name);
               for (j = 0; j < loops; j++) {
                  if (j)
                     printf("; ");
                  frB_sp = (unsigned int *)&spec_sp_fargs[i + j];
                  printf("%s(%08x)", test_group.op, *frB_sp);
               }
               printf( " ? %x (CRx)\n", crx);
            }
         }
      }
      k++;
      printf( "\n" );
   }
}


static void test_ftsqrt(void)
{
   int i, crx;
   unsigned int flags;
   unsigned long long * frbp;
   build_special_fargs_table();


   for (i = 0; i < nb_special_fargs; i++) {
      f14 = spec_fargs[i];
      frbp = (unsigned long long *)&spec_fargs[i];
      SET_FPSCR_ZERO;
      SET_CR_XER_ZERO;
      __asm__ __volatile__ ("ftsqrt           cr1, %0" : : "d" (f14));
      GET_CR(flags);
      crx = (flags & 0x0f000000) >> 24;
      printf( "ftsqrt: %016llx ? %x (CRx)\n", *frbp, crx);
   }
   printf( "\n" );
}

static void
test_popcntw(void)
{
#ifdef __powerpc64__
   uint64_t res;
   unsigned long long src = 0x9182736405504536ULL;
   r14 = src;
   __asm__ __volatile__ ("popcntw          %0, %1" : "=r" (res): "r" (r14));
   printf("popcntw: 0x%llx => 0x%016llx\n", (unsigned long long)src, (unsigned long long)res);
#else
   uint32_t res;
   unsigned int src = 0x9182730E;
   r14 = src;
   __asm__ __volatile__ ("popcntw          %0, %1" : "=r" (res): "r" (r14));
   printf("popcntw: 0x%x => 0x%08x\n", src, (int)res);
#endif
   printf( "\n" );
}


static test_table_t
         all_tests[] =
{

                    { &test_vsx_one_fp_arg,
                      "Test VSX vector and scalar single argument instructions"} ,
                    { &test_int_to_fp_convert,
                      "Test VSX vector integer to float conversion instructions" },
                    { &test_div_extensions,
                       "Test div extensions" },
                    { &test_ftsqrt,
                       "Test ftsqrt instruction" },
                    { &test_vx_tdivORtsqrt,
                       "Test vector and scalar tdiv and tsqrt instructions" },
                    { &test_popcntw,
                       "Test popcntw instruction" },
                    { NULL, NULL }
};
#endif // HAS_VSX

int main(int argc, char *argv[])
{
#ifdef HAS_VSX

   test_table_t aTest;
   test_func_t func;
   int i = 0;

   while ((func = all_tests[i].test_category)) {
      aTest = all_tests[i];
      printf( "%s\n", aTest.name );
      (*func)();
      i++;
   }
   if (spec_fargs)
     free(spec_fargs);
   if (spec_sp_fargs)
     free(spec_sp_fargs);

#endif // HAS _VSX

   return 0;
}
