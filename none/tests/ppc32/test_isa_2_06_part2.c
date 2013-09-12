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
} fp_test_args_t;


fp_test_args_t fp_cmp_tests[] = {
                                   {8, 8},
                                   {8, 14},
                                   {8, 6},
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
                                   {10, 6},
                                   {10, 5},
                                   {10, 4},
                                   {10, 7},
                                   {10, 9},
                                   {10, 10},
                                   {12, 8},
                                   {12, 14},
                                   {12, 6},
                                   {12, 5},
                                   {1, 1},
                                   {2, 2},
                                   {3, 3},
                                   {4, 4},
};


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
                                     {11, 12}
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
   15     0   412   0x32585a9900000ULL         A couple more positive finite numbers
   16     0   413   0x82511a2000000ULL         ...
*/

   uint64_t mant;
   uint32_t mant_sp;
   uint16_t _exp;
   int s;
   int j, i = 0;

   if (spec_fargs)
      return;

   spec_fargs = malloc( 17 * sizeof(double) );
   spec_sp_fargs = malloc( 17 * sizeof(float) );

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

   /* None of the ftdiv tests succeed.
    * FRA = value #0; FRB = value #1
    * ea_ = -2; e_b = 5
    * fl_flag || fg_flag || fe_flag = 100
    */

   /*************************************************
    *     fe_flag tests
    *
    *************************************************/

   /* fe_flag <- 1 if FRA is a NaN
    * FRA = value #9; FRB = value #1
    * e_a = 1024; e_b = 5
    * fl_flag || fg_flag || fe_flag = 101
    */

   /* fe_flag <- 1 if FRB is a NaN
    * FRA = value #1; FRB = value #12
    * e_a = 5; e_b = 1024
    * fl_flag || fg_flag || fe_flag = 101
    */

   /* fe_flag <- 1 if e_b <= -1022
    * FRA = value #0; FRB = value #2
    * e_a = -2; e_b = -1022
    * fl_flag || fg_flag || fe_flag = 101
    *
    */
   // #2
   s = 0;
   _exp = 0x001;
   mant = 0x8000000b77501ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* fe_flag <- 1 if e_b >= 1021
    * FRA = value #1; FRB = value #3
    * e_a = 5; e_b = 1023
    * fl_flag || fg_flag || fe_flag = 101
    */
   // #3
   s = 0;
   _exp = 0x7fe;
   mant = 0x800000000051bULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /* fe_flag <- 1 if FRA != 0 && e_a - e_b >= 1023
    * Let FRA = value #3 and FRB be value #0.
    * e_a = 1023; e_b = -2
    * fl_flag || fg_flag || fe_flag = 101
    */

   /* fe_flag <- 1 if FRA != 0 && e_a - e_b <= -1023
    * Let FRA = value #0 above and FRB be value #3 above
    * e_a = -2; e_b = 1023
    * fl_flag || fg_flag || fe_flag = 101
    */

   /* fe_flag <- 1 if FRA != 0 && e_a <= -970
    * Let FRA = value #4 and FRB be value #0
    * e_a = -1005; e_b = -2
    * fl_flag || fg_flag || fe_flag = 101
   */
   // #4
   s = 0;
   _exp = 0x012;
   mant = 0x3214569900000ULL;
   register_farg(&spec_fargs[i++], s, _exp, mant);

   /*************************************************
    *     fg_flag tests
    *
    *************************************************/
   /* fg_flag <- 1 if FRA is an Infinity
    * NOTE: FRA = Inf also sets fe_flag
    * Do two tests, using values #7 and #8 (+/- Inf) for FRA.
    * Test 1:
    *   Let FRA be value #7 and FRB be value #1
    *   e_a = 1024; e_b = 5
    *   fl_flag || fg_flag || fe_flag = 111
    *
    * Test 2:
    *   Let FRA be value #8 and FRB be value #1
    *   e_a = 1024; e_b = 5
    *   fl_flag || fg_flag || fe_flag = 111
    *
    */

   /* fg_flag <- 1 if FRB is an Infinity
    * NOTE: FRB = Inf also sets fe_flag
    * Let FRA be value #1 and FRB be value #7
    * e_a = 5; e_b = 1024
    * fl_flag || fg_flag || fe_flag = 111
    */

   /* fg_flag <- 1 if FRB is denormalized
    * NOTE: e_b < -1022 ==> fe_flag <- 1
    * Let FRA be value #0 and FRB be value #13
    * e_a = -2; e_b = -1023
    * fl_flag || fg_flag || fe_flag = 111
    */

   /* fg_flag <- 1 if FRB is +zero
    * NOTE: FRA = Inf also sets fe_flag
    * Let FRA = val #5; FRB = val #5
    * ea_ = -1023; e_b = -1023
    * fl_flag || fg_flag || fe_flag = 111
    */

   /* fg_flag <- 1 if FRB is -zero
    * NOTE: FRA = Inf also sets fe_flag
    * Let FRA = val #5; FRB = val #6
    * ea_ = -1023; e_b = -1023
    * fl_flag || fg_flag || fe_flag = 111
    */

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

   /* A couple positive finite numbers ... */
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
   DOUBLE_TEST
} precision_type_t;

typedef enum {
   VX_SCALAR_FP_NMSUB = 0,
   // ALL VECTOR-TYPE OPS SHOULD BE ADDED AFTER THIS LINE
   VX_VECTOR_FP_MULT_AND_OP2 = 10,
   // and before this line
   VX_BASIC_CMP = 30,
   VX_CONV_WORD,
   VX_DEFAULT
} vx_fp_test_type;

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

static vector unsigned int vec_out, vec_inA, vec_inB, vec_inC;

static Bool do_dot;
static void test_xvcmpeqdp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpeqdp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpeqdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcmpgedp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpgedp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpgedp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcmpgtdp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpgtdp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpgtdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcmpeqsp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpeqsp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpeqsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcmpgesp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpgesp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpgesp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcmpgtsp(void)
{
   if (do_dot)
      __asm__ __volatile__ ("xvcmpgtsp.          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xvcmpgtsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static Bool do_aXp;
static Bool do_dp;
static void test_xsnmsub(void)
{
   if (do_aXp)
      __asm__ __volatile__ ("xsnmsubadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsnmsubmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmadd(void)
{
   if (do_aXp)
      if (do_dp)
         __asm__ __volatile__ ("xvmaddadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvmaddasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      if (do_dp)
         __asm__ __volatile__ ("xvmaddmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvmaddmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvnmadd(void)
{
   if (do_aXp)
      if (do_dp)
         __asm__ __volatile__ ("xvnmaddadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvnmaddasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      if (do_dp)
         __asm__ __volatile__ ("xvnmaddmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvnmaddmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvnmsub(void)
{
   if (do_aXp)
      if (do_dp)
         __asm__ __volatile__ ("xvnmsubadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvnmsubasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      if (do_dp)
         __asm__ __volatile__ ("xvnmsubmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvnmsubmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmsub(void)
{
   if (do_aXp)
      if (do_dp)
         __asm__ __volatile__ ("xvmsubadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvmsubasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      if (do_dp)
         __asm__ __volatile__ ("xvmsubmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
      else
         __asm__ __volatile__ ("xvmsubmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xssqrtdp(void)
{
   __asm__ __volatile__ ("xssqrtdp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrdpim(void)
{
   __asm__ __volatile__ ("xsrdpim   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrdpip(void)
{
   __asm__ __volatile__ ("xsrdpip   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xstdivdp(void)
{
   __asm__ __volatile__ ("xstdivdp   6, %x0, %x1" : : "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xsmaxdp(void)
{
   __asm__ __volatile__ ("xsmaxdp   %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsmindp(void)
{
   __asm__ __volatile__ ("xsmindp   %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvadddp(void)
{
   __asm__ __volatile__ ("xvadddp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvaddsp(void)
{
   __asm__ __volatile__ ("xvaddsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvdivdp(void)
{
   __asm__ __volatile__ ("xvdivdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvdivsp(void)
{
   __asm__ __volatile__ ("xvdivsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmuldp(void)
{
   __asm__ __volatile__ ("xvmuldp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmulsp(void)
{
   __asm__ __volatile__ ("xvmulsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvsubdp(void)
{
   __asm__ __volatile__ ("xvsubdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmaxdp(void)
{
   __asm__ __volatile__ ("xvmaxdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmindp(void)
{
   __asm__ __volatile__ ("xvmindp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvmaxsp(void)
{
   __asm__ __volatile__ ("xvmaxsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvminsp(void)
{
   __asm__ __volatile__ ("xvminsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvsubsp(void)
{
   __asm__ __volatile__ ("xvsubsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvresp(void)
{
   __asm__ __volatile__ ("xvresp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xxsel(void)
{
   unsigned long long * dst;
   unsigned long long xa[] =  { 0xa12bc37de56f9708ULL, 0x3894c1fddeadbeefULL};
   unsigned long long xb[] =  { 0xfedc432124681235ULL, 0xf1e2d3c4e0057708ULL};
   unsigned long long xc[] =  { 0xffffffff01020304ULL, 0x128934bd00000000ULL};

   memcpy(&vec_inA, xa, 16);
   memcpy(&vec_inB, xb, 16);
   memcpy(&vec_inC, xc, 16);


   __asm__ __volatile__ ("xxsel   %x0, %x1, %x2, %x3" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB),"wa" (vec_inC));
   dst = (unsigned long long *) &vec_out;
   printf("xxsel %016llx,%016llx,%016llx => %016llx\n", xa[0], xb[0], xc[0], *dst);
   dst++;
   printf("xxsel %016llx,%016llx,%016llx => %016llx\n", xa[1], xb[1], xc[1], *dst);
   printf("\n");
}

static void test_xxspltw(void)
{
   int uim;
   unsigned long long * dst = NULL;
   unsigned long long xb[] =  { 0xfedc432124681235ULL, 0xf1e2d3c4e0057708ULL};
   memcpy(&vec_inB, xb, 16);

   for (uim = 0; uim < 4; uim++) {
      switch (uim) {
         case 0:
            __asm__ __volatile__ ("xxspltw   %x0, %x1, 0" : "=wa" (vec_out): "wa" (vec_inB));
            break;
         case 1:
            __asm__ __volatile__ ("xxspltw   %x0, %x1, 1" : "=wa" (vec_out): "wa" (vec_inB));
            break;
         case 2:
            __asm__ __volatile__ ("xxspltw   %x0, %x1, 2" : "=wa" (vec_out): "wa" (vec_inB));
            break;
         case 3:
            __asm__ __volatile__ ("xxspltw   %x0, %x1, 3" : "=wa" (vec_out): "wa" (vec_inB));
            break;
      }
      dst = (unsigned long long *) &vec_out;
      printf("xxspltw 0x%016llx%016llx %d=> 0x%016llx",  xb[0], xb[1], uim, *dst);
      dst++;
      printf("%016llx\n", *dst);
   }
   printf("\n");
}

static void test_xscvdpsxws(void)
{
   __asm__ __volatile__ ("xscvdpsxws  %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvdpuxds(void)
{
   __asm__ __volatile__ ("xscvdpuxds  %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcpsgndp(void)
{
   __asm__ __volatile__  ("xvcpsgndp  %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcpsgnsp(void)
{
   __asm__ __volatile__  ("xvcpsgnsp  %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xvcvdpsxws(void)
{
   __asm__ __volatile__ ("xvcvdpsxws  %x0, %x1 " : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xvcvspsxws(void)
{
   __asm__ __volatile__ ("xvcvspsxws  %x0, %x1 " : "=wa" (vec_out): "wa" (vec_inB));
}

static vx_fp_test_t
vx_vector_one_fp_arg_tests[] = {
                                { &test_xvresp, "xvresp", NULL, 16, SINGLE_TEST, VX_BASIC_CMP, "1/x"},
                                { &test_xvcvdpsxws, "xvcvdpsxws", NULL, 16, DOUBLE_TEST, VX_CONV_WORD, "conv"},
                                { &test_xvcvspsxws, "xvcvspsxws", NULL, 16, SINGLE_TEST, VX_CONV_WORD, "conv"},
                                { NULL, NULL, NULL, 0 , 0, 0, NULL}
};

static vx_fp_test_t
vx_vector_fp_tests[] = {
                        { &test_xvcmpeqdp, "xvcmpeqdp", fp_cmp_tests, 64, DOUBLE_TEST, VX_BASIC_CMP, "eq"},
                        { &test_xvcmpgedp, "xvcmpgedp", fp_cmp_tests, 64, DOUBLE_TEST, VX_BASIC_CMP, "ge"},
                        { &test_xvcmpgtdp, "xvcmpgtdp", fp_cmp_tests, 64, DOUBLE_TEST, VX_BASIC_CMP, "gt"},
                        { &test_xvcmpeqsp, "xvcmpeqsp", fp_cmp_tests, 64, SINGLE_TEST, VX_BASIC_CMP, "eq"},
                        { &test_xvcmpgesp, "xvcmpgesp", fp_cmp_tests, 64, SINGLE_TEST, VX_BASIC_CMP, "ge"},
                        { &test_xvcmpgtsp, "xvcmpgtsp", fp_cmp_tests, 64, SINGLE_TEST, VX_BASIC_CMP, "gt"},
                        { &test_xvadddp, "xvadddp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "+" },
                        { &test_xvaddsp, "xvaddsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "+" },
                        { &test_xvdivdp, "xvdivdp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "/" },
                        { &test_xvdivsp, "xvdivsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "/" },
                        { &test_xvmuldp, "xvmuldp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "*" },
                        { &test_xvmulsp, "xvmulsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "*" },
                        { &test_xvsubdp, "xvsubdp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "-" },
                        { &test_xvsubsp, "xvsubsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "-" },
                        { &test_xvmaxdp, "xvmaxdp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "@max@" },
                        { &test_xvmindp, "xvmindp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "@min@" },
                        { &test_xvmaxsp, "xvmaxsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "@max@" },
                        { &test_xvminsp, "xvminsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "@min@" },
                        { &test_xvcpsgndp, "xvcpsgndp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, "+-cp"},
                        { &test_xvcpsgnsp, "xvcpsgnsp", two_arg_fp_tests, 64, SINGLE_TEST, VX_DEFAULT, "+-cp"},
                        { NULL, NULL, NULL, 0 , 0, 0, NULL}
};


static vx_fp_test_t
vx_aORm_fp_tests[] = {
                       { &test_xsnmsub, "xsnmsub", two_arg_fp_tests, 64, DOUBLE_TEST, VX_SCALAR_FP_NMSUB, "!*-"},
                       { &test_xvmadd, "xvmadd", two_arg_fp_tests, 64, DOUBLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "*+"},
                       { &test_xvmadd, "xvmadd", two_arg_fp_tests, 64, SINGLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "*+"},
                       { &test_xvnmadd, "xvnmadd", two_arg_fp_tests, 64, DOUBLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "!*+"},
                       { &test_xvnmadd, "xvnmadd", two_arg_fp_tests, 64, SINGLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "!*+"},
                       { &test_xvmsub, "xvmsub", two_arg_fp_tests, 64, DOUBLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "*-"},
                       { &test_xvmsub, "xvmsub", two_arg_fp_tests, 64, SINGLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "*-"},
                       { &test_xvnmsub, "xvnmsub", two_arg_fp_tests, 64, DOUBLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "!*-"},
                       { &test_xvnmsub, "xvnmsub", two_arg_fp_tests, 64, SINGLE_TEST, VX_VECTOR_FP_MULT_AND_OP2, "!*-"},
                       { NULL, NULL, NULL, 0, 0, 0,  NULL }
};

static vx_fp_test_t
vx_simple_scalar_fp_tests[] = {
                               { &test_xssqrtdp, "xssqrtdp", NULL, 17, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xsrdpim, "xsrdpim", NULL, 17, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xsrdpip, "xsrdpip", NULL, 17, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xstdivdp, "xstdivdp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xsmaxdp, "xsmaxdp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xsmindp, "xsmindp", two_arg_fp_tests, 64, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { &test_xscvdpsxws, "xscvdpsxws", NULL, 17, DOUBLE_TEST, VX_CONV_WORD, NULL},
                               { &test_xscvdpuxds, "xscvdpuxds", NULL, 17, DOUBLE_TEST, VX_DEFAULT, NULL},
                               { NULL, NULL, NULL, 0, 0, 0, NULL }
};


#ifdef __powerpc64__
static void test_bpermd(void)
{
   /* NOTE: Bit number is '0 . . . 63'
    *
    * Permuted bits are generated bit 0 -7 as follows:
    *    index = (r14)8*i:8*i+7
    *    perm[i] = (r15)index
    *
    * So, for i = 0, index is (r14)8*0:8*0+7, or (r14)0:7, which is the MSB
    * byte of r14, 0x1b(27/base 10).  This identifies bit 27 of r15, which is '1'.
    * For i = 1, index is 0x2c, identifying bit 44 of r15, which is '1'.
    * So the result of the first two iterations of i are:
    *   perm = 0b01xxxxxx
    *
    */
   r15 = 0xa12bc37de56f9708ULL;
   r14 = 0x1b2c31f030000001ULL;
   __asm__ __volatile__ ("bpermd %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
   printf("bpermd: 0x%016llx : 0x%016llx => 0x%llx\n", (unsigned long long)r14,
          (unsigned long long)r15, (unsigned long long)r17);
   printf("\n");
}
#endif

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
static void test_divde(void)
{
   int divde_type = DIV_BASE;
   if (do_OE)
      divde_type |= DIV_OE;
   if (do_dot)
      divde_type |= DIV_DOT;

   switch (divde_type) {
      case 1:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divde %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 3:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeo %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 5:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divde. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 7:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divdeo. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      default:
         fprintf(stderr, "Invalid divde type. Exiting\n");
         exit(1);
   }
}
#endif

static void test_divweu(void)
{
   int divweu_type = DIV_BASE;
   if (do_OE)
      divweu_type |= DIV_OE;
   if (do_dot)
      divweu_type |= DIV_DOT;

   switch (divweu_type) {
      case 1:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweu %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 3:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweuo %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 5:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweu. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      case 7:
        SET_CR_XER_ZERO;
         __asm__ __volatile__ ("divweuo. %0, %1, %2" : "=r" (r17) : "r" (r14),"r" (r15));
         GET_CR_XER(div_flags, div_xer);
         break;
      default:
         fprintf(stderr, "Invalid divweu type. Exiting\n");
         exit(1);
   }
}

static void test_fctiduz(void)
{
   if (do_dot)
      __asm__ __volatile__ ("fctiduz. %0, %1" : "=d" (f17) : "d" (f14));
   else
      __asm__ __volatile__ ("fctiduz %0, %1" : "=d" (f17) : "d" (f14));
}

static void test_fctidu(void)
{
   if (do_dot)
      __asm__ __volatile__ ("fctidu. %0, %1" : "=d" (f17) : "d" (f14));
   else
      __asm__ __volatile__ ("fctidu %0, %1" : "=d" (f17) : "d" (f14));
}

static void test_fctiwuz(void)
{
   if (do_dot)
      __asm__ __volatile__ ("fctiwuz. %0, %1" : "=d" (f17) : "d" (f14));
   else
      __asm__ __volatile__ ("fctiwuz %0, %1" : "=d" (f17) : "d" (f14));
}

static void test_fctiwu(void)
{
   if (do_dot)
      __asm__ __volatile__ ("fctiwu. %0, %1" : "=d" (f17) : "d" (f14));
   else
      __asm__ __volatile__ ("fctiwu %0, %1" : "=d" (f17) : "d" (f14));
}

typedef struct simple_test {
   test_func_t test_func;
   char * name;
   precision_type_t precision;
} simple_test_t;

static simple_test_t fct_tests[] = {
                                    { &test_fctiduz, "fctiduz", DOUBLE_TEST },
                                    { &test_fctidu, "fctidu", DOUBLE_TEST },
                                    { &test_fctiwuz, "fctiwuz", SINGLE_TEST },
                                    { &test_fctiwu, "fctiwu", SINGLE_TEST },
                                   { NULL, NULL }
};

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
static void print_vector_fp_result(unsigned int cc, vx_fp_test_t * test_group, int i)
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

   if (dp) {
      dst_dp = (unsigned long long *) &vec_out;
      printf(" => %016llx %016llx\n", dst_dp[0], dst_dp[1]);
   } else {
      dst_sp = (unsigned int *) &vec_out;
      printf(" => %08x %08x %08x %08x\n", dst_sp[0], dst_sp[1], dst_sp[2], dst_sp[3]);
   }
   free(name);
}


static void print_vx_aORm_fp_result(unsigned long long * XT_arg, unsigned long long * XB_arg,
                                    vx_fp_test_t * test_group, int i)
{
   int a_idx, k;
   char * name = malloc(20);
   int dp = test_group->precision == DOUBLE_TEST ? 1 : 0;
   int loops = dp ? 2 : 4;
   fp_test_args_t * targs = &test_group->targs[i];
   unsigned long long frA_dp, * dst_dp;
   unsigned int frA_sp, * dst_sp;

   strcpy(name, test_group->name);
   if (do_aXp)
      if (dp)
         strcat(name, "adp");
      else
         strcat(name, "asp");
   else
      if (dp)
         strcat(name, "mdp");
      else
         strcat(name, "msp");

   printf("#%d: %s ", dp? i/2 : i/4, name);
   for (k = 0; k < loops; k++) {
      a_idx = targs->fra_idx;
      if (k)
         printf(" AND ");
      if (dp) {
         frA_dp = *((unsigned long long *)&spec_fargs[a_idx]);
         printf("%s(%016llx,%016llx,%016llx)", test_group->op, XT_arg[k], frA_dp, XB_arg[k]);
      } else {
         unsigned int * xt_sp = (unsigned int *)XT_arg;
         unsigned int * xb_sp = (unsigned int *)XB_arg;
         frA_sp = *((unsigned int *)&spec_sp_fargs[a_idx]);
         printf("%s(%08x,%08x,%08x)", test_group->op, xt_sp[k], frA_sp, xb_sp[k]);
      }
      targs++;
   }

   if (dp) {
      dst_dp = (unsigned long long *) &vec_out;
      printf(" => %016llx %016llx\n", dst_dp[0], dst_dp[1]);
   } else {
      dst_sp = (unsigned int *) &vec_out;
      printf(" => %08x %08x %08x %08x\n", dst_sp[0], dst_sp[1], dst_sp[2], dst_sp[3]);
   }
   free(name);
}

/* This function currently only supports double precision input arguments. */
static void test_vx_simple_scalar_fp_ops(void)
{
   test_func_t func;
   int k = 0;

   build_special_fargs_table();
   while ((func = vx_simple_scalar_fp_tests[k].test_func)) {
      unsigned long long * frap, * frbp, * dst;
      unsigned int * pv;
      int idx;
      vx_fp_test_t test_group = vx_simple_scalar_fp_tests[k];
      Bool convToWord = (test_group.type == VX_CONV_WORD);
      if (test_group.precision != DOUBLE_TEST) {
         fprintf(stderr, "Unsupported single precision for scalar op in test_vx_aORm_fp_ops\n");
         exit(1);
      }
      pv = (unsigned int *)&vec_out;
      // clear vec_out
      for (idx = 0; idx < 4; idx++, pv++)
         *pv = 0;

      /* If num_tests is exactly equal to nb_special_fargs, this implies the
       * instruction being tested only requires one floating point argument
       * (e.g. xssqrtdp).
       */
      if (test_group.num_tests == nb_special_fargs && !test_group.targs) {
         void * inB;
         int i;
         for (i = 0; i < nb_special_fargs; i++) {
            inB = (void *)&spec_fargs[i];
            frbp = (unsigned long long *)&spec_fargs[i];
            memcpy(&vec_inB, inB, 8);
            (*func)();
            dst = (unsigned long long *) &vec_out;
            printf("#%d: %s %016llx => %016llx\n", i, test_group.name, *frbp,
                   convToWord ? (*dst & 0x00000000ffffffffULL) : *dst);
         }
      } else {
         void * inA, * inB;
         unsigned int condreg, flags;
         int isTdiv = (strstr(test_group.name, "xstdivdp") != NULL) ? 1 : 0;
         int i;
         for (i = 0; i < test_group.num_tests; i++) {
            fp_test_args_t aTest = test_group.targs[i];
            inA = (void *)&spec_fargs[aTest.fra_idx];
            inB = (void *)&spec_fargs[aTest.frb_idx];
            frap = (unsigned long long *)&spec_fargs[aTest.fra_idx];
            frbp = (unsigned long long *)&spec_fargs[aTest.frb_idx];
            // Only need to copy one doubleword into each vector's element 0
            memcpy(&vec_inA, inA, 8);
            memcpy(&vec_inB, inB, 8);
            SET_FPSCR_ZERO;
            SET_CR_XER_ZERO;
            (*func)();
            GET_CR(flags);
            if (isTdiv) {
               condreg = (flags & 0x000000f0) >> 4;
               printf("#%d: %s %016llx,%016llx => cr %x\n", i, test_group.name, *frap, *frbp, condreg);
            } else {
               dst = (unsigned long long *) &vec_out;
               printf("#%d: %s %016llx,%016llx => %016llx\n", i, test_group.name,
                      *frap, *frbp, *dst);
            }
         }
      }
      printf( "\n" );
      k++;
   }
}

static void test_vx_aORm_fp_ops(void)
{
   /* These ops need a third src argument, which is stored in element 0 of
    * VSX[XT] -- i.e., vec_out.  For the xs<ZZZ>m{d|s}p cases, VSX[XT] holds
    * src3 and VSX[XB] holds src2; for the xs<ZZZ>a{d|s}p cases, VSX[XT] holds
    * src2 and VSX[XB] holds src3.  The fp_test_args_t that holds the test
    * data (input args, result) contain only two inputs, so I arbitrarily
    * choose some spec_fargs elements for the third source argument.
    * Note that that by using the same input data for a given pair of
    * a{d|s}p/m{d|s}p-type instructions (by swapping the src2 and src3
    * arguments), the expected result should be the same.
    */

   test_func_t func;
   int k;
   char * test_name = (char *)malloc(20);
   k = 0;
   do_dot = False;

   build_special_fargs_table();
   while ((func = vx_aORm_fp_tests[k].test_func)) {
      int i, stride;
      Bool repeat = False;
      Bool scalar = False;
      unsigned long long * frap, * frbp, * dst;
      vx_fp_test_t test_group = vx_aORm_fp_tests[k];
      vx_fp_test_type test_type = test_group.type;
      do_dp = test_group.precision == DOUBLE_TEST ? True : False;
      frap = frbp = NULL;

      if (test_type < VX_VECTOR_FP_MULT_AND_OP2) {
            scalar = True;
            strcpy(test_name, test_group.name);
            if (!repeat) {
               repeat = 1;
               stride = 1;
               // Only support double precision scalar ops in this function
               if (do_dp) {
                  strcat(test_name, "adp");
               } else {
                  fprintf(stderr, "Unsupported single precision for scalar op in test_vx_aORm_fp_ops\n");
                  exit(1);
               }
               do_aXp = True;
            }
      } else if (test_type < VX_BASIC_CMP) {
         // Then it must be a VX_VECTOR_xxx type
            stride = do_dp ? 2 : 4;
            if (!repeat) {
               // No need to work up the testcase name here, since that will be done in
               // the print_vx_aORm_fp_result() function we'll call for vector-type ops.
               repeat = 1;
               do_aXp = True;
            }
      } else {
            printf("ERROR:  Invalid VX FP test type %d\n", test_type);
            exit(1);
      }

again:
      for (i = 0; i < test_group.num_tests; i+=stride) {
         void  * inA, * inB;
         int m, fp_idx[4];
         unsigned long long vsr_XT[2];
         unsigned long long vsr_XB[2];
         fp_test_args_t aTest = test_group.targs[i];
         for (m = 0; m < stride; m++)
            fp_idx[m] = i % (nb_special_fargs - stride) + m;

         /* When repeat == True, we're on the first time through of one of the VX_FP_SMx
          * test types, meaning we're testing a xs<ZZZ>adp case, thus we have to swap
          * inputs as described above:
          *    src2 <= VSX[XT]
          *    src3 <= VSX[XB]
          */
         if (scalar) {
            // For scalar op, only need to copy one doubleword into each vector's element 0
            inA = (void *)&spec_fargs[aTest.fra_idx];
            inB = (void *)&spec_fargs[aTest.frb_idx];
            frap = (unsigned long long *)&spec_fargs[aTest.fra_idx];
            memcpy(&vec_inA, inA, 8);
            if (repeat) {
               memcpy(&vec_out, inB, 8);  // src2
               memcpy(&vec_inB, &spec_fargs[fp_idx[0]], 8);  //src3
               frbp = (unsigned long long *)&spec_fargs[fp_idx[0]];
            } else {
               frbp = (unsigned long long *)&spec_fargs[aTest.frb_idx];
               memcpy(&vec_inB, inB, 8);  // src2
               memcpy(&vec_out, &spec_fargs[fp_idx[0]], 8);  //src3
            }
            memcpy(vsr_XT, &vec_out, 8);
         } else {
            int j, loops = do_dp ? 2 : 4;
            size_t len = do_dp ? 8 : 4;
            void * vec_src = repeat ? (void *)&vec_inB : (void *)&vec_out;
            for (j = 0; j < loops; j++) {
               if (do_dp)
                  memcpy(vec_src + (j * len), &spec_fargs[fp_idx[j]], len);
               else
                  memcpy(vec_src + (j * len), &spec_sp_fargs[fp_idx[j]], len);
            }
            if (do_dp)
               setup_dp_fp_args(&test_group.targs[i], repeat);
            else
               setup_sp_fp_args(&test_group.targs[i], repeat);

            memcpy(vsr_XT, &vec_out, 16);
            memcpy(vsr_XB, &vec_inB, 16);
         }

         (*func)();
         dst = (unsigned long long *) &vec_out;
         if (test_type < VX_VECTOR_FP_MULT_AND_OP2)
            printf( "#%d: %s %s(%016llx,%016llx,%016llx) = %016llx\n", i,
                    test_name, test_group.op, vsr_XT[0], *frap, *frbp, *dst );
         else
            print_vx_aORm_fp_result(vsr_XT, vsr_XB, &test_group, i);
      }
      printf( "\n" );

      if (repeat) {
         repeat = 0;
         if (test_type < VX_VECTOR_FP_MULT_AND_OP2) {
               strcpy(test_name, test_group.name);
               strcat(test_name, "mdp");
         }
         do_aXp = False;
         goto again;
      }
      k++;
   }
   printf( "\n" );
   free(test_name);
}

static void test_vx_vector_one_fp_arg(void)
{
   test_func_t func;
   int k;
   k = 0;
   build_special_fargs_table();

   while ((func = vx_vector_one_fp_arg_tests[k].test_func)) {
      int idx, i;
      vx_fp_test_t test_group = vx_vector_one_fp_arg_tests[k];
      Bool convToWord = (test_group.type == VX_CONV_WORD);
      Bool dp = (test_group.precision == DOUBLE_TEST) ? True : False;
      Bool xvrespTest = (strstr(test_group.name , "xvresp") != NULL) ? True: False;
      int stride = dp ? 2 : 4;

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
            for (j = 0; j < 2; j++) {
               inB = (void *)&spec_fargs[i + j];
               // copy double precision FP into vector element i
               memcpy(((void *)&vec_inB) + (j * 8), inB, 8);
            }
            // execute test insn
            (*func)();
            dst_dp = (unsigned long long *) &vec_out;
            printf("#%d: %s ", i/2, test_group.name);
            for (j = 0; j < 2; j++) {
               if (j)
                  printf("; ");
               frB_dp = (unsigned long long *)&spec_fargs[i + j];
               printf("%s(%016llx)", test_group.op, *frB_dp);
               printf(" = %016llx", convToWord ? (dst_dp[j] & 0x00000000ffffffffULL) : dst_dp[j]);
            }
            printf("\n");
         } else {
            int j;
            unsigned int * frB_sp, * dst_sp;

            for (j = 0; j < 4; j++) {
               inB = (void *)&spec_sp_fargs[i + j];
               // copy single precision FP into vector element i
               memcpy(((void *)&vec_inB) + (j * 4), inB, 4);
            }
            // execute test insn
            (*func)();
            dst_sp = (unsigned int *) &vec_out;
            // print result
            printf("#%d: %s ", i/4, test_group.name);
            for (j = 0; j < 4; j++) {
               if (j)
                  printf("; ");
               frB_sp = (unsigned int *)&spec_sp_fargs[i + j];
               printf("%s(%08x)", test_group.op, *frB_sp);
               if (xvrespTest) {
                  float calc_diff = fabs(spec_sp_fargs[i + j]/256);
                  float sp_res;
                  memcpy(&sp_res, &dst_sp[j], 4);
                  float div_result = 1/spec_sp_fargs[i + j];
                  float real_diff = fabs(sp_res - div_result);
                  printf( " ==> %s",
                          ( ( sp_res == div_result )
                                   || ( isnan(sp_res) && isnan(div_result) )
                                   || ( real_diff <= calc_diff ) ) ? "PASS"
                                                                     : "FAIL");
               } else {
                  printf(" = %08x", dst_sp[j]);
               }
            }
            printf("\n");
         }
      }
      k++;
      printf( "\n" );
   }

}

/* This function assumes the instruction being tested requires two args. */
static void test_vx_vector_fp_ops(void)
{
   test_func_t func;
   int k;
   k = 0;
   build_special_fargs_table();

   while ((func = vx_vector_fp_tests[k].test_func)) {
      int idx, i, repeat = 1;
      vx_fp_test_t test_group = vx_vector_fp_tests[k];
      int stride = test_group.precision == DOUBLE_TEST ? 2 : 4;
      do_dot = False;

again:
      for (i = 0; i < test_group.num_tests; i+=stride) {
         unsigned int * pv, condreg;
         unsigned int flags;

         pv = (unsigned int *)&vec_out;
         if (test_group.precision == DOUBLE_TEST)
            setup_dp_fp_args(&test_group.targs[i], False);
         else
            setup_sp_fp_args(&test_group.targs[i], False);

         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         // execute test insn
         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)();
         GET_CR(flags);
         if (test_group.type == VX_BASIC_CMP) {
            condreg = (flags & 0x000000f0) >> 4;
         } else {
            condreg = VX_NOT_CMP_OP;
         }
         print_vector_fp_result(condreg, &test_group, i);
      }
      printf("\n");
      if (repeat && test_group.type == VX_BASIC_CMP) {
         repeat = 0;
         do_dot = True;
         goto again;
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
                                   { &test_divde, "divde", dw_tdata_len, DIV_BASE, DOUBLE_TEST },
                                   { &test_divde, "divdeo", dw_tdata_len, DIV_OE, DOUBLE_TEST },
#endif
                                   { &test_divweu, "divweu", w_tdata_len, DIV_BASE, SINGLE_TEST },
                                   { &test_divweu, "divweuo", w_tdata_len, DIV_OE, SINGLE_TEST },
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
            printf("0x%016llx / 0x%016llx = 0x%016llx;",
                   div_dw_tdata[i][0], div_dw_tdata[i][1], (signed long long) r17);
         } else {
            printf("0x%08x / 0x%08x = 0x%08x;",
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

static void test_fct_ops(void)
{
   test_func_t func;
   int k;
   k = 0;

   while ((func = fct_tests[k].test_func)) {
      int i, repeat = 1;
      simple_test_t test_group = fct_tests[k];
      do_dot = False;

again:
      for (i = 0; i < nb_special_fargs; i++) {
         double result;
#define SINGLE_MASK 0x00000000FFFFFFFFULL

         f14 = spec_fargs[i];
         // execute test insn
         SET_FPSCR_ZERO;
         (*func)();
         result = f17;
         printf("#%d: %s%s: ", i, test_group.name, do_dot ? "." : "");
         printf("0x%016llx (%e) ==> 0x%016llx\n",
                *((unsigned long long *)(&spec_fargs[i])), spec_fargs[i],
                test_group.precision == SINGLE_TEST ? (SINGLE_MASK &
                         *((unsigned long long *)(&result))) :
                         *((unsigned long long *)(&result)));
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

#ifdef __powerpc64__
void test_stdbrx(void)
{
   unsigned long long store, val = 0xdeadbacf12345678ULL;
   printf("stdbrx: 0x%llx ==> ", val);
   r17 = (HWord_t)val;
   r14 = (HWord_t)&store;
   __asm__ __volatile__ ("stdbrx %0, 0, %1" : : "r"(r17), "r"(r14));
   printf("0x%llx\n", store);
   printf( "\n" );
}
#endif

static test_table_t
         all_tests[] =
{
                    { &test_vx_vector_one_fp_arg,
                      "Test VSX vector single arg instructions"},
                    { &test_vx_vector_fp_ops,
                      "Test VSX floating point compare and basic arithmetic instructions" },
#ifdef __powerpc64__
                     { &test_bpermd,
                       "Test bit permute double"},
#endif
                     { &test_xxsel,
                         "Test xxsel instruction" },
                     { &test_xxspltw,
                         "Test xxspltw instruction" },
                     { &test_div_extensions,
                       "Test div extensions" },
                     { &test_fct_ops,
                       "Test floating point convert [word | doubleword] unsigned, with round toward zero" },
#ifdef __powerpc64__
                     { &test_stdbrx,
                      "Test stdbrx instruction"},
#endif
                     { &test_vx_aORm_fp_ops,
                      "Test floating point arithmetic instructions -- with a{d|s}p or m{d|s}p"},
                     { &test_vx_simple_scalar_fp_ops,
                      "Test scalar floating point arithmetic instructions"},
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
