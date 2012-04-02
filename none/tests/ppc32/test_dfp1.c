/*  Copyright (C) 2012 IBM

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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#if defined(HAS_DFP)

register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");
register double f18 __asm__ ("fr18");
register double f19 __asm__ ("fr19");


typedef unsigned char Bool;
#define True 1
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

#define GET_FPSCR(_arg) \
    __asm__ __volatile__ ("mffs %0"  : "=f"(_arg) )

#define SET_FPSCR_DRN \
    __asm__ __volatile__ ("mtfsf  1, %0, 0, 1" :  : "f"(f14) )


// The assembly-level instructions being tested
static Bool do_dot;
static void _test_dadd (void)
{
   if (do_dot)
      __asm__ __volatile__ ("dadd. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("dadd %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dsub (void)
{
   if (do_dot)
      __asm__ __volatile__ ("dsub. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("dsub %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dmul (void)
{
   if (do_dot)
      __asm__ __volatile__ ("dmul. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("dmul %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_ddiv (void)
{
   if (do_dot)
      __asm__ __volatile__ ("ddiv. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("ddiv %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

// Quad DFP arith instructions
static void _test_daddq (void)
{
   if (do_dot)
      __asm__ __volatile__ ("daddq. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("daddq %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dsubq (void)
{
   if (do_dot)
      __asm__ __volatile__ ("dsubq. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("dsubq %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_dmulq (void)
{
   if (do_dot)
      __asm__ __volatile__ ("dmulq. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("dmulq %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_ddivq (void)
{
   if (do_dot)
      __asm__ __volatile__ ("ddivq. %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
   else
      __asm__ __volatile__ ("ddivq %0, %1, %2" : "=f" (f18) : "f" (f14),"f" (f16));
}

static void _test_mffs (void)
{
   __asm__ __volatile__ ("mffs %0"  : "=f"(f14));
}

static void _test_mtfsf (int upper)
{
   if (upper)
      __asm__ __volatile__ ("mtfsf  1, %0, 0, 1" :  : "f"(f14) );
   else
      __asm__ __volatile__ ("mtfsf  1, %0, 0, 0" :  : "f"(f14) );
}

typedef void (*test_func_t)(void);
typedef struct test_table
{
   test_func_t test_category;
   char * name;
} test_table_t;

/*
 *  345.0DD (0x2207c00000000000 0xe50)
 *  1.2300e+5DD (0x2207c00000000000 0x14c000)
 *  -16.0DD (0xa207c00000000000 0xe0)
 *  0.00189DD (0x2206c00000000000 0xcf)
 *  -4.1235DD (0xa205c00000000000 0x10a395bcf)
 *  9.8399e+20DD (0x2209400000000000 0x253f1f534acdd4)
 *  0DD (0x2208000000000000 0x0)
 *  0DD (0x2208000000000000 0x0)
 *  infDD (0x7800000000000000 0x0)
 *  nanDD (0x7c00000000000000 0x0
 */
static unsigned long long dfp128_vals[] = {
                                    // Some finite numbers
                                    0x2207c00000000000ULL, 0x0000000000000e50ULL,
                                    0x2207c00000000000ULL, 0x000000000014c000ULL,
                                    0xa207c00000000000ULL, 0x00000000000000e0ULL,
                                    0x2206c00000000000ULL, 0x00000000000000cfULL,
                                    0xa205c00000000000ULL, 0x000000010a395bcfULL,
                                    0x6209400000fd0000ULL, 0x00253f1f534acdd4ULL, // huge number
                                    0x000400000089b000ULL, 0x0a6000d000000049ULL, // very small number
                                    // flavors of zero
                                    0x2208000000000000ULL, 0x0000000000000000ULL,
                                    0xa208000000000000ULL, 0x0000000000000000ULL, // negative
                                    0xa248000000000000ULL, 0x0000000000000000ULL,
                                    // flavors of NAN
                                    0x7c00000000000000ULL, 0x0000000000000000ULL, // quiet
                                    0xfc00000000000000ULL, 0xc00100035b007700ULL,
                                    0x7e00000000000000ULL, 0xfe000000d0e0a0d0ULL, // signaling
                                    // flavors of Infinity
                                    0x7800000000000000ULL, 0x0000000000000000ULL,
                                    0xf800000000000000ULL, 0x0000000000000000ULL, // negative
                                    0xf900000000000000ULL, 0x0000000000000000ULL
};

static unsigned long long dfp64_vals[] = {
                                 // various finite numbers
                                 0x2234000000000e50ULL,
                                 0x223400000014c000ULL,
                                 0xa2340000000000e0ULL,// negative
                                 0x22240000000000cfULL,
                                 0xa21400010a395bcfULL,// negative
                                 0x6e4d3f1f534acdd4ULL,// huge number
                                 0x000400000089b000ULL,// very small number
                                 // flavors of zero
                                 0x2238000000000000ULL,
                                 0xa238000000000000ULL,
                                 0x4248000000000000ULL,
                                 // flavors of NAN
                                 0x7e34000000000111ULL,
                                 0xfe000000d0e0a0d0ULL,//signaling
                                 0xfc00000000000000ULL,//quiet
                                 // flavors of Infinity
                                 0x7800000000000000ULL,
                                 0xf800000000000000ULL,//negative
                                 0x7a34000000000000ULL,
};


typedef struct dfp_test_args {
   int fra_idx;
   int frb_idx;
} dfp_test_args_t;


// Index pairs from dfp64_vals or dfp128_vals array to be used with dfp_two_arg_tests
static dfp_test_args_t dfp_2args_x2[] = {
                                  {0, 1},
                                  {2, 1},
                                  {3, 4},
                                  {0, 6},
                                  {2, 4},
                                  {5, 1},
                                  {5, 2},
                                  {7, 8},
                                  {7, 1},
                                  {9, 15},
                                  {8, 12},
                                  {7, 11},
                                  {13, 2},
                                  {13, 14},
                                  {15, 12},
                                  {14, 11},
                                  {12, 12},
                                  {12, 11},
                                  {11, 11}
};

// Index pairs from dfp64_vals array to be used with dfp_two_arg_tests
static dfp_test_args_t dfp_2args_x1[] = {
                                    {0, 1},
                                    {2, 1},
                                    {3, 4},
                                    {0, 6},
                                    {2, 4},
                                    {5, 1},
                                    {5, 2},
                                    {7, 1},
                                    {7, 2},
                                    {8, 0},
                                    {8, 1},
                                    {8, 2},
                                    {7, 8},
                                    {12, 14},
                                    {12, 1},
                                    {12, 13},
                                    {12, 12},
                                    {12, 11},
                                    {11, 14},
                                    {11, 0},
                                    {11, 13},
                                    {11, 11},
                                    {14, 14},
                                    {14, 3},
                                    {14, 15},
};

typedef enum {
   LONG_TEST,
   QUAD_TEST
} precision_type_t;

typedef struct dfp_test
{
   test_func_t test_func;
   const char * name;
   dfp_test_args_t * targs;
   int num_tests;
   precision_type_t precision;
   const char * op;
   Bool cr_supported;
} dfp_test_t;


static dfp_test_t
dfp_two_arg_tests[] = {
                     { &_test_dadd, "dadd", dfp_2args_x1, 25, LONG_TEST, "+", False},
                     { &_test_dsub, "dsub", dfp_2args_x1, 25, LONG_TEST, "-", False},
                     { &_test_dmul, "dmul", dfp_2args_x2, 19, LONG_TEST, "*", False},
                     { &_test_ddiv, "ddiv", dfp_2args_x2, 19, LONG_TEST, "/", False},
                     { &_test_daddq, "daddq", dfp_2args_x1, 25, QUAD_TEST, "+", False},
                     { &_test_dsubq, "dsubq", dfp_2args_x1, 25, QUAD_TEST, "-", False},
                     { &_test_dmulq, "dmulq", dfp_2args_x2, 19, QUAD_TEST, "*", False},
                     { &_test_ddivq, "ddivq", dfp_2args_x2, 19, QUAD_TEST, "/", False},
                     { NULL, NULL, NULL, 0, 0, NULL}
};

static void test_dfp_two_arg_ops(void)
{
   test_func_t func;
   unsigned long long u0, u0x, u1, u1x;
   double res, d0, d1, *d0p, *d1p;
   double d0x, d1x, *d0xp, *d1xp;
   int k = 0;
   u0x = u1x = 0;
   d0p = &d0;
   d0xp = &d0x;
   d1p = &d1;
   d1xp = &d1x;

   while ((func = dfp_two_arg_tests[k].test_func)) {
      int i, repeat = 1;
      dfp_test_t test_group = dfp_two_arg_tests[k];
      do_dot = False;

again:
      for (i = 0; i < test_group.num_tests; i++) {
         unsigned int condreg;
         unsigned int flags;

         if (test_group.precision == LONG_TEST) {
            u0 = dfp64_vals[test_group.targs[i].fra_idx];
            u1 = dfp64_vals[test_group.targs[i].frb_idx];
         } else {
            u0 = dfp128_vals[test_group.targs[i].fra_idx * 2];
            u0x = dfp128_vals[(test_group.targs[i].fra_idx * 2) + 1];
            u1 = dfp128_vals[test_group.targs[i].frb_idx * 2];
            u1x = dfp128_vals[(test_group.targs[i].frb_idx * 2) + 1];
         }
         *(unsigned long long *)d0p = u0;
         *(unsigned long long *)d1p = u1;
         f14 = d0;
         f16 = d1;
         if (test_group.precision == QUAD_TEST) {
            *(unsigned long long *)d0xp = u0x;
            *(unsigned long long *)d1xp = u1x;
            f15 = d0x;
            f17 = d1x;
         }

         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)();
         GET_CR(flags);
         res = f18;

         condreg = (flags & 0x000000f0) >> 4;
         printf("%s%s %016llx", test_group.name, do_dot? "." : "", u0);
         if (test_group.precision == LONG_TEST) {
            printf(" %s %016llx => %016llx",
                   test_group.op, u1, *((unsigned long long *)(&res)));
         } else {
            double resx = f19;
            printf(" %016llx %s %016llx %016llx ==> %016llx %016llx",
                   u0x, test_group.op, u1, u1x,
                   *((unsigned long long *)(&res)), *((unsigned long long *)(&resx)));
         }
         if (test_group.cr_supported)
            printf(" (cr = %08x)\n", condreg);
         else
            printf("\n");

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

void test_move_toFrom_fpscr(void)
{
#define BFP_MAX_RM 3
   int shift = 0;
   unsigned long long i, max_rm, expected_val;
   double fpscr_in, fpscr_out;
   unsigned long long * hex_fpscr_in = (unsigned long long *)&fpscr_in;
   unsigned long long * hex_fpscr_out = (unsigned long long *)&fpscr_out;


   max_rm = 4;
again:
   /* NOTE: The first time through this loop is for setting the binary
    * floating point rounding mode (bits 62:63 of FPSCR).  The second time
    * through is for setting the decimal floating point rounding mode
    * (bits 29:31 of FPSCR).  In the second time through this loop, the value
    * returned should include the final binary FP rounding mode, along with
    * the decimal FP rounding modes.
    */
   for (i = 0; i < max_rm; i++) {
      *hex_fpscr_in = (i << shift);
      f14 = fpscr_in;
      _test_mtfsf(max_rm/8);
      *hex_fpscr_in = 0ULL;
      f14= fpscr_in;
      _test_mffs();
      fpscr_out = f14;
      if (max_rm == 4) {
         *hex_fpscr_out &= (max_rm - 1) << shift;
         expected_val = i << shift;
      } else {
         *hex_fpscr_out &= BFP_MAX_RM | ((max_rm - 1) << shift);
         expected_val = (i << shift) | BFP_MAX_RM;
      }

      printf("FPSCR %s floating point rounding mode %016llx == %016llx? %s\n",
             (max_rm == 8) ? "decimal" : "binary",
             *hex_fpscr_out, expected_val,
             (expected_val == *hex_fpscr_out) ? "yes" : "no");
   }
   if (max_rm == 4) {
      max_rm = 8;
      shift = 32;
      goto again;
   }
}

void test_rounding_modes(void)
{
   int j;
   unsigned long long u0, u1, rm_idx;
   double res, d0, d1, *d0p, *d1p, fpscr;
   unsigned long long * hex_fpscr = (unsigned long long *)&fpscr;
   u0 = 0x26cc3f1f534acdd4ULL;
   u1 = 0x27feff197a42ba06ULL;
   d0p = &d0;
   d1p = &d1;

   for (j = 0; j < 12; j++) {
      for (rm_idx = 0; rm_idx < 8; rm_idx++) {
         *hex_fpscr = 0ULL;
         __asm__ __volatile__ ("mffs %0"  : "=f"(f14));
         fpscr = f14;
         *hex_fpscr &= 0xFFFFFFF0FFFFFFFFULL;
         *hex_fpscr |= (rm_idx << 32);
         f14 = fpscr;
         SET_FPSCR_DRN;
         *(unsigned long long *)d0p = u0;
         *(unsigned long long *)d1p = u1;
         f14 = d0;
         f16 = d1;
         _test_dmul();
         res = f18;
         printf("test #%d: dmul with rounding mode %d: %016llx * %016llx => %016llx\n",
                j, (int)rm_idx, u0, u1, *((unsigned long long *)(&res)));
         printf("\n");
      }
      // Changing the least significant bit of one of the dmul arguments give us more
      // opportunities for different rounding modes to yield different results which
      // can then be validated.
      u0++;
   }
}

static test_table_t
         all_tests[] =
{
                    { &test_dfp_two_arg_ops,
                      "Test DFP arithmetic instructions"},
                    { &test_rounding_modes,
                      "Test DFP rounding modes"},
                    { &test_move_toFrom_fpscr,
                    "Test move to/from FPSCR"},
                    { NULL, NULL }
};
#endif // HAS_DFP

int main() {
#if defined(HAS_DFP)

   test_table_t aTest;
   test_func_t func;
   int i = 0;

   while ((func = all_tests[i].test_category)) {
      aTest = all_tests[i];
      printf( "%s\n", aTest.name );
      (*func)();
      i++;
   }

#endif // HAS_DFP
   return 0;
}
