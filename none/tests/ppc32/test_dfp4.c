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

typedef union stuff {
   _Decimal64  dec_val;
   _Decimal128  dec_val128;
   unsigned long long u64_val;
   struct {
      unsigned long long valu;
      unsigned long long vall;
   } u128;
} dfp_val_t;


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

/* In _test_dtstdc[q], DCM can be one of 6 possible data classes, numbered 0-5.
 * In reality, DCM is a 6-bit mask field.  We just test the individual values
 * and assume that masking multiple values would work OK.
 * BF is the condition register bit field which can range from 0-7.  But for
 * testing purposes, we only use BF values of '0' and '5'.
 */
static void _test_dtstdc(int BF, int DCM, dfp_val_t val1, dfp_val_t x1 __attribute__((unused)))
{
   _Decimal64 f14 = val1.dec_val;
   if (DCM < 0 || DCM > 5 || !(BF == 0 || BF == 5)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", BF, DCM);
      return;
   }
   switch (DCM) {
      case 0:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 1" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 1" : : "f" (f14));
         break;
      case 1:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 2" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 2" : : "f" (f14));
         break;
      case 2:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 4" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 4" : : "f" (f14));
         break;
      case 3:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 8" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 8" : : "f" (f14));
         break;
      case 4:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 16" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 16" : : "f" (f14));
         break;
      case 5:
         if (BF)
            __asm__ __volatile__ ("dtstdc 5, %0, 32" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdc 0, %0, 32" : : "f" (f14));
         break;
      default:
         break;
   }
}

static void _test_dtstdcq(int BF, int DCM, dfp_val_t val1, dfp_val_t x1 __attribute__((unused)))
{
   _Decimal128 f14 = val1.dec_val128;
   if (DCM < 0 || DCM > 5 || !(BF == 0 || BF == 5)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", BF, DCM);
      return;
   }
   switch (DCM) {
      case 0:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 1" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 1" : : "f" (f14));
         break;
      case 1:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 2" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 2" : : "f" (f14));
         break;
      case 2:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 4" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 4" : : "f" (f14));
         break;
      case 3:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 8" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 8" : : "f" (f14));
         break;
      case 4:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 16" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 16" : : "f" (f14));
         break;
      case 5:
         if (BF)
            __asm__ __volatile__ ("dtstdcq 5, %0, 32" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdcq 0, %0, 32" : : "f" (f14));
         break;
      default:
         break;
   }
}

/* In _test_dtstdg[q], DGM can be one of 6 possible data groups, numbered 0-5.
 * In reality, DGM is a 6-bit mask field.  We just test the individual values
 * and assume that masking multiple values would work OK.
 * BF is the condition register bit field which can range from 0-7.  But for
 * testing purposes, we only use BF values of '0' and '5'.
 */
static void _test_dtstdg(int BF, int DGM, dfp_val_t val1, dfp_val_t x1 __attribute__((unused)))
{
   _Decimal64 f14 = val1.dec_val;
   if (DGM < 0 || DGM > 5 || !(BF == 0 || BF == 5)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", BF, DGM);
      return;
   }
   switch (DGM) {
      case 0:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 1" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 1" : : "f" (f14));
         break;
      case 1:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 2" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 2" : : "f" (f14));
         break;
      case 2:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 4" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 4" : : "f" (f14));
         break;
      case 3:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 8" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 8" : : "f" (f14));
         break;
      case 4:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 16" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 16" : : "f" (f14));
         break;
      case 5:
         if (BF)
            __asm__ __volatile__ ("dtstdg 5, %0, 32" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdg 0, %0, 32" : : "f" (f14));
         break;
      default:
         break;
   }
}

static void _test_dtstdgq(int BF, int DGM, dfp_val_t val1, dfp_val_t x1 __attribute__((unused)))
{
   _Decimal128 f14 = val1.dec_val128;
   if (DGM < 0 || DGM > 5 || !(BF == 0 || BF == 5)) {
      fprintf(stderr, "Invalid inputs to asm test: a=%d, b=%d\n", BF, DGM);
      return;
   }
   switch (DGM) {
      case 0:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 1" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 1" : : "f" (f14));
         break;
      case 1:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 2" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 2" : : "f" (f14));
         break;
      case 2:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 4" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 4" : : "f" (f14));
         break;
      case 3:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 8" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 8" : : "f" (f14));
         break;
      case 4:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 16" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 16" : : "f" (f14));
         break;
      case 5:
         if (BF)
            __asm__ __volatile__ ("dtstdgq 5, %0, 32" : : "f" (f14));
         else
            __asm__ __volatile__ ("dtstdgq 0, %0, 32" : : "f" (f14));
         break;
      default:
         break;
   }
}

/* In _test_dtstex[q], BF is the condition register bit field indicating the
 * CR field in which the result of the test should be placed.  BF can range
 * from 0-7, but for testing purposes, we only use BF values of '4' and '7'.
 */
static void
_test_dtstex(int BF, int x __attribute__((unused)), dfp_val_t val1, dfp_val_t val2)
{
   _Decimal64 f14 = val1.dec_val;
   _Decimal64 f16 = val2.dec_val;
   if (!(BF == 4 || BF == 7)) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF);
      return;
   }
   switch (BF) {
      case 4:
         __asm__ __volatile__ ("dtstex  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dtstex  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}

static void _test_dtstexq(int BF, int x __attribute__((unused)), dfp_val_t val1, dfp_val_t val2)
{
   _Decimal128 f14 = val1.dec_val128;
   _Decimal128 f16 = val2.dec_val128;
   if (!(BF == 4 || BF == 7)) {
      fprintf(stderr, "Invalid input to asm test: a=%d\n", BF);
      return;
   }
   switch (BF) {
      case 4:
         __asm__ __volatile__ ("dtstexq  4, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      case 7:
         __asm__ __volatile__ ("dtstexq  7, %0, %1" :  : "f" (f14),"f" (f16));
         break;
      default:
         break;
   }
}



typedef void (*test_func_t)(int a, int b,  dfp_val_t val1,  dfp_val_t val2);
typedef void (*test_driver_func_t)(void);
typedef struct test_table
{
   test_driver_func_t test_category;
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

// Both Long and Quad arrays of DFP values should have the same length, so it
// doesn't matter which array I use for calculating the following #define.
#define NUM_DFP_VALS (sizeof(dfp64_vals)/8)

typedef struct dfp_test_args {
   int fra_idx;
   int frb_idx;
} dfp_test_args_t;


// Index pairs from dfp64_vals array to be used with dfp_two_arg_tests
static dfp_test_args_t dfp_2args_x1[] = {
                                    {0, 1},
                                    {2, 1},
                                    {4, 3},
                                    {6, 0},
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
} dfp_test_t;

typedef struct dfp_one_arg_test
{
   test_func_t test_func;
   const char * name;
   precision_type_t precision;
   const char * op;
} dfp_one_arg_test_t;



static dfp_one_arg_test_t
dfp_ClassAndGroupTest_tests[] = {
                                 { &_test_dtstdc,  "dtstdc", LONG_TEST, "[tCls]"},
                                 { &_test_dtstdcq, "dtstdcq", QUAD_TEST, "[tCls]"},
                                 { &_test_dtstdg,  "dtstdg", LONG_TEST, "[tGrp]"},
                                 { &_test_dtstdgq, "dtstdgq", QUAD_TEST, "[tGrp]"},
                                 { NULL, NULL, 0, NULL}
};

static void test_dfp_ClassAndGroupTest_ops(void)
{
   test_func_t func;
   dfp_val_t test_val, dummy;

   int k = 0;

   while ((func = dfp_ClassAndGroupTest_tests[k].test_func)) {
      int i;
      dfp_one_arg_test_t test_def = dfp_ClassAndGroupTest_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         int data_class_OR_group, BF = 0;
         Bool repeat = True;

         if (test_def.precision == LONG_TEST) {
            test_val.u64_val = dfp64_vals[i];
         } else {
            test_val.u128.valu = dfp128_vals[i * 2];
            test_val.u64_val = test_val.u128.valu;
            test_val.u128.vall = dfp128_vals[(i * 2) + 1];
         }

again:
         for (data_class_OR_group = 0; data_class_OR_group < 6; data_class_OR_group++) {
            unsigned int condreg;
            unsigned int flags;
            SET_FPSCR_ZERO;
            SET_CR_XER_ZERO;
            (*func)(BF, data_class_OR_group, test_val, dummy);
            GET_CR(flags);

            condreg = ((flags >> (4 * (7-BF)))) & 0xf;
            printf("%s (DC/DG=%d) %s%016llx", test_def.name, data_class_OR_group,
                   test_def.op, test_val.u64_val);
            if (test_def.precision == QUAD_TEST) {
               printf(" %016llx", test_val.u128.vall);
            }
            printf(" => %x (BF=%d)\n", condreg, BF);
         }
         if (repeat) {
            repeat = False;
            BF = 5;
            goto again;
         }
      }
      k++;
      printf( "\n" );
   }
}


static dfp_test_t
dfp_ExpTest_tests[] = {
                   { &_test_dtstex, "dtstex", dfp_2args_x1, 25, LONG_TEST, "[tExp]"},
                   { &_test_dtstexq, "dtstexq", dfp_2args_x1, 25, QUAD_TEST, "[tExp]"},
                   { NULL, NULL, NULL, 0, 0, NULL}
};


static void test_dfp_ExpTest_ops(void)
{
   dfp_val_t test_val1, test_val2;
   test_func_t func;
   int k = 0;

   while ((func = dfp_ExpTest_tests[k].test_func)) {
      /* BF is a 3-bit instruction field that indicates the CR field in which the
       * result of the test should be placed.  We won't iterate through all
       * 8 possible BF values since storing compare results to a given field is
       * a well-tested mechanism in VEX.  But we will test two BF values, just as
       * a sniff-test.
       */
      int i, repeat = 1, BF = 4;
      dfp_test_t test_def = dfp_ExpTest_tests[k];

again:
      for (i = 0; i < test_def.num_tests; i++) {
         unsigned int condreg;
         unsigned int flags;

         if (test_def.precision == LONG_TEST) {
            test_val1.u64_val = dfp64_vals[test_def.targs[i].fra_idx];
            test_val2.u64_val  = dfp64_vals[test_def.targs[i].frb_idx];
         } else {
            test_val1.u128.valu = dfp128_vals[test_def.targs[i].fra_idx * 2];
            test_val1.u64_val = test_val1.u128.valu;
            test_val1.u128.vall = dfp128_vals[(test_def.targs[i].fra_idx * 2) + 1];
            test_val2.u128.valu = dfp128_vals[test_def.targs[i].frb_idx * 2];
            test_val2.u64_val = test_val2.u128.valu;
            test_val2.u128.vall = dfp128_vals[(test_def.targs[i].frb_idx * 2) + 1];
         }

         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)(BF, 0, test_val1, test_val2);
         GET_CR(flags);

         condreg = ((flags >> (4 * (7-BF)))) & 0xf;
         printf("%s %016llx", test_def.name, test_val1.u64_val);
         if (test_def.precision == LONG_TEST) {
            printf(" %s %016llx ",
                   test_def.op, test_val2.u64_val);
         } else {
            printf(" %016llx %s %016llx %016llx ",
                   test_val1.u128.vall, test_def.op, test_val2.u128.valu, test_val2.u128.vall);
         }
         printf(" => %x (BF=%d)\n", condreg, BF);
      }
      if (repeat) {
         repeat = 0;
         BF = 7;
         goto again;
      }
      k++;
      printf( "\n" );
   }
}


static test_table_t
         all_tests[] =
{
                    { &test_dfp_ExpTest_ops,
                      "Test DFP exponent test instructions"},
                    { &test_dfp_ClassAndGroupTest_ops,
                      "Test DFP class and group test instructions"},
                    { NULL, NULL }
};
#endif // HAS_DFP

int main() {
#if defined(HAS_DFP)

   test_table_t aTest;
   test_driver_func_t func;
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
