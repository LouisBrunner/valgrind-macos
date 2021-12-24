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
 along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#if defined(VGP_ppc64le_linux)
      unsigned long long vall;
      unsigned long long valu;
#else
      unsigned long long valu;
      unsigned long long vall;
#endif
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

#ifndef __powerpc64__
typedef uint32_t HWord_t;
#else
typedef uint64_t HWord_t;
#endif /* __powerpc64__ */

enum BF_vals { BF_val1 = 0, BF_val2 = 1, BF_val3 =6};

// The assembly-level instructions being tested
static void _test_dtstsf(unsigned int BF, unsigned int ref_sig, dfp_val_t *valB)
{
   _Decimal64 f16 = valB->dec_val;
   register HWord_t r14 __asm__ ("r14");
   double f14;
   r14 = (HWord_t)&ref_sig;

   __asm __volatile__ ("lfiwax %0, 0, %1" : "=f" (f14): "r" (r14));
   switch (BF) {
      case BF_val1:
         __asm__ __volatile__ ("dtstsf %0, %1, %2" : : "i" (BF_val1), "f" (f14), "f" (f16));
         break;
      case BF_val2:
         __asm__ __volatile__ ("dtstsf %0, %1, %2" : : "i" (BF_val2), "f" (f14), "f" (f16));
         break;
      case BF_val3:
         __asm__ __volatile__ ("dtstsf %0, %1, %2" : : "i" (BF_val3), "f" (f14), "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for BF\n", BF);
         break;
   }
}

static void _test_dtstsfq(unsigned int BF, unsigned int ref_sig, dfp_val_t *valB)
{
   _Decimal128 f16 = valB->dec_val128;
   register HWord_t r14 __asm__ ("r14");
   double f14;
   r14 = (HWord_t)&ref_sig;

   __asm __volatile__ ("lfiwax %0, 0, %1" : "=f" (f14): "r" (r14));
   switch (BF) {
      case BF_val1:
         __asm__ __volatile__ ("dtstsfq %0, %1, %2" : : "i" (BF_val1), "f" (f14), "f" (f16));
         break;
      case BF_val2:
         __asm__ __volatile__ ("dtstsfq %0, %1, %2" : : "i" (BF_val2), "f" (f14), "f" (f16));
         break;
      case BF_val3:
         __asm__ __volatile__ ("dtstsfq %0, %1, %2" : : "i" (BF_val3), "f" (f14), "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for BF\n", BF);
         break;
   }
}

static dfp_val_t _test_ddedpd(unsigned int SP, dfp_val_t *valB)
{
   _Decimal64 ret = 0;
   dfp_val_t result;
   _Decimal64 f16 = valB->dec_val;
   switch (SP) {
      case 0:
         __asm__ __volatile__ ("ddedpd. 0, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("ddedpd. 1, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("ddedpd. 2, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("ddedpd. 3, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for SP\n", SP);
         break;
   }
   result.dec_val = ret;
   return result;
}


static dfp_val_t _test_ddedpdq(unsigned int SP, dfp_val_t *valB)
{
   _Decimal128 ret = 0;
   dfp_val_t result;
   _Decimal128 f16 = valB->dec_val128;
   switch (SP) {
      case 0:
         __asm__ __volatile__ ("ddedpdq 0, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("ddedpdq 1, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 2:
         __asm__ __volatile__ ("ddedpdq 2, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 3:
         __asm__ __volatile__ ("ddedpdq 3, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for SP\n", SP);
         break;
   }
   result.dec_val128 = ret;
   return result;
}

static dfp_val_t _test_denbcd(unsigned int S, dfp_val_t *valB)
{
   _Decimal64 ret = 0;
   dfp_val_t result;
   _Decimal64 f16 = valB->dec_val;
   switch (S) {
      case 0:
         __asm__ __volatile__ ("denbcd. 0, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("denbcd. 1, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for S\n", S);
         break;
   }
   result.dec_val = ret;
   return result;
}


static dfp_val_t _test_denbcdq(unsigned int S, dfp_val_t *valB)
{
   _Decimal128 ret = 0;
   dfp_val_t result;
   _Decimal128 f16 = valB->dec_val128;
   switch (S) {
      case 0:
         __asm__ __volatile__ ("denbcdq 0, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      case 1:
         __asm__ __volatile__ ("denbcdq 1, %0, %1" : "=f" (ret) : "f" (f16));
         break;
      default:
         fprintf(stderr, "Invalid value %d for S\n", S);
         break;
   }
   result.dec_val128 = ret;
   return result;
}


typedef void (*test_funcp_t)(unsigned int imm, unsigned int imm2,  dfp_val_t *valB);
typedef dfp_val_t (*test_func_bcdp_t)(unsigned int imm, dfp_val_t *valB);
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

/* The bcd64_vals and bdc128_vals hold the unique results of executing
 * the ddedpd instruction on the basic dfp64 and dfp128 array values.
 * Executing the inverse operation (denbcd) on these values with the
 * appropriate S (signed) value should yield values approximating the
 * original dfp values (except being 2^4 in magnitude since the decoding
 * operation shifted the value one hex digit to the left to make room
 * for signedness info).
 */
static unsigned long long bcd64_vals[] = {
                                          0x0000000000003450ULL,
                                          0x000000000003450cULL,
                                          0x000000000003450fULL,
                                          0x0000000001230000ULL,
                                          0x000000001230000cULL,
                                          0x000000001230000fULL,
                                          0x0000000000000160ULL,
                                          0x000000000000160dULL,
                                          0x0000000000000189ULL,
                                          0x000000000000189cULL,
                                          0x000000000000189fULL,
                                          0x0000004123456789ULL,
                                          0x000004123456789dULL,
                                          0x9839871234533354ULL,
                                          0x839871234533354cULL,
                                          0x839871234533354fULL,
                                          0x0000000008864000ULL,
                                          0x000000008864000cULL,
                                          0x000000008864000fULL,
                                          0x0000000000000000ULL,
                                          0x000000000000000cULL,
                                          0x000000000000000fULL,
                                          0x000000000000000dULL,
                                          0x0000000000000211ULL,
                                          0x000000000000211cULL,
                                          0x000000000000211fULL,
                                          0x0000003882028150ULL,
                                          0x000003882028150dULL
 };

static unsigned long long bcd128_vals[] = {
                                           0x0000000000000000ULL, 0x0000000000003450ULL,
                                           0x0000000000000000ULL, 0x000000000003450cULL,
                                           0x0000000000000000ULL, 0x000000000003450fULL,
                                           0x0000000000000000ULL, 0x0000000001230000ULL,
                                           0x0000000000000000ULL, 0x000000001230000cULL,
                                           0x0000000000000000ULL, 0x000000001230000fULL,
                                           0x0000000000000000ULL, 0x0000000000000160ULL,
                                           0x0000000000000000ULL, 0x000000000000160dULL,
                                           0x0000000000000000ULL, 0x0000000000000189ULL,
                                           0x0000000000000000ULL, 0x000000000000189cULL,
                                           0x0000000000000000ULL, 0x000000000000189fULL,
                                           0x0000000000000000ULL, 0x0000004123456789ULL,
                                           0x0000000000000000ULL, 0x000004123456789dULL,
                                           0x0000097100000000ULL, 0x9839871234533354ULL,
                                           0x0000971000000009ULL, 0x839871234533354cULL,
                                           0x0000971000000009ULL, 0x839871234533354fULL,
                                           0x0000010954000051ULL, 0x8000640000000049ULL,
                                           0x0000109540000518ULL, 0x000640000000049cULL,
                                           0x0000109540000518ULL, 0x000640000000049fULL,
                                           0x0000000000000000ULL, 0x0000000000000000ULL,
                                           0x0000000000000000ULL, 0x000000000000000cULL,
                                           0x0000000000000000ULL, 0x000000000000000fULL,
                                           0x0000000000000000ULL, 0x000000000000000dULL,
                                           0x0000000000080000ULL, 0x0200801330811600ULL,
                                           0x0000000000800000ULL, 0x200801330811600dULL,
                                           0x0000000000088170ULL, 0x0000003882028150ULL,
                                           0x0000000000881700ULL, 0x000003882028150cULL,
                                           0x0000000000881700ULL, 0x000003882028150fULL
};

// Both Long and Quad arrays of DFP values should have the same length, so it
// doesn't matter which array I use for calculating the following #define.
#define NUM_DFP_VALS (sizeof(dfp64_vals)/8)

typedef enum {
   LONG_TEST,
   QUAD_TEST
} precision_type_t;

typedef struct dfp_one_arg_test
{
   test_funcp_t test_func;
   const char * name;
   precision_type_t precision;
   const char * op;
} dfp_one_arg_test_t;

typedef struct dfp_one_arg_bcd_test
{
   test_func_bcdp_t test_func;
   const char * name;
   precision_type_t precision;
   const char * op;
} dfp_one_arg_bcd_test_t;

static dfp_one_arg_bcd_test_t
dfp_test_dfp_ddedpd_tests[] = {
                            { &_test_ddedpd, "ddedpd", LONG_TEST, "[D->B]"},
                            { &_test_ddedpdq, "ddedpdq", QUAD_TEST, "[D->B]"},
                            { NULL, NULL, 0, NULL}
};

static void test_dfp_ddedpd_ops(void)
{
   test_func_bcdp_t func;
   dfp_val_t test_val;

   int k = 0;

   while ((func = dfp_test_dfp_ddedpd_tests[k].test_func)) {
      int i;
      dfp_one_arg_bcd_test_t test_def = dfp_test_dfp_ddedpd_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         unsigned int SP;

         if (test_def.precision == LONG_TEST) {
            test_val.u64_val = dfp64_vals[i];
         } else {
            test_val.u128.valu = dfp128_vals[i * 2];
            test_val.u128.vall = dfp128_vals[(i * 2) + 1];
         }

         for (SP = 0; SP < 4; SP++) {
            dfp_val_t result;

	    /* There is an ABI change in how 128 bit arguments are aligned 
             * with GCC 5.0.  The compiler generates a "note" about this
             * starting with GCC 4.8.  To avoid generating the "note", pass
             * the address of the 128-bit arguments rather then the value.
	     */
            result = (*func)(SP, &test_val);
            printf("%s (SP=%d) %s", test_def.name, SP, test_def.op);
            if (test_def.precision == LONG_TEST) {
               printf("%016llx ==> %016llx\n", test_val.u64_val, result.u64_val);
            } else {
               printf("%016llx %016llx ==> %016llx %016llx\n",
                      test_val.u128.valu, test_val.u128.vall,
                      result.u128.valu, result.u128.vall);
            }
         }
      }
      k++;
      printf( "\n" );
   }
}

static dfp_one_arg_bcd_test_t
dfp_test_dfp_denbcd_tests[] = {
                            { &_test_denbcd, "denbcd", LONG_TEST, "[B->D]"},
                            { &_test_denbcdq, "denbcdq", QUAD_TEST, "[B->D]"},
                            { NULL, NULL, 0, NULL}
};

static void test_dfp_denbcd_ops(void)
{
   test_func_bcdp_t func;
   dfp_val_t test_val;
   int num_test_vals;

   int k = 0;

   while ((func = dfp_test_dfp_denbcd_tests[k].test_func)) {
      int i;
      dfp_one_arg_bcd_test_t test_def = dfp_test_dfp_denbcd_tests[k];
      if (test_def.precision == LONG_TEST)
         num_test_vals = sizeof(bcd64_vals)/sizeof(unsigned long long);
      else
         num_test_vals = sizeof(bcd128_vals)/(2 * sizeof(unsigned long long));

      for (i = 0; i < num_test_vals; i++) {
         unsigned int S;
         dfp_val_t result;
         /* The DPD-to-BCD decodings may contain up to 3 decodings for each normal DFP
          * value: the first is an unsigned decoding, and the other two are
          * signed decodings, with SP[1] set to '0' and '1' respectively at decode
          * time. But some of the results of decodings were duplicates, so they were
          * not included in the bcd64_vals and bcd128_vals arrays.
          *
          * When doing the encoding operation (denbcd), we'll attempt both S=0 and
          * S=1; one or the other should encode the BCD value to something close to
          * its original DFP value (except being 2^4 in magnitude since the decoding
          * operation shifted the value one hex digit to the left to make room
          * for signedness info).
          */
         for (S = 0; S < 2; S++) {
            if (test_def.precision == LONG_TEST) {
               test_val.u64_val = bcd64_vals[i];
            } else {
               test_val.u128.valu = bcd128_vals[i * 2];
               test_val.u128.vall = bcd128_vals[(i * 2) + 1];
            }

	    /* There is an API change in how 128 bit arguments are aligned 
             * with GCC 5.0.  The compiler generates a "note" about this
             * starting with GCC 4.8.  To avoid generating the "note", pass
             * the address of the 128-bit arguments rather then the value.
	     */
            result = (*func)(S, &test_val);
            printf("%s (S=%d) %s", test_def.name, S, test_def.op);
            if (test_def.precision == LONG_TEST) {
               printf("%016llx ==> %016llx\n", test_val.u64_val, result.u64_val);
            } else {
               printf("%016llx %016llx ==> %016llx %016llx\n",
                      test_val.u128.valu, test_val.u128.vall,
                      result.u128.valu, result.u128.vall);
            }
         }
      }
      k++;
      printf( "\n" );
   }
}


static dfp_one_arg_test_t
dfp_test_significance_tests[] = {
                                          { &_test_dtstsf,  "dtstsf", LONG_TEST, "[tSig]"},
                                          { &_test_dtstsfq, "dtstsfq", QUAD_TEST, "[tSig]"},
                                          { NULL, NULL, 0, NULL}
};

static void test_dfp_test_significance_ops(void)
{
   test_funcp_t func;
   dfp_val_t test_valB;
   int k = 0;
   unsigned int BF_vals[] = {BF_val1, BF_val2, BF_val3};
   unsigned int reference_sig, reference_sig_vals[] = {0U, 1U, 2U, 4U, 6U, 63U};
   int num_reference_sig_vals = sizeof(reference_sig_vals)/sizeof(unsigned int);

   while ((func = dfp_test_significance_tests[k].test_func)) {
      int i;
      dfp_one_arg_test_t test_def = dfp_test_significance_tests[k];

      for (i = 0; i < NUM_DFP_VALS; i++) {
         int j;
         if (test_def.precision == LONG_TEST) {
            test_valB.u64_val = dfp64_vals[i];
         } else {
            test_valB.u128.valu = dfp128_vals[i * 2];
            test_valB.u128.vall = dfp128_vals[(i * 2) + 1];
         }

         for (j = 0; j < num_reference_sig_vals; j++) {
            int bf_idx, BF;
            reference_sig = reference_sig_vals[j];
            for (bf_idx = 0; bf_idx < sizeof(BF_vals)/sizeof(unsigned int); bf_idx++) {
               unsigned int condreg;
               unsigned int flags;
               BF = BF_vals[bf_idx];
               SET_FPSCR_ZERO;
               SET_CR_XER_ZERO;
               /* There is an ABI change in how 128 bit arguments are aligned 
                * with GCC 5.0.  The compiler generates a "note" about this
                * starting with GCC 4.9.  To avoid generating the "note", pass
                * the address of the 128-bit arguments rather then the value.
                */
               (*func)(BF, reference_sig, &test_valB);
               GET_CR(flags);

               condreg = ((flags >> (4 * (7-BF)))) & 0xf;
               printf("%s (ref_sig=%d) %s", test_def.name, reference_sig, test_def.op);
               if (test_def.precision == LONG_TEST) {
                  printf("%016llx", test_valB.u64_val);
               } else {
                  printf("%016llx %016llx", test_valB.u128.valu, test_valB.u128.vall);
               }
               printf(" => %x (BF=%d)\n", condreg, BF);
            }
         }
         printf( "\n" );
      }
      k++;
   }
}

static test_table_t
         all_tests[] =
{
                    { &test_dfp_test_significance_ops,
                      "Test DFP test significance instructions"},
                    { &test_dfp_ddedpd_ops,
                      "Test DFP DPD-to-BCD instructions"},
                    { &test_dfp_denbcd_ops,
                      "Test DFP BCD-to-DPD instructions"},
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

#else
   printf("HAS_DFP not detected.\n");
#endif // HAS_DFP
   return 0;
}
