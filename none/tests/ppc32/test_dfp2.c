/*  Copyright (C) 2012 IBM

 Author: Maynard Johnson <maynardj@us.ibm.com>
         Carl Love <carll@us.ibm.com>

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
#include <string.h>
#include <elf.h>
#include <link.h>

#define PPC_FEATURE_HAS_VSX  0x00000080 /* Vector Scalar Extension. */

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

#define SET_FPSCR_ZERO \
		do { double _d = 0.0;		                           \
		__asm__ __volatile__ ("mtfsf 0xFF, %0" : : "f"(_d) ); \
		} while (0)

#define GET_FPSCR(_arg) \
  __asm__ __volatile__ ("mffs %0"  : "=f"(_arg) )

#define SET_FPSCR_DRN \
  __asm__ __volatile__ ("mtfsf  1, %0, 0, 1" :  : "f"(f14) )

#define SH_0  0
#define SH_1  1
#define SH_2  15
#define SH_3  63

#define NUM_RND_MODES  8
#define CONDREG_MASK  0x0f000000
#define CONDREG_SHIFT 24

static char ** my_envp;
static inline char** __auxv_find(void)
{
   char **result = my_envp;
   /* Scan over the env vector looking for the ending NULL */
   for (; *result != NULL; ++result) {
   }
   /* Bump the pointer one more step, which should be the auxv. */
   return ++result;
}

static unsigned long fetch_at_hwcap(void)
{
   static unsigned long auxv_hwcap = 0;
   int i;
   ElfW(auxv_t) * auxv_buf = NULL;

   if (auxv_hwcap)
      return auxv_hwcap;

   auxv_buf = (ElfW(auxv_t)*) __auxv_find();
   for (i = 0; auxv_buf[i].a_type != AT_NULL; i++)
      if (auxv_buf[i].a_type == AT_HWCAP) {
         auxv_hwcap = auxv_buf[i].a_un.a_val;
         break;
      }

   return auxv_hwcap;
}

int get_vsx(void) 
{
   /* Check to see if the AUX vector has the bit set indicating the HW
    * supports the vsx instructions.  This implies the processor is
    * at least a POWER 7.
    */
   unsigned long hwcap;

   hwcap = fetch_at_hwcap();
   if ((hwcap & PPC_FEATURE_HAS_VSX) == PPC_FEATURE_HAS_VSX)
      return 1;

   return 0;
}

/* The assembly-level instructions being tested */
static void _test_dscri (int shift)
{
   switch(shift) {
   case SH_0:
      __asm__ __volatile__ ("dscri  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_0));
      break;

   case SH_1:
      __asm__ __volatile__ ("dscri  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_1));
      break;

   case SH_2:
      __asm__ __volatile__ ("dscri  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_2));
      break;

   case SH_3:
      __asm__ __volatile__ ("dscri  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_3));
      break;
   default:
      printf(" dscri, unsupported shift case %d\n", shift);
   }
}

static void _test_dscli (int shift)
{
   switch(shift) {
   case SH_0:
      __asm__ __volatile__ ("dscli  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_0));
      break;

   case SH_1:
      __asm__ __volatile__ ("dscli  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_1));
      break;

   case SH_2:
      __asm__ __volatile__ ("dscli  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_2));
      break;

   case SH_3:
      __asm__ __volatile__ ("dscli  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_3));
      break;
   default:
      printf(" dscli, unsupported shift case %d\n", shift);
   }
}

static void _test_dctdp (void)
{
   __asm__ __volatile__ ("dctdp  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_drsp (void)
{
   __asm__ __volatile__ ("drsp  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_dctfix (void)
{
   __asm__ __volatile__ ("dctfix  %0, %1" : "=f" (f18) : "f" (f14));
}

/* Power 7 and newer processors support this instruction */
static void _test_dcffix (void)
{
   __asm__ __volatile__ ("dcffix  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_dscriq (int shift)
{
   switch(shift) {
   case SH_0:
      __asm__ __volatile__ ("dscriq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_0));
      break;
   case SH_1:
      __asm__ __volatile__ ("dscriq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_1));
      break;
   case SH_2:
      __asm__ __volatile__ ("dscriq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_2));
      break;
   case SH_3:
      __asm__ __volatile__ ("dscriq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_3));
      break;
   default:
      printf(" dscriq, unsupported shift case %d\n", shift);
   }
}

static void _test_dscliq (int shift)
{
   switch(shift) {
   case SH_0:
      __asm__ __volatile__ ("dscliq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_0));
      break;
   case SH_1:
      __asm__ __volatile__ ("dscliq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_1));
      break;
   case SH_2:
      __asm__ __volatile__ ("dscliq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_2));
      break;
   case SH_3:
      __asm__ __volatile__ ("dscliq  %0, %1, %2" : "=f" (f18) : "f" (f14), "i" (SH_3));
      break;
   default:
      printf(" dscliq, unsupported shift case %d\n", shift);
   }
}

static void _test_dctqpq (void)
{
   __asm__ __volatile__ ("dctqpq  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_dctfixq (void)
{
   __asm__ __volatile__ ("dctfixq  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_drdpq (void)
{
   __asm__ __volatile__ ("drdpq  %0, %1" : "=f" (f18) : "f" (f14));
}

static void _test_dcffixq (void)
{
   __asm__ __volatile__ ("dcffixq  %0, %1" : "=f" (f18) : "f" (f14));
}

typedef void (*test_func_t)();
typedef void (*test_func_main_t)(int);
typedef void (*test_func_shift_t)(int);
typedef struct test_table
{
   test_func_main_t test_category;
   char * name;
} test_table_t;

static unsigned long long dfp128_vals[] = {
                                           // Some finite numbers
                                           0x2207c00000000000ULL, 0x0000000000000e50ULL,
                                           0x2f07c00000000000ULL, 0x000000000014c000ULL,  //large number
                                           0xa207c00000000000ULL, 0x00000000000000e0ULL,
                                           0x2206c00000000000ULL, 0x00000000000000cfULL,
                                           0xa205c00000000000ULL, 0x000000010a395bcfULL,
                                           0x6209400000fd0000ULL, 0x00253f1f534acdd4ULL, // a small number
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

static unsigned long long int64_vals[] = {
                                          // I64 values
                                          0x0ULL,                // zero
                                          0x1ULL,                // one
                                          0xffffffffffffffffULL, // minus one
                                          0x2386f26fc0ffffULL,   // 9999999999999999
                                          0xffdc790d903f0001ULL, // -9999999999999999
                                          0x462d53c8abac0ULL,    // 1234567890124567
                                          0xfffb9d2ac3754540ULL, // -1234567890124567
};

static unsigned long long dfp64_vals[] = {
                                          // various finite numbers
                                          0x2234000000000e50ULL,
                                          0x223400000014c000ULL,
                                          0xa2340000000000e0ULL,// negative
                                          0x22240000000000cfULL,
                                          0xa21400010a395bcfULL,// negative
                                          0x6e4d3f1f534acdd4ULL,// large number
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


/* Index pairs from dfp64_vals or dfp128_vals array to be used with 
 * dfp_two_arg_tests */
static dfp_test_args_t int64_args_x1[] = {
  /*                        {int64 input val, unused } */
                                          {0, 0},
                                          {1, 0},
                                          {2, 0},
                                          {3, 0},
                                          {4, 0},
                                          {5, 0},
                                          {6, 0},
};

static dfp_test_args_t dfp_2args_x1[] = {
  /*                               {dfp_arg, shift_arg} */
                                         {0, SH_0},
                                         {0, SH_1},
                                         {0, SH_2},
                                         {0, SH_3},
                                         {5, SH_0},
                                         {5, SH_1},
                                         {5, SH_2},
                                         {5, SH_3},
                                         {6, SH_0},
                                         {6, SH_1},
                                         {6, SH_2},
                                         {6, SH_3},
                                         {7, SH_0},
                                         {7, SH_1},
                                         {7, SH_2},
                                         {7, SH_3},
                                         {10, SH_0},
                                         {10, SH_1},
                                         {10, SH_2},
                                         {10, SH_3},
                                         {13, SH_0},
                                         {13, SH_1},
                                         {13, SH_2},
                                         {13, SH_3},
};

/* Index pairs from dfp64_vals array to be used with dfp_one_arg_tests */
static dfp_test_args_t dfp_1args_x1[] = {
  /*                               {dfp_arg, unused} */
                                         {0, 0},
                                         {1, 0},
                                         {2, 0},
                                         {3, 0},
                                         {4, 0},
                                         {5, 0},
                                         {6, 0},
                                         {7, 0},
                                         {8, 0},
                                         {9, 0},
                                         {10, 0},
                                         {11, 0},
                                         {12, 0},
                                         {13, 0},
                                         {14, 0},
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

/* The dcffix and dcffixq tests are a little different in that they both take
 * an I64 input.  
 */
static dfp_test_t
dfp_dcffix_dcffixq_tests[] = {
                              { &_test_dcffixq,"dcffixq", int64_args_x1, 7, QUAD_TEST, "I64S->D128", True},
                              /* Power 7 instruction */
                              { &_test_dcffix, "dcffix",  int64_args_x1, 7, LONG_TEST, "I64S->D64", True},
                              { NULL, NULL, NULL, 0, 0, NULL}
};

static dfp_test_t
dfp_one_arg_tests[] = {
                       { &_test_dctdp,  "dctdp",   dfp_1args_x1, 15, LONG_TEST, "D32->D64", True},
                       { &_test_drsp,   "drsp",    dfp_1args_x1, 15, LONG_TEST, "D64->D32", True},
                       { &_test_dctfix, "dctfix",  dfp_1args_x1, 15, LONG_TEST, "D64->I64S", True},
                       { &_test_dctqpq, "dctqpq",  dfp_1args_x1, 15, QUAD_TEST, "D64->D128", True},
                       { &_test_dctfixq,"dctfixq", dfp_1args_x1, 15, QUAD_TEST, "D128->I64S", True},
                       { &_test_drdpq,  "drdpq",   dfp_1args_x1, 15, QUAD_TEST, "D128->D64", True},
                       { NULL, NULL, NULL, 0, 0, NULL}
};


static dfp_test_t
dfp_two_arg_tests[] = {
                       { &_test_dscri,  "dscri",   dfp_2args_x1, 20, LONG_TEST, ">>", True},
                       { &_test_dscli,  "dscli",   dfp_2args_x1, 20, LONG_TEST, "<<", True},
                       { &_test_dscriq, "dscriq",  dfp_2args_x1, 20, QUAD_TEST, ">>", True},
                       { &_test_dscliq, "dscliq",  dfp_2args_x1, 20, QUAD_TEST, "<<", True},
                       { NULL, NULL, NULL, 0, 0, NULL}
};

void set_rounding_mode(unsigned long long rnd_mode)
{
   double fpscr;
   unsigned long long * hex_fpscr = (unsigned long long *)&fpscr;

   *hex_fpscr = 0ULL;
   __asm__ __volatile__ ("mffs %0"  : "=f"(f14));
   fpscr = f14;
   *hex_fpscr &= 0xFFFFFFF0FFFFFFFFULL;
   *hex_fpscr |= (rnd_mode << 32);
   f14 = fpscr;
   SET_FPSCR_DRN;
}

static void test_dfp_one_arg_ops(int unused)
{
   test_func_t func;
   unsigned long long u0, u0x;
   double res, d0, *d0p;
   double d0x, *d0xp;
   unsigned long round_mode;
   int k = 0;

   u0x = 0;
   d0p = &d0;
   d0xp = &d0x;

   while ((func = dfp_one_arg_tests[k].test_func)) {
      int i;

      for (round_mode = 0; round_mode < NUM_RND_MODES; round_mode++) {
         /* Do each test with each of the possible rounding modes */
         dfp_test_t test_group = dfp_one_arg_tests[k];

         printf("\ntest with rounding mode %lu \n", round_mode);
         /* The set_rounding_mode() uses the global value f14. Call the
          * function before setting up the test for the specific instruction
          * to avoid avoid conflicts using f14.
          */
         set_rounding_mode(round_mode);

         for (i = 0; i < test_group.num_tests; i++) {

            if (test_group.precision == LONG_TEST) {
               u0 = dfp64_vals[test_group.targs[i].fra_idx];
            } else {
               u0 = dfp128_vals[test_group.targs[i].fra_idx * 2];
               u0x = dfp128_vals[(test_group.targs[i].fra_idx * 2) + 1];
            }

            *(unsigned long long *)d0p = u0;
            f14 = d0;
            if (test_group.precision == QUAD_TEST) {
	       *(unsigned long long *)d0xp = u0x;
                f15 = d0x;
            }

            (*func)();
            res = f18;

            printf("%s %016llx", test_group.name, u0);

            if (test_group.precision == LONG_TEST) {
               printf(" %s  => %016llx",
                      test_group.op, *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf(" %016llx %s ==> %016llx %016llx",
                      u0x, test_group.op,
                      *((unsigned long long *)(&res)),
                      *((unsigned long long *)(&resx)));
            }
            printf("\n");
         }
      }

      k++;
      printf( "\n" );
   }
}

static void test_dfp_two_arg_ops(int unused)
/* Shift instructions: first argument is the DFP source, second argument
 * is 6 bit shift amount.
 */
{
   test_func_shift_t func;
   unsigned long long u0, u0x;
   unsigned int shift_by;
   double res, d0, *d0p;
   double d0x, *d0xp;
   unsigned long round_mode;
   int k = 0;

   u0x = 0;
   d0p = &d0;
   d0xp = &d0x;

   while ((func = dfp_two_arg_tests[k].test_func)) {
      int i;

      for (round_mode = 0; round_mode < NUM_RND_MODES; round_mode++) {
         /* Do each test with each of the possible rounding modes */
         dfp_test_t test_group = dfp_two_arg_tests[k];

         printf("\ntest with rounding mode %lu \n", round_mode);

         /* The set_rounding_mode() uses the global value f14. Call the
          * function before setting up the test for the specific instruction
          * to avoid avoid conflicts using f14.
          */
         set_rounding_mode(round_mode);

         for (i = 0; i < test_group.num_tests; i++) {

            shift_by = test_group.targs[i].frb_idx;

            if (test_group.precision == LONG_TEST) {
               u0 = dfp64_vals[test_group.targs[i].fra_idx];
            } else {
               u0 = dfp128_vals[test_group.targs[i].fra_idx * 2];
               u0x = dfp128_vals[(test_group.targs[i].fra_idx * 2) + 1];
            }

            *(unsigned long long *)d0p = u0;
            f14 = d0;
            if (test_group.precision == QUAD_TEST) {
               *(unsigned long long *)d0xp = u0x;
               f15 = d0x;
            }

            (*func)(shift_by);
            res = f18;

            printf("%s %016llx", test_group.name, u0);

            if (test_group.precision) {
               printf(" %s %-3d => %016llx",
                      test_group.op, shift_by, *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf(" %016llx %s %-3d  ==> %016llx %016llx",
                      u0x, test_group.op, shift_by,
                      *((unsigned long long *)(&res)),
                      *((unsigned long long *)(&resx)));
            }
            printf("\n" );
         }
      }

      k++;
      printf( "\n" );
   }
}

static void test_dcffix_dcffixq(int has_vsx)
{
   test_func_t func;
   unsigned long long u0;
   double res, d0, *d0p;
   int k = 0, round_mode;

   d0p = &d0;


   while ((func = dfp_dcffix_dcffixq_tests[k].test_func)) {
      int i;

      if ((!has_vsx) && (!strcmp("dcffix", dfp_dcffix_dcffixq_tests[k].name))) {
         k++;
         /* The test instruction is dcffix it is supported on POWER 7
          * and newer processors.  Skip if not POWER 7 or newer.
          */
         continue;
      }

      for (round_mode = 0; round_mode < NUM_RND_MODES; round_mode++) {
         /* Do each test with each of the possible rounding modes */
         dfp_test_t test_group = dfp_dcffix_dcffixq_tests[k];

         printf("\ntest with rounding mode %u \n", round_mode);

         /* The set_rounding_mode() uses the global value f14. Call the
          * function before setting up the test for the specific instruction
          * to avoid avoid conflicts using f14.
          */
         set_rounding_mode(round_mode); 

         for (i = 0; i < test_group.num_tests; i++) {

            /* The instructions take I64 inputs */
            u0 = int64_vals[test_group.targs[i].fra_idx];

            *(unsigned long long *)d0p = u0;
            f14 = d0;

            (*func)();
            res = f18;

            printf("%s %016llx", test_group.name, u0);

            if (test_group.precision) {
               printf(" %s  => %016llx",
                      test_group.op, *((unsigned long long *)(&res)));
            } else {
               double resx = f19;
               printf(" %s ==> %016llx %016llx",
                      test_group.op,
                      *((unsigned long long *)(&res)),
                      *((unsigned long long *)(&resx)));
            }
            printf("\n" );
         }
      }

      k++;
      printf( "\n" );
   }
}

static test_table_t
all_tests[] =
{
   { &test_dfp_one_arg_ops,
   "Test DFP fomat conversion instructions" },
   { &test_dfp_two_arg_ops,
   "Test DFP shift instructions" },
   { test_dcffix_dcffixq,
   "Test DCFFIX and DCFFIXQ instructions" },
   { NULL, NULL }
};
#endif // HAS_DFP

int main(int argc, char ** argv, char ** envp) {
#if defined(HAS_DFP)
   test_table_t aTest;
   test_func_t func;
   int i = 0, has_vsx;

   /* If the processor has the VSX functionality then it is POWER 7
    * or newer.
    */
   my_envp = envp;
   has_vsx = get_vsx();  

   while ((func = all_tests[i].test_category)) {
      aTest = all_tests[i];
      printf( "%s\n", aTest.name );
      (*func)(has_vsx);
      i++;
   }

#else
   printf("HAS_DFP not detected.\n");
#endif // HAS_DFP
   return 0;
}
