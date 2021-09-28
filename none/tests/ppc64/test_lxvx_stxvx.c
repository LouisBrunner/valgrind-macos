/*
 * test_lxvx_stxvx.c:
 *
 * The lxvx and stxvx instructions were extended mnemonics
 * of the lxvd2x and stxvd2x instructions, which are Big-Endian
 * by design in ISA 2.07 and earlier.
 * Beginning with ISA 3.0 these are unique instructions and
 * are endian aware.
 * Tests of those instructions must be aware of which
 * ISA level the code is being compiled to.
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

#include <stdio.h>
#include <stdint.h>

#ifdef HAS_ISA_2_07
#include <assert.h>
#include <ctype.h>     // isspace
#include <stdlib.h>
#include <string.h>
#include <unistd.h>    // getopt
#include <altivec.h>   // vector
#include <malloc.h>    // memalign

#undef DEBUG_VECTOR_PERMUTE
static int verbose = 0;

typedef uint64_t  HWord_t;

/* Define a small memory range used to test load-from and store-to vsx */
#define BUFFER_SIZE 4
#define MAX_BUFFER_PATTERNS 6
unsigned long buffer[BUFFER_SIZE];

static void dump_small_buffer(void) {
   int x;

   printf("[ ");

   for (x = 0; x < BUFFER_SIZE; x++)
      printf("%016lx ", buffer[x] );

   printf("]");
}

static void initialize_buffer(int t)
{
   int x;

   for (x = 0; x < BUFFER_SIZE; x++)
      /* Don't want each of the 32-bit chunks to be identical. Loads of a
       * byte from the wrong 32-bit chuck are not detectable if the chunks
       * are identical.
       */
      switch((t+x)%BUFFER_SIZE) {
      case 0:
         buffer[x] = 0xffffffffffffffff;
         break;
      case 1:
         buffer[x] = 0x0001020304050607;
         break;
      case 2:
         buffer[x] = 0x5555555555555555;
         break;
      case 3:
         buffer[x] = 0x0000000000000000;
         break;
      case 4:
         buffer[x] = 0x5a05a05a05a05a05;
         break;
      case 5:
         buffer[x] = 0x0102030405060708;
         break;
      default:
         buffer[x] = 0x1010101010101010;
         break;
   }
}

#define ALLCR "cr0","cr1","cr2","cr3","cr4","cr5","cr6","cr7"

#define SET_CR(_arg) \
      __asm__ __volatile__ ("mtcr  %0" : : "b"(_arg) : ALLCR );

#define SET_CR_ZERO \
      SET_CR(0)

#define GET_CR(_lval) \
      __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) )

#define GET_XER(_lval) \
      __asm__ __volatile__ ("mfxer %0" : "=b"(_lval) )

#define SET_CR_ZERO \
      SET_CR(0)

/* a table of exponent values for use in the float precision tests. */
unsigned long exponent_table[] = {
#ifdef EXHAUSTIVE_TESTS
  0x0000,   /* +/-0 or +/-DENormalized, depending on associated mantissa. */
  0x1a,     /* within NORmalized for 16,32,64,128-bit.                    */
  0x1f,     /* +/-INF or +/-NaN for 16bit, NORmalized for 32,64,128       */
  0xff,     /* +/-INF or +/-NaN for 32bit, NORmalized for 64,128          */
  0x7ff,    /* +/-INF or +/-NaN for 32 and 64bit, NORmalized for 128      */
  0x7fff,   /* +/-INF or +/-NaN for 128bit.                               */
#else
  0x0000,   /* +/-0 or +/-DENormalized, depending on associated mantissa. */
  0xff,     /* +/-INF or +/-NaN for 32bit, NORmalized for 64,128          */
  0x7ff,    /* +/-INF or +/-NaN for 32 and 64bit, NORmalized for 128      */
  0x7fff,   /* +/-INF or +/-NaN for 128bit.                               */
#endif
};
#define MAX_EXPONENTS  (sizeof(exponent_table) / sizeof(unsigned long))

unsigned long nb_float_vsxargs;

#define MAX_FLOAT_VSX_ARRAY_SIZE (((MAX_EXPONENTS * MAX_MANTISSAS) * 2 + 1) * 2)

static unsigned long * vsxargs = NULL;
unsigned long nb_vargs;

#define MAX_VSX_ARRAY_SIZE 42

static void build_vsx_table (void)
{
   long i = 0;
   // A VSX register is 128-bits wide.
   // We build contents here using pairs of 64-bit longs.
   // Permutes work against two (non-paired) VSX regs, so these are
   //  also grouped by twos.
   vsxargs = memalign(16, MAX_VSX_ARRAY_SIZE * sizeof(unsigned long));
#ifdef EXHAUSTIVE_TESTS
   vsxargs[i++] = 0x0000000000000000UL; vsxargs[i++] = 0x0000000000000000UL;
   vsxargs[i++] = 0x0102030405060708UL; vsxargs[i++] = 0x0102010201020102UL;

   vsxargs[i++] = 0xaaaaaaaaaaaaaaaaUL; vsxargs[i++] = 0xaaaaaaaaaaaaaaaaUL;
   vsxargs[i++] = 0x5555555555555555UL; vsxargs[i++] = 0x5555555555555555UL;

   vsxargs[i++] = 0x08090a0b0c0d0e0fUL; vsxargs[i++] = 0x0102010201020102UL;
   vsxargs[i++] = 0xf0f1f2f3f4f5f6f7UL; vsxargs[i++] = 0xf8f9fafbfcfdfeffUL;

   vsxargs[i++] = 0x7ea1a5a7abadb0baUL; vsxargs[i++] = 0x070d111d1e555e70UL;
   vsxargs[i++] = 0xe5e7ecedeff0f1faUL; vsxargs[i++] = 0xbeb1c0caced0dbdeUL;

   vsxargs[i++] = 0x00115e7eadbabec0UL; vsxargs[i++] = 0xced0deede5ecef00UL;
   vsxargs[i++] = 0x00111e7ea5abadb1UL; vsxargs[i++] = 0xbecad0deedeffe00UL;

   vsxargs[i++] = 0x0011223344556677UL; vsxargs[i++] = 0x8899aabbccddeeffUL;
   vsxargs[i++] = 0xf0e0d0c0b0a09080UL; vsxargs[i++] = 0x7060504030201000UL;
#else
   vsxargs[i++] = 0x0000000000000000UL; vsxargs[i++] = 0x0000000000000000UL;
   vsxargs[i++] = 0x0102030405060708UL; vsxargs[i++] = 0x0102010201020102UL;

   vsxargs[i++] = 0x0011223344556677UL; vsxargs[i++] = 0x8899aabbccddeeffUL;
   vsxargs[i++] = 0xf0e0d0c0b0a09080UL; vsxargs[i++] = 0x7060504030201000UL;
#endif

   // these next three groups are specific for vector rotate tests.
   //  bits 11:15,19:23,27:31 of each 32-bit word contain mb,me,sh values.
   vsxargs[i++] = 0x0000100000001002ULL; vsxargs[i++] = 0x0000100800001010ULL;
   vsxargs[i++] = 0x0010100000101002ULL; vsxargs[i++] = 0x0010100800101010ULL;

   // vector rotate special...
   vsxargs[i++] = 0x00001c0000001c02ULL; vsxargs[i++] = 0x00001c0800001c10ULL;
   vsxargs[i++] = 0x00101c0000101c02ULL; vsxargs[i++] = 0x00101c0800101c10ULL;

   // vector rotate special...
   vsxargs[i++] = 0x00001f0000001f02ULL; vsxargs[i++] = 0x00001f0800001f10ULL;
   vsxargs[i++] = 0x00101f0000101f02ULL; vsxargs[i++] = 0x00101f0800101f10ULL;

   nb_vargs = i;
}

#define VERBOSE_FUNCTION_CALLOUT \
   if (verbose) \
      printf("Test Harness Function: %s\n", __FUNCTION__);

/* XXXX these must all be callee-save regs! */
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");
register double  f14 __asm__ ("fr14");
register double  f15 __asm__ ("fr15");

/* globals used for vector tests */
static vector unsigned long vec_xt;

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
//dfp_val_t dfp_value;

/* individual instruction tests */
typedef void (*test_func_t) (void);
struct test_list_t {
   test_func_t func;
   const char *name;
};
typedef struct test_list_t test_list_t;

/* vector splat value */
volatile int x_splat;
//volatile int dfp_significance;

/* groups of instruction tests, calling individual tests */
typedef void (*test_group_t) (const char *name, test_func_t func,
                              unsigned int test_flags);

enum test_flags {
	unused = 0,
   /* Nb arguments */
};

static void test_lxvx(void)     {
  __asm__ __volatile__ ("lxvx %x0, 14, 15" : "=wa" (vec_xt));
}


static void test_stxvx(void)    {
  __asm__ __volatile__ ("stxvx %x0, 14, 15" :: "wa" (vec_xt));
}

static test_list_t testgroup_vector_loadstore[] = {
   { &test_lxvx    , "lxvx"     },
   { &test_stxvx   , "stxvx"    },
   { NULL          , NULL       },
};

/* table containing all of the instruction groups */
struct test_group_table_t {
   test_list_t *tests;
   const char *name;
   unsigned int flags;
};

typedef struct test_group_table_t test_group_table_t;

static test_group_table_t all_tests[] = {
   {
      testgroup_vector_loadstore,
      "ppc vector load/store",
      0,
   },
   { NULL,                   NULL,               0x00000000, },
};

static void testfunction_vector_loadstore (const char* instruction_name,
                                           test_func_t test_function,
                                           unsigned int ignore_flags) {
   /* exercises vector loads from memory, and vector stores from memory.
    * <load or store instruction>  XS, RA, RB
    * For these tests, RA will be zero.
    * EA is then, simply, RB.  */
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

         printf("\t\t\t\t%016lx %016lx ", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
         dump_small_buffer();
         printf("\n");
      }
   }
}

typedef struct insn_sel_flags_t_struct {
   unsigned int cr;
} insn_sel_flags_t;

static void do_tests ( insn_sel_flags_t seln_flags)
{
	test_group_t group_function;
	int n = 0;
	int j = 0;
	int i = 0;

	test_list_t *tests;

	printf("%s:\n", all_tests[i].name);
	group_function = &testfunction_vector_loadstore;
	tests = all_tests[i].tests;

		for (j = 0; tests[j].name != NULL; j++) {
			(*group_function)(tests[j].name, tests[j].func, 0);
			printf("\n");
			n++;
		}
	printf("\n");
	n++;

	if (verbose) printf("\n");

	printf("All done. Tested %d different instructions\n", n);
}
#endif

int main (int argc, char **argv)
{
#ifdef HAS_ISA_2_07
   insn_sel_flags_t flags;

   build_vsx_table();

   do_tests( flags );
#else
   printf("HAS_ISA_2_07 not detected.\n");
#endif
   return 0;
}

