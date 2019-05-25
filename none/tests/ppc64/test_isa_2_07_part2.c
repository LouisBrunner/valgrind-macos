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
 along with this program; if not, see <http://www.gnu.org/licenses/>.

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

#ifdef VGP_ppc64le_linux
#define isLE 1
#else
#define isLE 0
#endif

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
} fp_test_args_t;

static int nb_special_fargs;
static double * spec_fargs;
static float * spec_sp_fargs;

static void build_special_fargs_table(void)
{
   /*
    * Double precision:
    * Sign goes from zero to one               (1 bit)
    * Exponent goes from 0 to ((1 << 12) - 1)  (11 bits)
    * Mantissa goes from 1 to ((1 << 52) - 1)  (52 bits)
    * + special values:
    * +0.0      : 0 0x000 0x0000000000000 => 0x0000000000000000
    * -0.0      : 1 0x000 0x0000000000000 => 0x8000000000000000
    * +infinity : 0 0x7FF 0x0000000000000 => 0x7FF0000000000000
    * -infinity : 1 0x7FF 0x0000000000000 => 0xFFF0000000000000
    * +SNaN     : 0 0x7FF 0x7FFFFFFFFFFFF => 0x7FF7FFFFFFFFFFFF
    * -SNaN     : 1 0x7FF 0x7FFFFFFFFFFFF => 0xFFF7FFFFFFFFFFFF
    * +QNaN     : 0 0x7FF 0x8000000000000 => 0x7FF8000000000000
    * -QNaN     : 1 0x7FF 0x8000000000000 => 0xFFF8000000000000
    * (8 values)
    *
    * Single precision
    * Sign:     1 bit
    * Exponent: 8 bits
    * Mantissa: 23 bits
    * +0.0      : 0 0x00 0x000000 => 0x00000000
    * -0.0      : 1 0x00 0x000000 => 0x80000000
    * +infinity : 0 0xFF 0x000000 => 0x7F800000
    * -infinity : 1 0xFF 0x000000 => 0xFF800000
    * +SNaN     : 0 0xFF 0x3FFFFF => 0x7FBFFFFF
    * -SNaN     : 1 0xFF 0x3FFFFF => 0xFFBFFFFF
    * +QNaN     : 0 0xFF 0x400000 => 0x7FC00000
    * -QNaN     : 1 0xFF 0x400000 => 0xFFC00000
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

static unsigned int vstg[] __attribute__ ((aligned (16))) = { 0, 0, 0,0,
                                                              0, 0, 0, 0 };


static unsigned int viargs[] __attribute__ ((aligned (16))) = { 0x80000001,
                                                                0x89abcdef,
                                                                0x00112233,
                                                                0x74556677,
                                                                0x00001abb,
                                                                0x00000001,
                                                                0x31929394,
                                                                0xa1a2a3a4,
};
#define NUM_VIARGS_INTS (sizeof viargs/sizeof viargs[0])
#define NUM_VIARGS_VECS  (NUM_VIARGS_INTS/4)


static unsigned long long vdargs[] __attribute__ ((aligned (16))) = {
                                                                     0x0102030405060708ULL,
                                                                     0x090A0B0C0E0D0E0FULL,
                                                                     0xF1F2F3F4F5F6F7F8ULL,
                                                                     0xF9FAFBFCFEFDFEFFULL
};
#define NUM_VDARGS_INTS (sizeof vdargs/sizeof vdargs[0])
#define NUM_VDARGS_VECS  (NUM_VDARGS_INTS/2)

typedef void (*test_func_t)(void);

struct test_table
{
   test_func_t test_category;
   char * name;
};


typedef enum {
   SINGLE_TEST,
   SINGLE_TEST_SINGLE_RES,
   DOUBLE_TEST,
   DOUBLE_TEST_SINGLE_RES
} precision_type_t;
#define IS_DP_RESULT(x) ((x == SINGLE_TEST) || (x == DOUBLE_TEST))

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
   VX_SCALAR_SP_TO_VECTOR_SP,
   VX_DEFAULT
} vx_fp_test_type;

typedef enum {
   VSX_LOAD = 1,
   VSX_LOAD_SPLAT,
   VSX_STORE,
} vsx_ldst_type;

typedef enum {
   VSX_AND = 1,
   VSX_NAND,
   VSX_ANDC,
   VSX_OR,
   VSX_ORC,
   VSX_NOR,
   VSX_XOR,
   VSX_EQV,
} vsx_log_op;

struct vx_fp_test1
{
   test_func_t test_func;
   const char *name;
   fp_test_args_t * targs;
   int num_tests;
    vx_fp_test_type test_type;
 };

struct ldst_test
{
   test_func_t test_func;
   const char *name;
   precision_type_t precision;
   void * base_addr;
   uint32_t offset;
   vsx_ldst_type type;
};

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

struct xs_conv_test
{
   test_func_t test_func;
   const char *name;
   int num_tests;
};

struct simple_test
{
   test_func_t test_func;
   const char *name;
};

struct vsx_logic_test
{
   test_func_t test_func;
   const char *name;
   vsx_log_op op;
};

typedef struct vsx_logic_test logic_test_t;
typedef struct ldst_test ldst_test_t;
typedef struct simple_test xs_conv_test_t;
typedef struct vx_fp_test1 vx_fp_test_basic_t;
typedef struct vx_fp_test2 vx_fp_test2_t;
typedef struct test_table test_table_t;


static vector unsigned int vec_out, vec_inA, vec_inB;

static void test_xscvdpspn(void)
{
   __asm__ __volatile__ ("xscvdpspn   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvspdpn(void)
{
   __asm__ __volatile__ ("xscvspdpn  %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static int do_asp;
static void test_xsmadds(void)
{
   if (do_asp)
      __asm__ __volatile__ ("xsmaddasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsmaddmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsmsubs(void)
{
   if (do_asp)
      __asm__ __volatile__ ("xsmsubasp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsmsubmsp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xscvsxdsp (void)
{
   __asm__ __volatile__ ("xscvsxdsp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvuxdsp (void)
{
   __asm__ __volatile__ ("xscvuxdsp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsnmadds(void)
{
   if (do_asp)
      __asm__ __volatile__ ("xsnmaddasp        %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsnmaddmsp        %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsnmsubs(void)
{
   if (do_asp)
      __asm__ __volatile__ ("xsnmsubasp        %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsnmsubmsp        %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_stxsspx(void)
{
   __asm__ __volatile__ ("stxsspx          %x0, %1, %2" : : "wa" (vec_inA), "b" (r14),"r" (r15));
}

static void test_stxsiwx(void)
{
   __asm__ __volatile__ ("stxsiwx          %x0, %1, %2" : : "wa" (vec_inA), "b" (r14),"r" (r15));
}

static void test_lxsiwax(void)
{
   __asm__ __volatile__ ("lxsiwax          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_lxsiwzx(void)
{
   __asm__ __volatile__ ("lxsiwzx          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_lxsspx(void)
{
   __asm__ __volatile__ ("lxsspx          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_xssqrtsp(void)
{
   __asm__ __volatile__ ("xssqrtsp         %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsrsqrtesp(void)
{
   __asm__ __volatile__ ("xsrsqrtesp         %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

/* Three argument instuctions */
static void test_xxleqv(void)
{
   __asm__ __volatile__ ("xxleqv          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxlorc(void)
{
   __asm__ __volatile__ ("xxlorc          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxlnand(void)
{
   __asm__ __volatile__ ("xxlnand         %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsaddsp(void)
{
  __asm__ __volatile__ ("xsaddsp   %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xssubsp(void)
{
  __asm__ __volatile__ ("xssubsp   %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xsdivsp(void)
{
  __asm__ __volatile__ ("xsdivsp   %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA), "wa" (vec_inB));
}

static void test_xsmulsp(void)
{
   __asm__ __volatile__ ("xsmulsp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsresp(void)
{
   __asm__ __volatile__ ("xsresp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}
static void test_xsrsp(void)
{
   __asm__ __volatile__ ("xsrsp   %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

fp_test_args_t vx_math_tests[] = {
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
                                  {10, 11},
                                  {12, 8},
                                  {12, 14},
                                  {12, 6},
                                  {12, 5},
                                  {12, 4},
                                  {12, 7},
                                  {12, 9},
                                  {12, 11},
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
                                  {10, 11},
                                  {12, 8},
                                  {12, 14},
                                  {12, 6},
                                  {12, 5},
                                  {12, 4},
                                  {12, 7},
                                  {12, 9},
                                  {12, 11}
};

// These are all double precision inputs with double word outputs (mostly converted to single precision)
static vx_fp_test_basic_t vx_fp_tests[] = {
                                     { &test_xsmadds, "xsmadd", vx_math_tests, 64, VX_FP_SMAS},
                                     { &test_xsmsubs, "xsmsub", vx_math_tests, 64, VX_FP_SMSS},
                                     { &test_xsmulsp, "xsmulsp", vx_math_tests, 64, VX_FP_OTHER},
                                     { &test_xsdivsp, "xsdivsp", vx_math_tests, 64, VX_FP_OTHER},
                                     { &test_xsnmadds, "xsnmadd", vx_math_tests, 64, VX_FP_SNMAS},
                                     { &test_xsnmsubs, "xsnmsub", vx_math_tests, 64, VX_FP_SNMSS},
                                     { NULL, NULL, NULL, 0, 0 }
};

static vx_fp_test2_t
vsx_one_fp_arg_tests[] = {
                          { &test_xscvdpspn, "xscvdpspn", NULL, 20, DOUBLE_TEST_SINGLE_RES, VX_SCALAR_SP_TO_VECTOR_SP, "conv"},
                          { &test_xscvspdpn, "xscvspdpn", NULL, 20, SINGLE_TEST, VX_DEFAULT, "conv"},
                          { &test_xsresp,    "xsresp", NULL, 20, DOUBLE_TEST, VX_ESTIMATE, "1/x"},
                          { &test_xsrsp,     "xsrsp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "round"},
                          { &test_xsrsqrtesp, "xsrsqrtesp", NULL, 20, DOUBLE_TEST, VX_ESTIMATE, "1/sqrt"},
                          { &test_xssqrtsp, "xssqrtsp", NULL, 20, DOUBLE_TEST, VX_DEFAULT, "sqrt"},
                          { NULL, NULL, NULL, 0, 0, 0, NULL}
};

// These are all double precision inputs with double word outputs (mostly converted to single precision)
static vx_fp_test_basic_t
vx_simple_scalar_fp_tests[] = {
                          { &test_xssubsp, "xssubsp", vx_math_tests, 64, VX_DEFAULT},
                          { &test_xsaddsp, "xsaddsp", vx_math_tests, 64, VX_DEFAULT},
                          { NULL, NULL, NULL, 0 , 0}
};

static ldst_test_t
ldst_tests[] = {
                    { &test_stxsspx, "stxsspx", DOUBLE_TEST_SINGLE_RES, vstg, 0, VSX_STORE },
                    { &test_stxsiwx, "stxsiwx", SINGLE_TEST_SINGLE_RES, vstg, 4, VSX_STORE },
                    { &test_lxsiwax, "lxsiwax", SINGLE_TEST, viargs, 0, VSX_LOAD },
                    { &test_lxsiwzx, "lxsiwzx", SINGLE_TEST, viargs, 4, VSX_LOAD },
                    { &test_lxsspx,  "lxsspx",  SINGLE_TEST, NULL, 0, VSX_LOAD },
                    { NULL, NULL, 0, NULL, 0, 0 } };

static xs_conv_test_t
xs_conv_tests[] = {
                   { &test_xscvsxdsp, "xscvsxdsp"},
                   { &test_xscvuxdsp, "xscvuxdsp"},
                   { NULL, NULL}
};

static logic_test_t
logic_tests[] = {
                 { &test_xxleqv,  "xxleqv", VSX_EQV },
                 { &test_xxlorc,  "xxlorc", VSX_ORC },
                 { &test_xxlnand, "xxlnand", VSX_NAND },
                 { NULL, NULL, 0}
};

Bool check_reciprocal_estimate(Bool is_rsqrte, int idx, int output_vec_idx)
{
   /* NOTE:
    * This function has been verified only with the xsresp and xsrsqrtes instructions.
    *
    * Technically, the number of bits of precision for xsresp and xsrsqrtesp is
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
#define VSX_RECIP_ESTIMATE_MASK_SP 0xFFFF8000


   Bool result = False;
   double src_dp, res_dp;
   float calc_diff = 0;
   float real_diff = 0;
   double recip_divisor;
   float div_result;
   float calc_diff_tmp;

   src_dp = res_dp = 0;
   Bool src_is_negative = False;
   Bool res_is_negative = False;
   unsigned long long * dst_dp = NULL;
   unsigned long long * src_dp_ull;
   dst_dp = (unsigned long long *) &vec_out;
   src_dp = spec_fargs[idx];
   src_dp_ull = (unsigned long long *) &src_dp;
   src_is_negative = (*src_dp_ull & 0x8000000000000000ULL) ? True : False;
   res_is_negative = (dst_dp[output_vec_idx] & 0x8000000000000000ULL) ? True : False;
   memcpy(&res_dp, &dst_dp[output_vec_idx], 8);


   // Below are common rules
   if (isnan(src_dp))
      return isnan(res_dp);
   if (fpclassify(src_dp) == FP_ZERO)
      return isinf(res_dp);
   if (!src_is_negative && isinf(src_dp))
      return !res_is_negative && (fpclassify(res_dp) == FP_ZERO);
   if (is_rsqrte) {
      if (src_is_negative)
         return isnan(res_dp);
   } else {
      if (src_is_negative && isinf(src_dp))
         return res_is_negative && (fpclassify(res_dp) == FP_ZERO);
   }

   if (is_rsqrte)
      recip_divisor = sqrt(src_dp);
   else
      recip_divisor = src_dp;

   /* The instructions handled by this function take a double precision
    * input, perform a reciprocal estimate in double-precision, round
    * the result to single precision and store into the destination
    * register in double precision format.  So, to check the result
    * for accuracy, we use float (single precision) values.
    */
   div_result = 1.0/recip_divisor;
   calc_diff_tmp = recip_divisor * 16384.0;
   if (isnormal(calc_diff_tmp)) {
      calc_diff = fabs(1.0/calc_diff_tmp);
      real_diff = fabs((float)res_dp - div_result);
      result = ( ( res_dp == div_result )
               || ( real_diff <= calc_diff ) );
#if FRES_DEBUG
      unsigned int * dv = (unsigned int *)&div_result;
      unsigned int * rd = (unsigned int *)&real_diff;
      unsigned int * cd = (unsigned int *)&calc_diff;
      printf("\n\t {computed div_result: %08x; real_diff:  %08x; calc_diff:  %08x}\n",
             *dv, *rd, *cd);
#endif

   } else {
      /* Unable to compute theoretical difference, so we fall back to masking out
       * un-precise bits.
       */
      unsigned int * div_result_sp = (unsigned int *)&div_result;
      float res_sp = (float)res_dp;
      unsigned int * dst_sp = (unsigned int *)&res_sp;
#if FRES_DEBUG
      unsigned int * calc_diff_tmp_sp = (unsigned int *)&calc_diff_tmp;
      printf("Unable to compute theoretical difference, so we fall back to masking\n");
      printf("\tcalc_diff_tmp: %08x; div_result: %08x; vector result (sp): %08x\n",
             *calc_diff_tmp_sp, *div_result_sp, *dst_sp);
#endif
      result = (*dst_sp & VSX_RECIP_ESTIMATE_MASK_SP) == (*div_result_sp & VSX_RECIP_ESTIMATE_MASK_SP);
   }
   return result;
}

static void test_vx_fp_ops(void)
{

   test_func_t func;
   int k;
   char * test_name = (char *)malloc(20);
   void  * vecA_void_ptr, * vecB_void_ptr, * vecOut_void_ptr;

   if (isLE) {
      vecA_void_ptr = (void *)&vec_inA + 8;
      vecB_void_ptr = (void *)&vec_inB + 8;
      vecOut_void_ptr = (void *)&vec_out + 8;
   } else {
      vecA_void_ptr = (void *)&vec_inA;
      vecB_void_ptr = (void *)&vec_inB;
      vecOut_void_ptr = (void *)&vec_out;
   }

   k = 0;
   build_special_fargs_table();
   while ((func = vx_fp_tests[k].test_func)) {
      int i, repeat = 0;
      unsigned long long * frap, * frbp, * dst;
      vx_fp_test_basic_t test_group = vx_fp_tests[k];
      vx_fp_test_type test_type = test_group.test_type;

      switch (test_type) {
         case VX_FP_SMAS:
         case VX_FP_SMSS:
         case VX_FP_SNMAS:
         case VX_FP_SNMSS:
            if (test_type == VX_FP_SMAS)
               strcpy(test_name, "xsmadd");
            else if (test_type == VX_FP_SMSS)
               strcpy(test_name, "xsmsub");
            else if (test_type == VX_FP_SNMAS)
               strcpy(test_name, "xsnmadd");
            else
               strcpy(test_name, "xsnmsub");

            if (!repeat) {
               repeat = 1;
               strcat(test_name, "asp");
               do_asp = 1;
            }
            break;
         case VX_FP_OTHER:
            strcpy(test_name, test_group.name);
            break;
         default:
            printf("ERROR:  Invalid VX FP test type %d\n", test_type);
            exit(1);
      }

again:
      for (i = 0; i < test_group.num_tests; i++) {
         unsigned int * inA, * inB, * pv;

         fp_test_args_t aTest = test_group.targs[i];
         inA = (unsigned int *)&spec_fargs[aTest.fra_idx];
         inB = (unsigned int *)&spec_fargs[aTest.frb_idx];
         frap = (unsigned long long *)&spec_fargs[aTest.fra_idx];
         frbp = (unsigned long long *)&spec_fargs[aTest.frb_idx];
         int idx;
         unsigned long long vsr_XT;
         pv = (unsigned int *)&vec_out;

         // Only need to copy one doubleword into each vector's element 0
         memcpy(vecA_void_ptr, inA, 8);
         memcpy(vecB_void_ptr, inB, 8);

         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         if (test_type != VX_FP_OTHER) {
            /* Then we need a third src argument, which is stored in element 0 of
             * VSX[XT] -- i.e., vec_out.  For the xs<ZZZ>mdp cases, VSX[XT] holds
             * src3 and VSX[XB] holds src2; for the xs<ZZZ>adp cases, VSX[XT] holds
             * src2 and VSX[XB] holds src3.  The fp_test_args_t that holds the test
             * data (input args, result) contain only two inputs, so I arbitrarily
             * use spec_fargs elements 4 and 14 (alternating) for the third source
             * argument.  We can use the same input data for a given pair of
             * adp/mdp-type instructions by swapping the src2 and src3 arguments; thus
             * the expected result should be the same.
             */
            int extra_arg_idx;
            if (i % 2)
               extra_arg_idx = 4;
            else
               extra_arg_idx = 14;

            if (repeat) {
               /* We're on the first time through of one of the VX_FP_SMx
                * test types, meaning we're testing a xs<ZZZ>adp case, thus
                * we have to swap inputs as described above:
                *    src2 <= VSX[XT]
                *    src3 <= VSX[XB]
                */
               memcpy(vecOut_void_ptr, inB, 8);  // src2
               memcpy(vecB_void_ptr, &spec_fargs[extra_arg_idx], 8);  //src3
               frbp = (unsigned long long *)&spec_fargs[extra_arg_idx];
            } else {
               // Don't need to init src2, as it's done before the switch()
               memcpy(vecOut_void_ptr, &spec_fargs[extra_arg_idx], 8);  //src3
            }
            memcpy(&vsr_XT, vecOut_void_ptr, 8);
         }

         (*func)();
         dst = (unsigned long long *) &vec_out;
         if (isLE)
            dst++;

         if (test_type == VX_FP_OTHER)
            printf("#%d: %s %016llx %016llx = %016llx\n", i, test_name,
                   *frap, *frbp, *dst);
         else
            printf( "#%d: %s %016llx %016llx %016llx = %016llx\n", i,
                    test_name, vsr_XT, *frap, *frbp, *dst );

      }
      /*
           {
               // Debug code.  Keep this block commented out except when debugging.
               double result, expected;
               memcpy(&result, dst, 8);
               memcpy(&expected, &aTest.dp_bin_result, 8);
               printf( "\tFRA + FRB: %e + %e: Expected = %e; Actual = %e\n",
                       spec_fargs[aTest.fra_idx], spec_fargs[aTest.frb_idx],
                       expected, result );
            }
       */
      printf( "\n" );

      if (repeat) {
         repeat = 0;
         strcat(test_name, "UNKNOWN");
         switch (test_type) {
            case VX_FP_SMAS:
            case VX_FP_SMSS:
            case VX_FP_SNMAS:
            case VX_FP_SNMSS:
               if (test_type == VX_FP_SMAS)
                  strcpy(test_name, "xsmadd");
               else if (test_type == VX_FP_SMSS)
                  strcpy(test_name, "xsmsub");
               else if (test_type == VX_FP_SNMAS)
                  strcpy(test_name, "xsnmadd");
               else
                  strcpy(test_name, "xsnmsub");

               do_asp = 0;
               strcat(test_name, "msp");
               break;
            default:
               break;
         }
         goto again;
      }
      k++;
   }
   printf( "\n" );
   free(test_name);
}


static void test_vsx_one_fp_arg(void)
{
   test_func_t func;
   int k;
   void  * vecB_void_ptr;

   k = 0;
   build_special_fargs_table();

   while ((func = vsx_one_fp_arg_tests[k].test_func)) {
      int idx, i;
      unsigned long long *dst_dp;
      unsigned int * dst_sp;
      vx_fp_test2_t test_group = vsx_one_fp_arg_tests[k];
      /* size of source operands */
      Bool dp  = ((test_group.precision == DOUBLE_TEST) ||
		  (test_group.precision == DOUBLE_TEST_SINGLE_RES)) ? True : False;
      /* size of result */
      Bool dp_res = IS_DP_RESULT(test_group.precision);
      Bool is_sqrt = (strstr(test_group.name, "sqrt")) ? True : False;

      vecB_void_ptr = (void *)&vec_inB;
      if (isLE) {
         vecB_void_ptr += dp? 8 : 12;
      }

      for (i = 0; i < test_group.num_tests; i++) {
         unsigned int * pv;
         void * inB;

         pv = (unsigned int *)&vec_out;
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         if (dp) {
            int vec_out_idx;
            unsigned long long * frB_dp;
            if (isLE)
               vec_out_idx = dp_res ? 1 : 3;
            else
               vec_out_idx = 0;

            if (test_group.test_type == VX_SCALAR_SP_TO_VECTOR_SP) {
               /* Take a single-precision value stored in double word element 0
                * of src in double-precision format and convert to single-
                * precision and store in word element 0 of dst.
                */
               double input = spec_sp_fargs[i];
               memcpy(vecB_void_ptr, (void *)&input, 8);
            } else {
               inB = (void *)&spec_fargs[i];
               // copy double precision FP into input vector element 0
               memcpy(vecB_void_ptr, inB, 8);
            }

            // execute test insn
            (*func)();
            if (dp_res)
               dst_dp = (unsigned long long *) &vec_out;
            else
               dst_sp = (unsigned int *) &vec_out;

            printf("#%d: %s ", i, test_group.name);
            frB_dp = (unsigned long long *)&spec_fargs[i];
            printf("%s(%016llx)", test_group.op, *frB_dp);
            if (test_group.test_type == VX_ESTIMATE)
            {
               Bool res;
               res = check_reciprocal_estimate(is_sqrt, i, vec_out_idx);
               printf(" ==> %s)", res ? "PASS" : "FAIL");
            } else if (dp_res) {
               printf(" = %016llx", dst_dp[vec_out_idx]);
            } else {
               printf(" = %08x", dst_sp[vec_out_idx]);
            }

            printf("\n");
         } else {  // single precision test type
            int vec_out_idx;
            if (isLE)
               vec_out_idx = dp_res ? 1 : 3;
            else
               vec_out_idx = 0;
            // Clear input vector
            pv = (unsigned int *)&vec_inB;
            for (idx = 0; idx < 4; idx++, pv++)
               *pv = 0;
            inB = (void *)&spec_sp_fargs[i];
            // copy single precision FP into input vector element i
            memcpy(vecB_void_ptr, inB, 4);
            // execute test insn
            (*func)();
            if (dp_res)
               dst_dp = (unsigned long long *) &vec_out;
            else
               dst_sp = (unsigned int *) &vec_out;
            // print result
            printf("#%d: %s ", i, test_group.name);
               printf("%s(%08x)", test_group.op, *((unsigned int *)&spec_sp_fargs[i]));
               if (dp_res)
                     printf(" = %016llx", dst_dp[vec_out_idx]);
               else
                  printf(" = %08x", dst_sp[vec_out_idx]);

            printf("\n");
         }
      }
      k++;
      printf( "\n" );
   }
}

/* This function currently only supports two double precision input arguments. */
static void test_vsx_two_fp_arg(void)
{
   test_func_t func;
   int k = 0;
   void  * vecA_void_ptr, * vecB_void_ptr;

   if (isLE) {
      vecA_void_ptr = (void *)&vec_inA + 8;
      vecB_void_ptr = (void *)&vec_inB + 8;
   } else {
      vecA_void_ptr = (void *)&vec_inA;
      vecB_void_ptr = (void *)&vec_inB;
   }

   build_special_fargs_table();
   while ((func = vx_simple_scalar_fp_tests[k].test_func)) {
      unsigned long long * frap, * frbp, * dst;
      unsigned int * pv;
      int idx;
      vx_fp_test_basic_t test_group = vx_simple_scalar_fp_tests[k];
      pv = (unsigned int *)&vec_out;
      // clear vec_out
      for (idx = 0; idx < 4; idx++, pv++)
         *pv = 0;

      void * inA, * inB;
      int i;
      for (i = 0; i < test_group.num_tests; i++) {
         fp_test_args_t aTest = test_group.targs[i];
         inA = (void *)&spec_fargs[aTest.fra_idx];
         inB = (void *)&spec_fargs[aTest.frb_idx];
         frap = (unsigned long long *)&spec_fargs[aTest.fra_idx];
         frbp = (unsigned long long *)&spec_fargs[aTest.frb_idx];
         // Only need to copy one doubleword into each vector's element 0
         memcpy(vecA_void_ptr, inA, 8);
         memcpy(vecB_void_ptr, inB, 8);
         (*func)();
         dst = (unsigned long long *) &vec_out;
         if (isLE)
            dst++;
         printf("#%d: %s %016llx,%016llx => %016llx\n", i, test_group.name,
                *frap, *frbp, *dst);
      }
      printf( "\n" );
      k++;
   }
}

/* This function handles the following cases:
 *   1) Single precision value stored in double-precision
 *      floating-point format in doubleword element 0 of src VSX register
 *   2) Integer word value stored in word element 1 of src VSX register
 */
static void _do_store_test (ldst_test_t storeTest)
{
   test_func_t func;
   unsigned int *dst32;
   unsigned int i, idx;
   unsigned int * pv = (unsigned int *) storeTest.base_addr;
   void  * vecA_void_ptr;

   if (isLE) {
      if (storeTest.precision == SINGLE_TEST_SINGLE_RES)
         vecA_void_ptr = (void *)&vec_inA + 8;
   } else {
      if (storeTest.precision == SINGLE_TEST_SINGLE_RES)
         vecA_void_ptr = (void *)&vec_inA + 4;
      else
         vecA_void_ptr = (void *)&vec_inA;
   }

   func = storeTest.test_func;
   r14 = (HWord_t) storeTest.base_addr;
   r15 = (HWord_t) storeTest.offset;

   /* test some of the pre-defined single precision values */
   for (i = 0; i < nb_special_fargs; i+=3) {
      // clear out storage destination
      for (idx = 0; idx < 4; idx++)
         *(pv + idx) = 0;

      printf( "%s:", storeTest.name );
      if (storeTest.precision == SINGLE_TEST_SINGLE_RES)
      {
         unsigned int * arg_ptr = (unsigned int *)&spec_sp_fargs[i];
         memcpy(vecA_void_ptr, arg_ptr, sizeof(unsigned int));
         printf(" %08x ==> ", *arg_ptr);
      } else {
         unsigned long long * dp;
         double input = spec_sp_fargs[i];
         dp = (unsigned long long *)&input;
         memcpy(vecA_void_ptr, dp, sizeof(unsigned long long));
         printf(" %016llx ==> ", *dp);
      }

      // execute test insn
      (*func)();
      dst32 = (unsigned int*)(storeTest.base_addr);
      dst32 += (storeTest.offset/sizeof(int));
      printf( "%08x\n", *dst32);
   }

   printf("\n");
}

static void _do_load_test(ldst_test_t loadTest)
{
   test_func_t func;
   unsigned int i;
   unsigned long long * dst_dp;

   func = loadTest.test_func;
   r15 = (HWord_t) loadTest.offset;

   if (loadTest.base_addr == NULL) {
      /* Test lxsspx: source is single precision value, so let's */
      /* test some of the pre-defined single precision values. */
      int num_loops = (loadTest.offset == 0) ?  nb_special_fargs : (nb_special_fargs - (loadTest.offset/sizeof(int)));
      for (i = 0; i < num_loops; i+=3) {
         unsigned int * sp = (unsigned int *)&spec_sp_fargs[i + (loadTest.offset/sizeof(int))];
         printf( "%s:", loadTest.name );
         printf(" %08x ==> ", *sp);
         r14 = (HWord_t)&spec_sp_fargs[i];

         // execute test insn
         (*func)();
         dst_dp = (unsigned long long *) &vec_out;
         if (isLE)
            dst_dp++;
         printf("%016llx\n", *dst_dp);
      }
   } else {
      // source is an integer word
      int num_loops = (loadTest.offset == 0) ?  NUM_VIARGS_INTS : (NUM_VIARGS_INTS - (loadTest.offset/sizeof(int)));
      for (i = 0; i < num_loops; i++) {
         printf( "%s:", loadTest.name );
         r14 = (HWord_t)&viargs[i];
         printf(" %08x ==> ", viargs[i + (loadTest.offset/sizeof(int))]);

         // execute test insn
         (*func)();
         dst_dp = (unsigned long long *) &vec_out;
         if (isLE)
            dst_dp++;
         printf("%016llx\n", *dst_dp);
      }
   }
   printf("\n");
}

static void test_ldst(void)
{
   int k = 0;

   while (ldst_tests[k].test_func) {
      if (ldst_tests[k].type == VSX_STORE)
         _do_store_test(ldst_tests[k]);
      else {
         _do_load_test(ldst_tests[k]);
      }
      k++;
      printf("\n");
   }
}

static void test_xs_conv_ops(void)
{

   test_func_t func;
   int k = 0;
   void  * vecB_void_ptr;

   if (isLE)
      vecB_void_ptr = (void *)&vec_inB + 8;
   else
      vecB_void_ptr = (void *)&vec_inB;

   build_special_fargs_table();
   while ((func = xs_conv_tests[k].test_func)) {
      int i;
      unsigned long long * dst;
      xs_conv_test_t test_group = xs_conv_tests[k];
      for (i = 0; i < NUM_VDARGS_INTS; i++) {
         unsigned long long  * inB, * pv;
         int idx;
         inB = (unsigned long long *)&vdargs[i];
         memcpy(vecB_void_ptr, inB, 8);
         pv = (unsigned long long *)&vec_out;
         // clear vec_out
         for (idx = 0; idx < 2; idx++, pv++)
            *pv = 0ULL;
         (*func)();
         dst = (unsigned long long *) &vec_out;
         if (isLE)
            dst++;
         printf("#%d: %s %016llx => %016llx\n", i, test_group.name, vdargs[i], *dst);
      }
      k++;
      printf("\n");
   }
   printf( "\n" );
}


static void test_vsx_logic(void)
{
   logic_test_t aTest;
   test_func_t func;
   int k;
   k = 0;

   while ((func = logic_tests[k].test_func)) {

      unsigned int * pv;
      unsigned int * inA, * inB, * dst;
      int idx, i;
      aTest = logic_tests[k];
      for (i = 0; i <= NUM_VIARGS_VECS; i+=4) {
         pv = (unsigned int *)&vec_out;
         inA = &viargs[i];
         inB = &viargs[i];
         memcpy(&vec_inA, inA, sizeof(vector unsigned int));
         memcpy(&vec_inB, inB, sizeof(vector unsigned int));
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         // execute test insn
         (*func)();
         dst = (unsigned int*) &vec_out;

         printf( "#%d: %10s ", k, aTest.name);
         printf( " (%08x %08x %08x %08x, ", inA[0], inA[1], inA[2], inA[3]);
         printf( " %08x %08x %08x %08x)", inB[0], inB[1], inB[2], inB[3]);
         printf(" ==> %08x %08x %08x %08x\n", dst[0], dst[1], dst[2], dst[3]);
      }
      k++;
   }
   printf( "\n" );
}


//----------------------------------------------------------

static test_table_t all_tests[] = {
                                     { &test_vx_fp_ops,
                                       "Test VSX floating point instructions"},
                                     { &test_vsx_one_fp_arg,
                                       "Test VSX vector and scalar single argument instructions"} ,
                                     { &test_vsx_logic,
                                       "Test VSX logic instructions" },
                                     { &test_xs_conv_ops,
                                       "Test VSX scalar integer conversion instructions" },
                                     { &test_ldst,
                                       "Test VSX load/store dp to sp instructions" },
                                     { &test_vsx_two_fp_arg,
                                       "Test VSX vector and scalar two argument instructions"} ,
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
#else
   printf("NO ISA 2.07 SUPPORT\n");
#endif
   return 0;
}
