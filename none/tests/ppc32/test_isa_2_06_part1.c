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
typedef struct ldst_test ldst_test_t;
typedef struct vsx_logic_test logic_test_t;
typedef struct xs_conv_test xs_conv_test_t;
typedef struct p7_fp_test fp_test_t;
typedef struct vx_fp_test vx_fp_test_t;
typedef struct vsx_move_test move_test_t;
typedef struct vsx_permute_test permute_test_t;
typedef struct test_table test_table_t;

static double *fargs = NULL;
static int nb_fargs;

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

static void build_fargs_table(void)
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
 * +QNaN     : 0 0x7FF 0x7FFFFFFFFFFFF => 0x7FF7FFFFFFFFFFFF
 * -QNaN     : 1 0x7FF 0x7FFFFFFFFFFFF => 0xFFF7FFFFFFFFFFFF
 * +SNaN     : 0 0x7FF 0x8000000000000 => 0x7FF8000000000000
 * -SNaN     : 1 0x7FF 0x8000000000000 => 0xFFF8000000000000
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
 * +QNaN     : 0 0xFF 0x3FFFFF => 0x7FBFFFFF
 * -QNaN     : 1 0xFF 0x3FFFFF => 0xFFBFFFFF
 * +SNaN     : 0 0xFF 0x400000 => 0x7FC00000
 * -SNaN     : 1 0xFF 0x400000 => 0xFFC00000
*/
{
   uint64_t mant;
   uint16_t _exp, e1;
   int s;
   int i=0;

   if (nb_fargs)
      return;

   fargs = malloc( 16 * sizeof(double) );
   for (s = 0; s < 2; s++) {
      for (e1 = 0x001;; e1 = ((e1 + 1) << 13) + 7) {
         if (e1 >= 0x400)
            e1 = 0x3fe;
         _exp = e1;
         for (mant = 0x0000000000001ULL; mant < (1ULL << 52);
         /* Add 'random' bits */
         mant = ((mant + 0x4A6) << 29) + 0x359) {
            register_farg( &fargs[i++], s, _exp, mant );
         }
         if (e1 == 0x3fe)
            break;
      }
   }
   // add a few smaller values to fargs . . .
   s = 0;
   _exp = 0x002;
   mant = 0x0000000000b01ULL;
   register_farg(&fargs[i++], s, _exp, mant);

   _exp = 0x000;
   mant = 0x00000203f0b3dULL;
   register_farg(&fargs[i++], s, _exp, mant);

   mant = 0x00000005a203dULL;
   register_farg(&fargs[i++], s, _exp, mant);

   s = 1;
   _exp = 0x002;
   mant = 0x0000000000b01ULL;
   register_farg(&fargs[i++], s, _exp, mant);

   _exp = 0x000;
   mant = 0x00000203f0b3dULL;
   register_farg(&fargs[i++], s, _exp, mant);

   nb_fargs = i;
}


typedef struct ftdiv_test {
   int fra_idx;
   int frb_idx;
   int cr_flags;
} ftdiv_test_args_t;

typedef struct fp_test_args {
   int fra_idx;
   int frb_idx;
   int cr_flags;
   unsigned long long dp_bin_result;
} fp_test_args_t;

unsigned long long xscvuxddp_results[] = {
                                          0x43cfec0000000000ULL,
                                          0x43d013c000000000ULL,
                                          0x4338000000b77501ULL,
                                          0x43dffa0000000001ULL,
                                          0x4372321456990000ULL,
                                          0x0000000000000000ULL,
                                          0x43e0000000000000ULL,
                                          0x43dffc0000000000ULL,
                                          0x43effe0000000000ULL,
                                          0x43dffe0000000000ULL,
                                          0x43efff0000000000ULL,
                                          0x43dffe0000000000ULL,
                                          0x43efff0000000000ULL,
                                          0x43e00106800000f0ULL,
                                          0x43e81a0ca1eb40f6ULL
};

unsigned long long xscvsxddp_results[] = {
                                           0x43cfec0000000000ULL,
                                           0x43d013c000000000ULL,
                                           0x4338000000b77501ULL,
                                           0x43dffa0000000001ULL,
                                           0x4372321456990000ULL,
                                           0x0000000000000000ULL,
                                           0xc3e0000000000000ULL,
                                           0x43dffc0000000000ULL,
                                           0xc330000000000000ULL,
                                           0x43dffe0000000000ULL,
                                           0xc320000000000002ULL,
                                           0x43dffe0000000000ULL,
                                           0xc320000000000000ULL,
                                           0xc3dffdf2fffffe20ULL,
                                           0xc3cf97cd7852fc26ULL,
};

unsigned long long xscvdpsxds_results[] = {
                                           0x0000000000000000ULL,
                                           0x000000000000003eULL,
                                           0x0000000000000000ULL,
                                           0x7fffffffffffffffULL,
                                           0x0000000000000000ULL,
                                           0x0000000000000000ULL,
                                           0x0000000000000000ULL,
                                           0x7fffffffffffffffULL,
                                           0x8000000000000000ULL,
                                           0x8000000000000000ULL,
                                           0x8000000000000000ULL,
                                           0x8000000000000000ULL,
                                           0x8000000000000000ULL,
                                           0x0000000000000000ULL,
                                           0xffffffffffffbe6cULL
};

ftdiv_test_args_t ftdiv_tests[] = {
                              {0, 1, 0x8},
                              {9, 1, 0xa},
                              {1, 12, 0xa},
                              {0, 2, 0xa},
                              {1, 3, 0xa},
                              {3, 0, 0xa},
                              {0, 3, 0xa},
                              {4, 0, 0xa},
                              {7, 1, 0xe},
                              {8, 1, 0xe},
                              {1, 7, 0xe},
                              {0, 13, 0xe},
                              {5, 5, 0xe},
                              {5, 6, 0xe},
};

fp_test_args_t xscmpX_tests[] = {
                                   {8, 8, 0x2, 0ULL},
                                   {8, 14, 0x8, 0ULL},
                                   {8, 6, 0x8, 0ULL},
                                   {8, 5, 0x8, 0ULL},
                                   {8, 4, 0x8, 0ULL},
                                   {8, 7, 0x8, 0ULL},
                                   {8, 9, 0x1, 0ULL},
                                   {8, 11, 0x1, 0ULL},
                                   {14, 8, 0x4, 0ULL},
                                   {14, 14, 0x2, 0ULL},
                                   {14, 6, 0x8, 0ULL},
                                   {14, 5, 0x8, 0ULL},
                                   {14, 4, 0x8, 0ULL},
                                   {14, 7, 0x8, 0ULL},
                                   {14, 9, 0x1, 0ULL},
                                   {14, 11, 0x1, 0ULL},
                                   {6, 8, 0x4, 0ULL},
                                   {6, 14, 0x4, 0ULL},
                                   {6, 6, 0x2, 0ULL},
                                   {6, 5, 0x2, 0ULL},
                                   {6, 4, 0x8, 0ULL},
                                   {6, 7, 0x8, 0ULL},
                                   {6, 9, 0x1, 0ULL},
                                   {6, 11, 0x1, 0ULL},
                                   {5, 8, 0x4, 0ULL},
                                   {5, 14, 0x4, 0ULL},
                                   {5, 6, 0x2, 0ULL},
                                   {5, 5, 0x2, 0ULL},
                                   {5, 4, 0x8, 0ULL},
                                   {5, 7, 0x8, 0ULL},
                                   {5, 9, 0x1, 0ULL},
                                   {5, 11, 0x1, 0ULL},
                                   {4, 8, 0x4, 0ULL},
                                   {4, 14, 0x4, 0ULL},
                                   {4, 6, 0x4, 0ULL},
                                   {4, 5, 0x4, 0ULL},
                                   {4, 1, 0x8, 0ULL},
                                   {4, 7, 0x8, 0ULL},
                                   {4, 9, 0x1, 0ULL},
                                   {4, 11, 0x1, 0ULL},
                                   {7, 8, 0x4, 0ULL},
                                   {7, 14, 0x4, 0ULL},
                                   {7, 6, 0x4, 0ULL},
                                   {7, 5, 0x4, 0ULL},
                                   {7, 4, 0x4, 0ULL},
                                   {7, 7, 0x2, 0ULL},
                                   {7, 9, 0x1, 0ULL},
                                   {7, 11, 0x1, 0ULL},
                                   {10, 8, 0x1, 0ULL},
                                   {10, 14, 0x1, 0ULL},
                                   {10, 6, 0x1, 0ULL},
                                   {10, 5, 0x1, 0ULL},
                                   {10, 4, 0x1, 0ULL},
                                   {10, 7, 0x1, 0ULL},
                                   {10, 9, 0x1, 0ULL},
                                   {10, 11, 0x1, 0ULL},
                                   {12, 8, 0x1, 0ULL},
                                   {12, 14, 0x1, 0ULL},
                                   {12, 6, 0x1, 0ULL},
                                   {12, 5, 0x1, 0ULL},
                                   {12, 4, 0x1, 0ULL},
                                   {12, 7, 0x1, 0ULL},
                                   {12, 9, 0x1, 0ULL},
                                   {12, 11, 0x1, 0ULL},
};

fp_test_args_t xsadddp_tests[] = {
                                   {8, 8, 0x0,   0xfff0000000000000ULL},
                                   {8, 14, 0x0,  0xfff0000000000000ULL},
                                   {8, 6, 0x0,   0xfff0000000000000ULL},
                                   {8, 5, 0x0,   0xfff0000000000000ULL},
                                   {8, 4, 0x0,   0xfff0000000000000ULL},
                                   {8, 7, 0x0,   0x7ff8000000000000ULL},
                                   {8, 9, 0x0,   0x7fffffffffffffffULL},
                                   {8, 11, 0x0,  0x7ff8000000000000ULL},
                                   {14, 8, 0x0,  0xfff0000000000000ULL},
                                   {14, 14, 0x0, 0xc0e0650f5a07b353ULL},
                                   {14, 6, 0x0,  0xc0d0650f5a07b353ULL},
                                   {14, 5, 0x0,  0xc0d0650f5a07b353ULL},
                                   {14, 4, 0x0,  0xc0d0650f5a07b353ULL},
                                   {14, 7, 0x0,  0x7ff0000000000000ULL},
                                   {14, 9, 0x0,  0x7fffffffffffffffULL},
                                   {14, 11, 0x0, 0x7ff8000000000000ULL},
                                   {6, 8, 0x0,   0xfff0000000000000ULL},
                                   {6, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {6, 6, 0x0,   0x8000000000000000ULL},
                                   {6, 5, 0x0,   0x0000000000000000ULL},
                                   {6, 4, 0x0,   0x0123214569900000ULL},
                                   {6, 7, 0x0,   0x7ff0000000000000ULL},
                                   {6, 9, 0x0,   0x7fffffffffffffffULL},
                                   {6, 11, 0x0,  0x7ff8000000000000ULL},
                                   {5, 8, 0x0,   0xfff0000000000000ULL},
                                   {5, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {5, 6, 0x0,   0x0000000000000000ULL},
                                   {5, 5, 0x0,   0x0000000000000000ULL},
                                   {5, 4, 0x0,   0x0123214569900000ULL},
                                   {5, 7, 0x0,   0x7ff0000000000000ULL},
                                   {5, 9, 0x0,   0x7fffffffffffffffULL},
                                   {5, 11, 0x0,  0x7ff8000000000000ULL},
                                   {4, 8, 0x0,   0xfff0000000000000ULL},
                                   {4, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {4, 6, 0x0,   0x0123214569900000ULL},
                                   {4, 5, 0x0,   0x0123214569900000ULL},
                                   {4, 1, 0x0,   0x404f000000000000ULL},
                                   {4, 7, 0x0,   0x7ff0000000000000ULL},
                                   {4, 9, 0x0,   0x7fffffffffffffffULL},
                                   {4, 11, 0x0,  0x7ff8000000000000ULL},
                                   {7, 8, 0x0,   0x7ff8000000000000ULL},
                                   {7, 14, 0x0,  0x7ff0000000000000ULL},
                                   {7, 6, 0x0,   0x7ff0000000000000ULL},
                                   {7, 5, 0x0,   0x7ff0000000000000ULL},
                                   {7, 4, 0x0,   0x7ff0000000000000ULL},
                                   {7, 7, 0x0,   0x7ff0000000000000ULL},
                                   {7, 9, 0x0,   0x7fffffffffffffffULL},
                                   {7, 11, 0x0,  0x7ff8000000000000ULL},
                                   {10, 8, 0x0,  0xffffffffffffffffULL},
                                   {10, 14, 0x0, 0xffffffffffffffffULL},
                                   {10, 6, 0x0,  0xffffffffffffffffULL},
                                   {10, 5, 0x0,  0xffffffffffffffffULL},
                                   {10, 4, 0x0,  0xffffffffffffffffULL},
                                   {10, 7, 0x0,  0xffffffffffffffffULL},
                                   {10, 9, 0x0,  0xffffffffffffffffULL},
                                   {10, 11, 0x0, 0xffffffffffffffffULL},
                                   {12, 8, 0x0,  0xfff8000000000000ULL},
                                   {12, 14, 0x0, 0xfff8000000000000ULL},
                                   {12, 6, 0x0,  0xfff8000000000000ULL},
                                   {12, 5, 0x0,  0xfff8000000000000ULL},
                                   {12, 4, 0x0,  0xfff8000000000000ULL},
                                   {12, 7, 0x0,  0xfff8000000000000ULL},
                                   {12, 9, 0x0,  0xfff8000000000000ULL},
                                   {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xsdivdp_tests[] = {
                                   {8, 8, 0x0,   0x7ff8000000000000ULL},
                                   {8, 14, 0x0,  0x7ff0000000000000ULL},
                                   {8, 6, 0x0,   0x7ff0000000000000ULL},
                                   {8, 5, 0x0,   0xfff0000000000000ULL},
                                   {8, 4, 0x0,   0xfff0000000000000ULL},
                                   {8, 7, 0x0,   0x7ff8000000000000ULL},
                                   {8, 9, 0x0,   0x7fffffffffffffffULL},
                                   {8, 11, 0x0,  0x7ff8000000000000ULL},
                                   {14, 8, 0x0,  0x0000000000000000ULL},
                                   {14, 14, 0x0, 0x3ff0000000000000ULL},
                                   {14, 6, 0x0,  0x7ff0000000000000ULL},
                                   {14, 5, 0x0,  0xfff0000000000000ULL},
                                   {14, 4, 0x0,  0xff9b6cb57ca13c00ULL},
                                   {14, 7, 0x0,  0x8000000000000000ULL},
                                   {14, 9, 0x0,  0x7fffffffffffffffULL},
                                   {14, 11, 0x0, 0x7ff8000000000000ULL},
                                   {6, 8, 0x0,   0x0000000000000000ULL},
                                   {6, 14, 0x0,  0x0000000000000000ULL},
                                   {6, 6, 0x0,   0x7ff8000000000000ULL},
                                   {6, 5, 0x0,   0x7ff8000000000000ULL},
                                   {6, 4, 0x0,   0x8000000000000000ULL},
                                   {6, 7, 0x0,   0x8000000000000000ULL},
                                   {6, 9, 0x0,   0x7fffffffffffffffULL},
                                   {6, 11, 0x0,  0x7ff8000000000000ULL},
                                   {5, 8, 0x0,   0x8000000000000000ULL},
                                   {5, 14, 0x0,  0x8000000000000000ULL},
                                   {5, 6, 0x0,   0x7ff8000000000000ULL},
                                   {5, 5, 0x0,   0x7ff8000000000000ULL},
                                   {5, 4, 0x0,   0x0000000000000000ULL},
                                   {5, 7, 0x0,   0x0000000000000000ULL},
                                   {5, 9, 0x0,   0x7fffffffffffffffULL},
                                   {5, 11, 0x0,  0x7ff8000000000000ULL},
                                   {4, 8, 0x0,   0x8000000000000000ULL},
                                   {4, 14, 0x0,  0x8042ab59d8b6ec87ULL},
                                   {4, 6, 0x0,   0xfff0000000000000ULL},
                                   {4, 5, 0x0,   0x7ff0000000000000ULL},
                                   {4, 1, 0x0,   0x00c3bf3f64b5ad6bULL},
                                   {4, 7, 0x0,   0x0000000000000000ULL},
                                   {4, 9, 0x0,   0x7fffffffffffffffULL},
                                   {4, 11, 0x0,  0x7ff8000000000000ULL},
                                   {7, 8, 0x0,   0x7ff8000000000000ULL},
                                   {7, 14, 0x0,  0xfff0000000000000ULL},
                                   {7, 6, 0x0,   0xfff0000000000000ULL},
                                   {7, 5, 0x0,   0x7ff0000000000000ULL},
                                   {7, 4, 0x0,   0x7ff0000000000000ULL},
                                   {7, 7, 0x0,   0x7ff8000000000000ULL},
                                   {7, 9, 0x0,   0x7fffffffffffffffULL},
                                   {7, 11, 0x0,  0x7ff8000000000000ULL},
                                   {10, 8, 0x0,  0xffffffffffffffffULL},
                                   {10, 14, 0x0, 0xffffffffffffffffULL},
                                   {10, 6, 0x0,  0xffffffffffffffffULL},
                                   {10, 5, 0x0,  0xffffffffffffffffULL},
                                   {10, 4, 0x0,  0xffffffffffffffffULL},
                                   {10, 7, 0x0,  0xffffffffffffffffULL},
                                   {10, 9, 0x0,  0xffffffffffffffffULL},
                                   {10, 11, 0x0, 0xffffffffffffffffULL},
                                   {12, 8, 0x0,  0xfff8000000000000ULL},
                                   {12, 14, 0x0, 0xfff8000000000000ULL},
                                   {12, 6, 0x0,  0xfff8000000000000ULL},
                                   {12, 5, 0x0,  0xfff8000000000000ULL},
                                   {12, 4, 0x0,  0xfff8000000000000ULL},
                                   {12, 7, 0x0,  0xfff8000000000000ULL},
                                   {12, 9, 0x0,  0xfff8000000000000ULL},
                                   {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xsmaddXdp_tests[] = {
                                   {8, 8, 0x0,   0x7ff8000000000000ULL},
                                   {8, 14, 0x0,  0xfff0000000000000ULL},
                                   {8, 6, 0x0,   0x7ff0000000000000ULL},
                                   {8, 5, 0x0,   0xfff0000000000000ULL},
                                   {8, 4, 0x0,   0x7ff0000000000000ULL},
                                   {8, 7, 0x0,   0x7ff8000000000000ULL},
                                   {8, 9, 0x0,   0x7fffffffffffffffULL},
                                   {8, 11, 0x0,  0x7ff8000000000000ULL},
                                   {14, 8, 0x0,  0xfff0000000000000ULL},
                                   {14, 14, 0x0, 0xc0d0650f5a07b353ULL},
                                   {14, 6, 0x0,  0x41b0cc9d05eec2a7ULL},
                                   {14, 5, 0x0,  0x82039a19ca8fcb5fULL},
                                   {14, 4, 0x0,  0x41b0cc9d05eec2a7ULL},
                                   {14, 7, 0x0,  0x7ff0000000000000ULL},
                                   {14, 9, 0x0,  0x7fffffffffffffffULL},
                                   {14, 11, 0x0, 0x7ff8000000000000ULL},
                                   {6, 8, 0x0,   0xfff0000000000000ULL},
                                   {6, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {6, 6, 0x0,   0x0000000000000000ULL},
                                   {6, 5, 0x0,   0x0000000000000000ULL},
                                   {6, 4, 0x0,   0x0123214569900000ULL},
                                   {6, 7, 0x0,   0x7ff0000000000000ULL},
                                   {6, 9, 0x0,   0x7fffffffffffffffULL},
                                   {6, 11, 0x0,  0x7ff8000000000000ULL},
                                   {5, 8, 0x0,   0xfff0000000000000ULL},
                                   {5, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {5, 6, 0x0,   0x8000000000000000ULL},
                                   {5, 5, 0x0,   0x0000000000000000ULL},
                                   {5, 4, 0x0,   0x0123214569900000ULL},
                                   {5, 7, 0x0,   0x7ff0000000000000ULL},
                                   {5, 9, 0x0,   0x7fffffffffffffffULL},
                                   {5, 11, 0x0,  0x7ff8000000000000ULL},
                                   {4, 8, 0x0,   0xfff0000000000000ULL},
                                   {4, 14, 0x0,  0xc0d0650f5a07b353ULL},
                                   {4, 6, 0x0,   0x82039a19ca8fcb5fULL},
                                   {4, 5, 0x0,   0x0000000000000000ULL},
                                   {4, 1, 0x0,   0x404f000000000000ULL},
                                   {4, 7, 0x0,   0x7ff0000000000000ULL},
                                   {4, 9, 0x0,   0x7fffffffffffffffULL},
                                   {4, 11, 0x0,  0x7ff8000000000000ULL},
                                   {7, 8, 0x0,   0xfff0000000000000ULL},
                                   {7, 14, 0x0,  0x7ff0000000000000ULL},
                                   {7, 6, 0x0,   0xfff0000000000000ULL},
                                   {7, 5, 0x0,   0x7ff0000000000000ULL},
                                   {7, 4, 0x0,   0xfff0000000000000ULL},
                                   {7, 7, 0x0,   0x7ff0000000000000ULL},
                                   {7, 9, 0x0,   0x7fffffffffffffffULL},
                                   {7, 11, 0x0,  0x7ff8000000000000ULL},
                                   {10, 8, 0x0,  0xffffffffffffffffULL},
                                   {10, 14, 0x0, 0xffffffffffffffffULL},
                                   {10, 6, 0x0,  0xffffffffffffffffULL},
                                   {10, 5, 0x0,  0xffffffffffffffffULL},
                                   {10, 4, 0x0,  0xffffffffffffffffULL},
                                   {10, 7, 0x0,  0xffffffffffffffffULL},
                                   {10, 9, 0x0,  0xffffffffffffffffULL},
                                   {10, 11, 0x0, 0xffffffffffffffffULL},
                                   {12, 8, 0x0,  0xfff8000000000000ULL},
                                   {12, 14, 0x0, 0xfff8000000000000ULL},
                                   {12, 6, 0x0,  0xfff8000000000000ULL},
                                   {12, 5, 0x0,  0xfff8000000000000ULL},
                                   {12, 4, 0x0,  0xfff8000000000000ULL},
                                   {12, 7, 0x0,  0xfff8000000000000ULL},
                                   {12, 9, 0x0,  0xfff8000000000000ULL},
                                   {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xsmsubXdp_tests[] = {
                                   {8, 8, 0x0,   0x7ff0000000000000ULL},
                                   {8, 14, 0x0,  0xfff0000000000000ULL},
                                   {8, 6, 0x0,   0x7ff0000000000000ULL},
                                   {8, 5, 0x0,   0xfff0000000000000ULL},
                                   {8, 4, 0x0,   0x7ff0000000000000ULL},
                                   {8, 7, 0x0,   0xfff0000000000000ULL},
                                   {8, 9, 0x0,   0x7fffffffffffffffULL},
                                   {8, 11, 0x0,  0x7ff8000000000000ULL},
                                   {14, 8, 0x0,  0x7ff0000000000000ULL},
                                   {14, 14, 0x0, 0x40d0650f5a07b353ULL},
                                   {14, 6, 0x0,  0x41b0cc9d05eec2a7ULL},
                                   {14, 5, 0x0,  0x82039a19ca8fcb5fULL},
                                   {14, 4, 0x0,  0x41b0cc9d05eec2a7ULL},
                                   {14, 7, 0x0,  0xfff0000000000000ULL},
                                   {14, 9, 0x0,  0x7fffffffffffffffULL},
                                   {14, 11, 0x0, 0x7ff8000000000000ULL},
                                   {6, 8, 0x0,   0x7ff0000000000000ULL},
                                   {6, 14, 0x0,  0x40d0650f5a07b353ULL},
                                   {6, 6, 0x0,   0x0000000000000000ULL},
                                   {6, 5, 0x0,   0x8000000000000000ULL},
                                   {6, 4, 0x0,   0x8123214569900000ULL},
                                   {6, 7, 0x0,   0xfff0000000000000ULL},
                                   {6, 9, 0x0,   0x7fffffffffffffffULL},
                                   {6, 11, 0x0,  0x7ff8000000000000ULL},
                                   {5, 8, 0x0,   0x7ff0000000000000ULL},
                                   {5, 14, 0x0,  0x40d0650f5a07b353ULL},
                                   {5, 6, 0x0,   0x0000000000000000ULL},
                                   {5, 5, 0x0,   0x0000000000000000ULL},
                                   {5, 4, 0x0,   0x8123214569900000ULL},
                                   {5, 7, 0x0,   0xfff0000000000000ULL},
                                   {5, 9, 0x0,   0x7fffffffffffffffULL},
                                   {5, 11, 0x0,  0x7ff8000000000000ULL},
                                   {4, 8, 0x0,   0x7ff0000000000000ULL},
                                   {4, 14, 0x0,  0x40d0650f5a07b353ULL},
                                   {4, 6, 0x0,   0x82039a19ca8fcb5fULL},
                                   {4, 5, 0x0,   0x0000000000000000ULL},
                                   {4, 1, 0x0,   0xc04f000000000000ULL},
                                   {4, 7, 0x0,   0xfff0000000000000ULL},
                                   {4, 9, 0x0,   0x7fffffffffffffffULL},
                                   {4, 11, 0x0,  0x7ff8000000000000ULL},
                                   {7, 8, 0x0,   0x7ff8000000000000ULL},
                                   {7, 14, 0x0,  0x7ff0000000000000ULL},
                                   {7, 6, 0x0,   0xfff0000000000000ULL},
                                   {7, 5, 0x0,   0x7ff0000000000000ULL},
                                   {7, 4, 0x0,   0xfff0000000000000ULL},
                                   {7, 7, 0x0,   0x7ff8000000000000ULL},
                                   {7, 9, 0x0,   0x7fffffffffffffffULL},
                                   {7, 11, 0x0,  0x7ff8000000000000ULL},
                                   {10, 8, 0x0,  0xffffffffffffffffULL},
                                   {10, 14, 0x0, 0xffffffffffffffffULL},
                                   {10, 6, 0x0,  0xffffffffffffffffULL},
                                   {10, 5, 0x0,  0xffffffffffffffffULL},
                                   {10, 4, 0x0,  0xffffffffffffffffULL},
                                   {10, 7, 0x0,  0xffffffffffffffffULL},
                                   {10, 9, 0x0,  0xffffffffffffffffULL},
                                   {10, 11, 0x0, 0xffffffffffffffffULL},
                                   {12, 8, 0x0,  0xfff8000000000000ULL},
                                   {12, 14, 0x0, 0xfff8000000000000ULL},
                                   {12, 6, 0x0,  0xfff8000000000000ULL},
                                   {12, 5, 0x0,  0xfff8000000000000ULL},
                                   {12, 4, 0x0,  0xfff8000000000000ULL},
                                   {12, 7, 0x0,  0xfff8000000000000ULL},
                                   {12, 9, 0x0,  0xfff8000000000000ULL},
                                   {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xsnmaddXdp_tests[] = {
                                     {8, 8, 0x0,   0x7ff8000000000000ULL},
                                     {8, 14, 0x0,  0x7ff0000000000000ULL},
                                     {8, 6, 0x0,   0xfff0000000000000ULL},
                                     {8, 5, 0x0,   0x7ff0000000000000ULL},
                                     {8, 4, 0x0,   0xfff0000000000000ULL},
                                     {8, 7, 0x0,   0x7ff8000000000000ULL},
                                     {8, 9, 0x0,   0x7fffffffffffffffULL},
                                     {8, 11, 0x0,  0x7ff8000000000000ULL},
                                     {14, 8, 0x0,  0x7ff0000000000000ULL},
                                     {14, 14, 0x0, 0x40d0650f5a07b353ULL},
                                     {14, 6, 0x0,  0xc1b0cc9d05eec2a7ULL},
                                     {14, 5, 0x0,  0x02039a19ca8fcb5fULL},
                                     {14, 4, 0x0,  0xc1b0cc9d05eec2a7ULL},
                                     {14, 7, 0x0,  0xfff0000000000000ULL},
                                     {14, 9, 0x0,  0x7fffffffffffffffULL},
                                     {14, 11, 0x0, 0x7ff8000000000000ULL},
                                     {6, 8, 0x0,   0x7ff0000000000000ULL},
                                     {6, 14, 0x0,  0x40d0650f5a07b353ULL},
                                     {6, 6, 0x0,   0x8000000000000000ULL},
                                     {6, 5, 0x0,   0x8000000000000000ULL},
                                     {6, 4, 0x0,   0x8123214569900000ULL},
                                     {6, 7, 0x0,   0xfff0000000000000ULL},
                                     {6, 9, 0x0,   0x7fffffffffffffffULL},
                                     {6, 11, 0x0,  0x7ff8000000000000ULL},
                                     {5, 8, 0x0,   0x7ff0000000000000ULL},
                                     {5, 14, 0x0,  0x40d0650f5a07b353ULL},
                                     {5, 6, 0x0,   0x0000000000000000ULL},
                                     {5, 5, 0x0,   0x8000000000000000ULL},
                                     {5, 4, 0x0,   0x8123214569900000ULL},
                                     {5, 7, 0x0,   0xfff0000000000000ULL},
                                     {5, 9, 0x0,   0x7fffffffffffffffULL},
                                     {5, 11, 0x0,  0x7ff8000000000000ULL},
                                     {4, 8, 0x0,   0x7ff0000000000000ULL},
                                     {4, 14, 0x0,  0x40d0650f5a07b353ULL},
                                     {4, 6, 0x0,   0x02039a19ca8fcb5fULL},
                                     {4, 5, 0x0,   0x8000000000000000ULL},
                                     {4, 1, 0x0,   0xc04f000000000000ULL},
                                     {4, 7, 0x0,   0xfff0000000000000ULL},
                                     {4, 9, 0x0,   0x7fffffffffffffffULL},
                                     {4, 11, 0x0,  0x7ff8000000000000ULL},
                                     {7, 8, 0x0,   0x7ff0000000000000ULL},
                                     {7, 14, 0x0,  0xfff0000000000000ULL},
                                     {7, 6, 0x0,   0x7ff0000000000000ULL},
                                     {7, 5, 0x0,   0xfff0000000000000ULL},
                                     {7, 4, 0x0,   0x7ff0000000000000ULL},
                                     {7, 7, 0x0,   0xfff0000000000000ULL},
                                     {7, 9, 0x0,   0x7fffffffffffffffULL},
                                     {7, 11, 0x0,  0x7ff8000000000000ULL},
                                     {10, 8, 0x0,  0xffffffffffffffffULL},
                                     {10, 14, 0x0, 0xffffffffffffffffULL},
                                     {10, 6, 0x0,  0xffffffffffffffffULL},
                                     {10, 5, 0x0,  0xffffffffffffffffULL},
                                     {10, 4, 0x0,  0xffffffffffffffffULL},
                                     {10, 7, 0x0,  0xffffffffffffffffULL},
                                     {10, 9, 0x0,  0xffffffffffffffffULL},
                                     {10, 11, 0x0, 0xffffffffffffffffULL},
                                     {12, 8, 0x0,  0xfff8000000000000ULL},
                                     {12, 14, 0x0, 0xfff8000000000000ULL},
                                     {12, 6, 0x0,  0xfff8000000000000ULL},
                                     {12, 5, 0x0,  0xfff8000000000000ULL},
                                     {12, 4, 0x0,  0xfff8000000000000ULL},
                                     {12, 7, 0x0,  0xfff8000000000000ULL},
                                     {12, 9, 0x0,  0xfff8000000000000ULL},
                                     {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xsmuldp_tests[] = {
                                  {8, 8, 0x0,   0x7ff0000000000000ULL},
                                  {8, 14, 0x0,  0x7ff0000000000000ULL},
                                  {8, 6, 0x0,   0x7ff8000000000000ULL},
                                  {8, 5, 0x0,   0x7ff8000000000000ULL},
                                  {8, 4, 0x0,   0xfff0000000000000ULL},
                                  {8, 7, 0x0,   0xfff0000000000000ULL},
                                  {8, 9, 0x0,   0x7fffffffffffffffULL},
                                  {8, 11, 0x0,  0x7ff8000000000000ULL},
                                  {14, 8, 0x0,  0x7ff0000000000000ULL},
                                  {14, 14, 0x0, 0x41b0cc9d05eec2a7ULL},
                                  {14, 6, 0x0,  0x0000000000000000ULL},
                                  {14, 5, 0x0,  0x8000000000000000ULL},
                                  {14, 4, 0x0,  0x82039a19ca8fcb5fULL},
                                  {14, 7, 0x0,  0xfff0000000000000ULL},
                                  {14, 9, 0x0,  0x7fffffffffffffffULL},
                                  {14, 11, 0x0, 0x7ff8000000000000ULL},
                                  {6, 8, 0x0,   0x7ff8000000000000ULL},
                                  {6, 14, 0x0,  0x0000000000000000ULL},
                                  {6, 6, 0x0,   0x0000000000000000ULL},
                                  {6, 5, 0x0,   0x8000000000000000ULL},
                                  {6, 4, 0x0,   0x8000000000000000ULL},
                                  {6, 7, 0x0,   0x7ff8000000000000ULL},
                                  {6, 9, 0x0,   0x7fffffffffffffffULL},
                                  {6, 11, 0x0,  0x7ff8000000000000ULL},
                                  {5, 8, 0x0,   0x7ff8000000000000ULL},
                                  {5, 14, 0x0,  0x8000000000000000ULL},
                                  {5, 6, 0x0,   0x8000000000000000ULL},
                                  {5, 5, 0x0,   0x0000000000000000ULL},
                                  {5, 4, 0x0,   0x0000000000000000ULL},
                                  {5, 7, 0x0,   0x7ff8000000000000ULL},
                                  {5, 9, 0x0,   0x7fffffffffffffffULL},
                                  {5, 11, 0x0,  0x7ff8000000000000ULL},
                                  {4, 8, 0x0,   0xfff0000000000000ULL},
                                  {4, 14, 0x0,  0x82039a19ca8fcb5fULL},
                                  {4, 6, 0x0,   0x8000000000000000ULL},
                                  {4, 5, 0x0,   0x0000000000000000ULL},
                                  {4, 1, 0x0,   0x0182883b3e438000ULL},
                                  {4, 7, 0x0,   0x7ff0000000000000ULL},
                                  {4, 9, 0x0,   0x7fffffffffffffffULL},
                                  {4, 11, 0x0,  0x7ff8000000000000ULL},
                                  {7, 8, 0x0,   0xfff0000000000000ULL},
                                  {7, 14, 0x0,  0xfff0000000000000ULL},
                                  {7, 6, 0x0,   0x7ff8000000000000ULL},
                                  {7, 5, 0x0,   0x7ff8000000000000ULL},
                                  {7, 4, 0x0,   0x7ff0000000000000ULL},
                                  {7, 7, 0x0,   0x7ff0000000000000ULL},
                                  {7, 9, 0x0,   0x7fffffffffffffffULL},
                                  {7, 11, 0x0,  0x7ff8000000000000ULL},
                                  {10, 8, 0x0,  0xffffffffffffffffULL},
                                  {10, 14, 0x0, 0xffffffffffffffffULL},
                                  {10, 6, 0x0,  0xffffffffffffffffULL},
                                  {10, 5, 0x0,  0xffffffffffffffffULL},
                                  {10, 4, 0x0,  0xffffffffffffffffULL},
                                  {10, 7, 0x0,  0xffffffffffffffffULL},
                                  {10, 9, 0x0,  0xffffffffffffffffULL},
                                  {10, 11, 0x0, 0xffffffffffffffffULL},
                                  {12, 8, 0x0,  0xfff8000000000000ULL},
                                  {12, 14, 0x0, 0xfff8000000000000ULL},
                                  {12, 6, 0x0,  0xfff8000000000000ULL},
                                  {12, 5, 0x0,  0xfff8000000000000ULL},
                                  {12, 4, 0x0,  0xfff8000000000000ULL},
                                  {12, 7, 0x0,  0xfff8000000000000ULL},
                                  {12, 9, 0x0,  0xfff8000000000000ULL},
                                  {12, 11, 0x0, 0xfff8000000000000ULL},
};

fp_test_args_t xssubdp_tests[] = {
                                  {8, 8, 0x0,   0x7ff8000000000000ULL},
                                  {8, 14, 0x0,  0xfff0000000000000ULL},
                                  {8, 6, 0x0,   0xfff0000000000000ULL},
                                  {8, 5, 0x0,   0xfff0000000000000ULL},
                                  {8, 4, 0x0,   0xfff0000000000000ULL},
                                  {8, 7, 0x0,   0xfff0000000000000ULL},
                                  {8, 9, 0x0,   0x7fffffffffffffffULL},
                                  {8, 11, 0x0,  0x7ff8000000000000ULL},
                                  {14, 8, 0x0,  0x7ff0000000000000ULL},
                                  {14, 14, 0x0, 0x0000000000000000ULL},
                                  {14, 6, 0x0,  0xc0d0650f5a07b353ULL},
                                  {14, 5, 0x0,  0xc0d0650f5a07b353ULL},
                                  {14, 4, 0x0,  0xc0d0650f5a07b353ULL},
                                  {14, 7, 0x0,  0xfff0000000000000ULL},
                                  {14, 9, 0x0,  0x7fffffffffffffffULL},
                                  {14, 11, 0x0, 0x7ff8000000000000ULL},
                                  {6, 8, 0x0,   0x7ff0000000000000ULL},
                                  {6, 14, 0x0,  0x40d0650f5a07b353ULL},
                                  {6, 6, 0x0,   0x0000000000000000ULL},
                                  {6, 5, 0x0,   0x8000000000000000ULL},
                                  {6, 4, 0x0,   0x8123214569900000ULL},
                                  {6, 7, 0x0,   0xfff0000000000000ULL},
                                  {6, 9, 0x0,   0x7fffffffffffffffULL},
                                  {6, 11, 0x0,  0x7ff8000000000000ULL},
                                  {5, 8, 0x0,   0x7ff0000000000000ULL},
                                  {5, 14, 0x0,  0x40d0650f5a07b353ULL},
                                  {5, 6, 0x0,   0x0000000000000000ULL},
                                  {5, 5, 0x0,   0x0000000000000000ULL},
                                  {5, 4, 0x0,   0x8123214569900000ULL},
                                  {5, 7, 0x0,   0xfff0000000000000ULL},
                                  {5, 9, 0x0,   0x7fffffffffffffffULL},
                                  {5, 11, 0x0,  0x7ff8000000000000ULL},
                                  {4, 8, 0x0,   0x7ff0000000000000ULL},
                                  {4, 14, 0x0,  0x40d0650f5a07b353ULL},
                                  {4, 6, 0x0,   0x0123214569900000ULL},
                                  {4, 5, 0x0,   0x0123214569900000ULL},
                                  {4, 1, 0x0,   0xc04f000000000000ULL},
                                  {4, 7, 0x0,   0xfff0000000000000ULL},
                                  {4, 9, 0x0,   0x7fffffffffffffffULL},
                                  {4, 11, 0x0,  0x7ff8000000000000ULL},
                                  {7, 8, 0x0,   0x7ff0000000000000ULL},
                                  {7, 14, 0x0,  0x7ff0000000000000ULL},
                                  {7, 6, 0x0,   0x7ff0000000000000ULL},
                                  {7, 5, 0x0,   0x7ff0000000000000ULL},
                                  {7, 4, 0x0,   0x7ff0000000000000ULL},
                                  {7, 7, 0x0,   0x7ff8000000000000ULL},
                                  {7, 9, 0x0,   0x7fffffffffffffffULL},
                                  {7, 11, 0x0,  0x7ff8000000000000ULL},
                                  {10, 8, 0x0,  0xffffffffffffffffULL},
                                  {10, 14, 0x0, 0xffffffffffffffffULL},
                                  {10, 6, 0x0,  0xffffffffffffffffULL},
                                  {10, 5, 0x0,  0xffffffffffffffffULL},
                                  {10, 4, 0x0,  0xffffffffffffffffULL},
                                  {10, 7, 0x0,  0xffffffffffffffffULL},
                                  {10, 9, 0x0,  0xffffffffffffffffULL},
                                  {10, 11, 0x0, 0xffffffffffffffffULL},
                                  {12, 8, 0x0,  0xfff8000000000000ULL},
                                  {12, 14, 0x0, 0xfff8000000000000ULL},
                                  {12, 6, 0x0,  0xfff8000000000000ULL},
                                  {12, 5, 0x0,  0xfff8000000000000ULL},
                                  {12, 4, 0x0,  0xfff8000000000000ULL},
                                  {12, 7, 0x0,  0xfff8000000000000ULL},
                                  {12, 9, 0x0,  0xfff8000000000000ULL},
                                  {12, 11, 0x0, 0xfff8000000000000ULL},
};



static int nb_special_fargs;
static double * spec_fargs;

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
   9      0   7ff   0x7FFFFFFFFFFFFULL         +QNaN
   10     1   7ff   0x7FFFFFFFFFFFFULL         -QNaN
   11     0   7ff   0x8000000000000ULL         +SNaN
   12     1   7ff   0x8000000000000ULL         -SNaN
   13     1   000   0x8340000078000ULL         Denormalized val (zero exp and non-zero fraction)
   14     1   40d   0x0650f5a07b353ULL         Negative finite number
    */

   uint64_t mant;
   uint16_t _exp;
   int s;
   int i = 0;

   if (spec_fargs)
      return;

   spec_fargs = malloc( 16 * sizeof(double) );

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

   nb_special_fargs = i;
}


struct test_table
{
   test_func_t test_category;
   char * name;
};

struct p7_fp_test
{
   test_func_t test_func;
   const char *name;
   int single;  // 1=single precision result; 0=double precision result
};

typedef enum {
   VX_FP_CMP,
   VX_FP_SMA,
   VX_FP_SMS,
   VX_FP_SNMA,
   VX_FP_OTHER
} vx_fp_test_type;

struct vx_fp_test
{
   test_func_t test_func;
   const char *name;
   fp_test_args_t * targs;
   int num_tests;
   vx_fp_test_type test_type;
};

struct xs_conv_test
{
   test_func_t test_func;
   const char *name;
   unsigned long long * results;
   int num_tests;
};

typedef enum {
   VSX_LOAD =1,
   VSX_LOAD_SPLAT,
   VSX_STORE
} vsx_ldst_type;

struct ldst_test
{
   test_func_t test_func;
   const char *name;
   void * base_addr;
   uint32_t offset;
   int num_words_to_process;
   vsx_ldst_type type;
};

typedef enum {
   VSX_AND = 1,
   VSX_XOR,
   VSX_ANDC,
   VSX_OR,
   VSX_NOR
} vsx_log_op;

struct vsx_logic_test
{
   test_func_t test_func;
   const char *name;
   vsx_log_op op;
};

struct vsx_move_test
{
   test_func_t test_func;
   const char *name;
   int xa_idx, xb_idx;
   unsigned long long expected_result;
};

struct vsx_permute_test
{
   test_func_t test_func;
   const char *name;
   unsigned int xa[4];
   unsigned int xb[4];
   unsigned int expected_output[4];
};

static vector unsigned int vec_out, vec_inA, vec_inB;

static void test_lxsdx(void)
{
   __asm__ __volatile__ ("lxsdx          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void
test_lxvd2x(void)
{
   __asm__ __volatile__ ("lxvd2x          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_lxvdsx(void)
{
   __asm__ __volatile__ ("lxvdsx          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_lxvw4x(void)
{
   __asm__ __volatile__ ("lxvw4x          %x0, %1, %2" : "=wa" (vec_out): "b" (r14),"r" (r15));
}

static void test_stxsdx(void)
{
   __asm__ __volatile__ ("stxsdx          %x0, %1, %2" : : "wa" (vec_inA), "b" (r14),"r" (r15));
}

static void test_stxvd2x(void)
{
   __asm__ __volatile__ ("stxvd2x          %x0, %1, %2" : : "wa" (vec_inA), "b" (r14),"r" (r15));
}

static void test_stxvw4x(void)
{
   __asm__ __volatile__ ("stxvw4x          %x0, %1, %2" : : "wa" (vec_inA), "b" (r14),"r" (r15));
}

static void test_xxlxor(void)
{
   __asm__ __volatile__ ("xxlxor          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxlor(void)
{
   __asm__ __volatile__ ("xxlor          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxlnor(void)
{
   __asm__ __volatile__ ("xxlnor          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxland(void)
{
   __asm__ __volatile__ ("xxland          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxlandc(void)
{
   __asm__ __volatile__ ("xxlandc          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxmrghw(void)
{
   __asm__ __volatile__ ("xxmrghw          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxmrglw(void)
{
   __asm__ __volatile__ ("xxmrglw          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxpermdi_00(void)
{
   __asm__ __volatile__ ("xxpermdi         %x0, %x1, %x2, 0x0" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxpermdi_01(void)
{
   __asm__ __volatile__ ("xxpermdi         %x0, %x1, %x2, 0x1" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxpermdi_10(void)
{
   __asm__ __volatile__ ("xxpermdi         %x0, %x1, %x2, 0x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxpermdi_11(void)
{
   __asm__ __volatile__ ("xxpermdi         %x0, %x1, %x2, 0x3" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxsldwi_0(void)
{
   __asm__ __volatile__ ("xxsldwi         %x0, %x1, %x2, 0" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxsldwi_1(void)
{
   __asm__ __volatile__ ("xxsldwi         %x0, %x1, %x2, 1" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxsldwi_2(void)
{
   __asm__ __volatile__ ("xxsldwi         %x0, %x1, %x2, 2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xxsldwi_3(void)
{
   __asm__ __volatile__ ("xxsldwi         %x0, %x1, %x2, 3" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_fcfids (void)
{
    __asm__ __volatile__ ("fcfids          %0, %1" : "=f" (f17): "d" (f14));
}

static void test_fcfidus (void)
{
    __asm__ __volatile__ ("fcfidus          %0, %1" : "=f" (f17): "d" (f14));
}

static void test_fcfidu (void)
{
    __asm__ __volatile__ ("fcfidu          %0, %1" : "=f" (f17): "d" (f14));
}

static void test_xsabsdp (void)
{
   __asm__ __volatile__ ("xsabsdp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscpsgndp (void)
{
   __asm__ __volatile__ ("xscpsgndp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsnabsdp (void)
{
   __asm__ __volatile__ ("xsnabsdp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xsnegdp (void)
{
   __asm__ __volatile__ ("xsnegdp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static int do_cmpudp;
static void test_xscmp (void)
{
   if (do_cmpudp)
      __asm__ __volatile__ ("xscmpudp          cr1, %x0, %x1" : : "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xscmpodp          cr1, %x0, %x1" : : "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsadddp(void)
{
   __asm__ __volatile__ ("xsadddp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsdivdp(void)
{
   __asm__ __volatile__ ("xsdivdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static int do_adp;
static void test_xsmadd(void)
{
   if (do_adp)
      __asm__ __volatile__ ("xsmaddadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsmaddmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsmsub(void)
{
   if (do_adp)
      __asm__ __volatile__ ("xsmsubadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsmsubmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsnmadd(void)
{
   if (do_adp)
      __asm__ __volatile__ ("xsnmaddadp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
   else
      __asm__ __volatile__ ("xsnmaddmdp          %x0, %x1, %x2" : "+wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xsmuldp(void)
{
   __asm__ __volatile__ ("xsmuldp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xssubdp(void)
{
   __asm__ __volatile__ ("xssubdp          %x0, %x1, %x2" : "=wa" (vec_out): "wa" (vec_inA),"wa" (vec_inB));
}

static void test_xscvdpsxds (void)
{
   __asm__ __volatile__ ("xscvdpsxds          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvsxddp (void)
{
   __asm__ __volatile__ ("xscvsxddp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static void test_xscvuxddp (void)
{
   __asm__ __volatile__ ("xscvuxddp          %x0, %x1" : "=wa" (vec_out): "wa" (vec_inB));
}

static unsigned int vstg[] __attribute__ ((aligned (16))) = { 0, 0, 0,0,
                                                              0, 0, 0, 0 };

#define NUM_VSTG_INTS (sizeof vstg/sizeof vstg[0])
#define NUM_VSTG_VECS (NUM_VSTG_INTS/4)

static unsigned int viargs[] __attribute__ ((aligned (16))) = { 0x01234567,
                                                                0x89abcdef,
                                                                0x00112233,
                                                                0x44556677,
                                                                0x8899aabb,
                                                                0x91929394,
                                                                0xa1a2a3a4,
                                                                0xb1b2b3b4,
                                                                0xc1c2c3c4,
                                                                0xd1d2d3d4,
                                                                0x7a6b5d3e
};
#define NUM_VIARGS_INTS (sizeof viargs/sizeof viargs[0])
#define NUM_VIARGS_VECS  (NUM_VIARGS_INTS/4)

static ldst_test_t ldst_tests[] = { { &test_lxsdx, "lxsdx", viargs, 0, 2, VSX_LOAD },
                                     { &test_lxsdx, "lxsdx", viargs, 4, 2, VSX_LOAD },
                                     { &test_lxvd2x, "lxvd2x", viargs, 0, 4, VSX_LOAD },
                                     { &test_lxvd2x, "lxvd2x", viargs, 4, 4, VSX_LOAD },
                                     { &test_lxvdsx, "lxvdsx", viargs, 0, 4, VSX_LOAD_SPLAT },
                                     { &test_lxvdsx, "lxvdsx", viargs, 4, 4, VSX_LOAD_SPLAT },
                                     { &test_lxvw4x, "lxvw4x", viargs, 0, 4, VSX_LOAD },
                                     { &test_lxvw4x, "lxvw4x", viargs, 4, 4, VSX_LOAD },
                                     { &test_stxsdx, "stxsdx", vstg, 0, 2, VSX_STORE },
                                     { &test_stxsdx, "stxsdx", vstg, 4, 2, VSX_STORE },
                                     { &test_stxvd2x, "stxvd2x", vstg, 0, 4, VSX_STORE },
                                     { &test_stxvd2x, "stxvd2x", vstg, 4, 4, VSX_STORE },
                                     { &test_stxvw4x, "stxvw4x", vstg, 0, 4, VSX_STORE },
                                     { &test_stxvw4x, "stxvw4x", vstg, 4, 4, VSX_STORE },
                                     { NULL, NULL, NULL, 0, 0, 0 } };

static logic_test_t logic_tests[] = { { &test_xxlxor, "xxlxor", VSX_XOR },
                                      { &test_xxlor, "xxlor", VSX_OR } ,
                                      { &test_xxlnor, "xxlnor", VSX_NOR },
                                      { &test_xxland, "xxland", VSX_AND },
                                      { &test_xxlandc, "xxlandc", VSX_ANDC },
                                      { NULL, NULL}};

static move_test_t move_tests[] = { { &test_xsabsdp, "xsabsdp", 0, 4, 0x0899aabb91929394ULL },
                                    { &test_xscpsgndp, "xscpsgndp", 4, 0, 0x8123456789abcdefULL },
                                    { &test_xsnabsdp, "xsnabsdp", 7, 3, 0xc45566778899aabbULL, },
                                    { &test_xsnegdp, "xsnegdp", 0, 7, 0x31b2b3b4c1c2c3c4ULL, },
                                    { NULL, NULL, 0, 0, 0 }

};

static permute_test_t permute_tests[] =
{
  { &test_xxmrghw, "xxmrghw", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x11111111, 0x55555555, 0x22222222, 0x66666666 }  /* XT expected output */
  },
  { &test_xxmrghw, "xxmrghw", 
    { 0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff }, /* XA input */
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XB input */
    { 0x00112233, 0x11111111, 0x44556677, 0x22222222 }  /* XT expected output */
  },
  { &test_xxmrglw, "xxmrglw", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x33333333, 0x77777777, 0x44444444, 0x88888888 }  /* XT expected output */
  },
  { &test_xxmrglw, "xxmrglw", 
    { 0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff}, /* XA input */
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444}, /* XB input */
    { 0x8899aabb, 0x33333333, 0xccddeeff, 0x44444444}  /* XT expected output */
  },
  { &test_xxpermdi_00, "xxpermdi DM=00", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x11111111, 0x22222222, 0x55555555, 0x66666666 }  /* XT expected output */
  },
  { &test_xxpermdi_01, "xxpermdi DM=01", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x11111111, 0x22222222, 0x77777777, 0x88888888 }  /* XT expected output */
  },
  { &test_xxpermdi_10, "xxpermdi DM=10", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x33333333, 0x44444444, 0x55555555, 0x66666666 }  /* XT expected output */
  },
  { &test_xxpermdi_11, "xxpermdi DM=11", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x33333333, 0x44444444, 0x77777777, 0x88888888 }  /* XT expected output */
  },
  { &test_xxsldwi_0, "xxsldwi SHW=0", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }  /* XT expected output */
  },
  { &test_xxsldwi_1, "xxsldwi SHW=1", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x22222222, 0x33333333, 0x44444444, 0x55555555 }  /* XT expected output */
  },
  { &test_xxsldwi_2, "xxsldwi SHW=2", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x33333333, 0x44444444, 0x55555555, 0x66666666 }  /* XT expected output */
  },
  { &test_xxsldwi_3, "xxsldwi SHW=3", 
    { 0x11111111, 0x22222222, 0x33333333, 0x44444444 }, /* XA input */
    { 0x55555555, 0x66666666, 0x77777777, 0x88888888 }, /* XB input */
    { 0x44444444, 0x55555555, 0x66666666, 0x77777777 }  /* XT expected output */
  },
  { NULL, NULL }
};

static fp_test_t fp_tests[] = { { &test_fcfids, "fcfids", 1 },
                                { &test_fcfidus, "fcfidus", 1 },
                                { &test_fcfidu, "fcfidu", 1 },
                                { NULL, NULL, 0 },

};

static vx_fp_test_t vx_fp_tests[] = {
                                     { &test_xscmp, "xscmp", xscmpX_tests, 64, VX_FP_CMP},
                                     { &test_xsadddp, "xsadddp", xsadddp_tests, 64, VX_FP_OTHER},
                                     { &test_xsdivdp, "xsdivdp", xsdivdp_tests, 64, VX_FP_OTHER},
                                     { &test_xsmadd, "xsmadd", xsmaddXdp_tests, 64, VX_FP_SMA},
                                     { &test_xsmsub, "xsmsub", xsmsubXdp_tests, 64, VX_FP_SMS},
                                     { &test_xsnmadd, "xsnmadd", xsnmaddXdp_tests, 64, VX_FP_SNMA},
                                     { & test_xsmuldp, "xsmuldp", xsmuldp_tests, 64, VX_FP_OTHER},
                                     { & test_xssubdp, "xssubdp", xssubdp_tests, 64, VX_FP_OTHER},
                                     { NULL, NULL, NULL, 0, 0 }
};

static xs_conv_test_t xs_conv_tests[] = {
                                         { &test_xscvdpsxds, "xscvdpsxds", xscvdpsxds_results, 15},
                                         { &test_xscvsxddp, "xscvsxddp", xscvsxddp_results, 15},
                                         { &test_xscvuxddp, "xscvuxddp", xscvuxddp_results, 15},
                                         { NULL, NULL, NULL, 0}
};

#ifdef __powerpc64__
static void test_ldbrx(void)
{
   int i, equality;
   HWord_t reg_out;
   unsigned char * byteIn, * byteOut;
   r14 = (HWord_t)viargs;
   // Just try the instruction an arbitrary number of times at different r15 offsets.
   for (i = 0; i < 3; i++) {
      int j, k;
      reg_out = 0;
      r15 = i * 4;
      equality = 1;
      __asm__ __volatile__ ("ldbrx          %0, %1, %2" : "=r" (reg_out): "b" (r14),"r" (r15));
      byteIn = ((unsigned char *)(r14 + r15));
      byteOut = (unsigned char *)&reg_out;

      printf("ldbrx:");
      for (k = 0; k < 7; k++) {
         printf( " %02x", (byteIn[k]));
      }
      printf(" (reverse) =>");
      for (j = 0; j < 8; j++) {
         printf( " %02x", (byteOut[j]));
      }
      printf("\n");
      for (j = 0, k = 7; j < 8; j++, k--) {
         equality &= (byteIn[k] == byteOut[j]);
      }
      if (!equality) {
         printf("FAILED: load with byte reversal is incorrect\n");
         errors++;
      }
   }
   printf( "\n" );
}

static void
test_popcntd(void)
{
   uint64_t res;
   unsigned long long src = 0x9182736405504536ULL;
   int i, answer = 0;
   r14 = src;
   __asm__ __volatile__ ("popcntd          %0, %1" : "=r" (res): "r" (r14));
   for (i = 0; i < 64; i++) {
      answer += (r14 & 1ULL);
      r14 = r14 >> 1;
   }
   printf("popcntd: 0x%llx => %d\n", src, (int)res);
   if (res!= answer) {
      printf("Error: unexpected result from popcntd\n");
      errors++;
   }
   printf( "\n" );
}
#endif

static void
test_lfiwzx(void)
{
   unsigned int i;
   unsigned int * src;
   uint64_t reg_out;
   r14 = (HWord_t)viargs;
   // Just try the instruction an arbitrary number of times at different r15 offsets.
   for (i = 0; i < 3; i++) {
      reg_out = 0;
      r15 = i * 4;
      __asm__ __volatile__ ("lfiwzx          %0, %1, %2" : "=d" (reg_out): "b" (r14),"r" (r15));
      src = ((unsigned int *)(r14 + r15));
      printf("lfiwzx: %u => %llu.00\n", *src, (unsigned long long)reg_out);

      if (reg_out > 0xFFFFFFFFULL || *src != (unsigned int)reg_out) {
         printf("FAILED: integer load to FP register is incorrect\n");
         errors++;
      }
   }
   printf( "\n" );
}

static void test_vx_fp_ops(void)
{

   test_func_t func;
   int k;
   char * test_name = (char *)malloc(20);
   k = 0;

   build_special_fargs_table();
   while ((func = vx_fp_tests[k].test_func)) {
      int i, condreg, repeat = 0;
      unsigned int flags;
      unsigned long long * frap, * frbp, * dst;
      vx_fp_test_t test_group = vx_fp_tests[k];
      vx_fp_test_type test_type = test_group.test_type;

      switch (test_type) {
         case VX_FP_CMP:
            strcpy(test_name, "xscmp");
            if (!repeat) {
               repeat = 1;
               strcat(test_name, "udp");
               do_cmpudp = 1;
            }
            break;
         case VX_FP_SMA:
         case VX_FP_SMS:
         case VX_FP_SNMA:
            if (test_type == VX_FP_SMA)
               strcpy(test_name, "xsmadd");
            else if (test_type == VX_FP_SMS)
               strcpy(test_name, "xsmsub");
            else
               strcpy(test_name, "xsnmadd");
            if (!repeat) {
               repeat = 1;
               strcat(test_name, "adp");
               do_adp = 1;
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
         // Only need to copy one doubleword into each vector's element 0
         memcpy(&vec_inA, inA, 8);
         memcpy(&vec_inB, inB, 8);

         switch (test_type) {
            case VX_FP_CMP:
               SET_FPSCR_ZERO;
               SET_CR_XER_ZERO;
               (*func)();
               GET_CR(flags);
               condreg = (flags & 0x0f000000) >> 24;
               printf("#%d: %s %016llx <=> %016llx ? %x (CRx)\n", i, test_name, *frap, *frbp, condreg);
              // printf("\tFRA: %e;  FRB: %e\n", spec_fargs[aTest.fra_idx], spec_fargs[aTest.frb_idx]);
               if ( condreg != aTest.cr_flags) {
                  printf("Error: Expected CR flags 0x%x; actual flags: 0x%x\n", aTest.cr_flags, condreg);
                  errors++;
               }
               break;
            case VX_FP_SMA:
            case VX_FP_SMS:
            case VX_FP_SNMA:
            case VX_FP_OTHER:
            {
               int idx;
               unsigned long long vsr_XT;
               pv = (unsigned int *)&vec_out;
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

                     //memcpy(&vec_out, &spec_fargs[14], 8);

                  if (repeat) {
                     /* We're on the first time through of one of the VX_FP_SMx
                      * test types, meaning we're testing a xs<ZZZ>adp case, thus we
                      * have to swap inputs as described above:
                      *    src2 <= VSX[XT]
                      *    src3 <= VSX[XB]
                      */
                     memcpy(&vec_out, inB, 8);  // src2
                     memcpy(&vec_inB, &spec_fargs[extra_arg_idx], 8);  //src3
                     frbp = (unsigned long long *)&spec_fargs[extra_arg_idx];
                  } else {
                     // Don't need to init src2, as it's done before the switch()
                     memcpy(&vec_out, &spec_fargs[extra_arg_idx], 8);  //src3
                  }
                  memcpy(&vsr_XT, &vec_out, 8);
               }

               (*func)();
               dst = (unsigned long long *) &vec_out;
               if (test_type == VX_FP_OTHER)
                  printf("#%d: %s %016llx %016llx = %016llx\n", i, test_name, *frap, *frbp, *dst);
               else
                  printf( "#%d: %s %016llx %016llx %016llx = %016llx\n", i,
                          test_name, vsr_XT, *frap, *frbp, *dst );

               if ( *dst != aTest.dp_bin_result) {
                  printf("Error: Expected result %016llx; actual result %016llx\n", aTest.dp_bin_result, *dst);
                  errors++;
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
               break;
            }
         }


      }
      printf( "\n" );

      if (repeat) {
         repeat = 0;
         switch (test_type) {
            case VX_FP_CMP:
               strcpy(test_name, "xscmp");
               strcat(test_name, "odp");
               do_cmpudp = 0;
               break;
            case VX_FP_SMA:
            case VX_FP_SMS:
            case VX_FP_SNMA:
               if (test_type == VX_FP_SMA)
                  strcpy(test_name, "xsmadd");
               else if (test_type == VX_FP_SMS)
                  strcpy(test_name, "xsmsub");
               else
                  strcpy(test_name, "xsnmadd");
               strcat(test_name, "mdp");
               do_adp = 0;
               break;
            case VX_FP_OTHER:
               break;
         }
         goto again;
      }
      k++;
   }
   printf( "\n" );
   free(test_name);
}

static void test_xs_conv_ops(void)
{

   test_func_t func;
   int k = 0;

   build_special_fargs_table();
   while ((func = xs_conv_tests[k].test_func)) {
      int i;
      unsigned long long * frbp, * dst;
      xs_conv_test_t test_group = xs_conv_tests[k];
      for (i = 0; i < test_group.num_tests; i++) {
         unsigned int * inB, * pv;
         int idx;
         unsigned long long exp_result = test_group.results[i];
         inB = (unsigned int *)&spec_fargs[i];
         frbp = (unsigned long long *)&spec_fargs[i];
         memcpy(&vec_inB, inB, 8);
         pv = (unsigned int *)&vec_out;
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;
         (*func)();
         dst = (unsigned long long *) &vec_out;
         printf("#%d: %s %016llx => %016llx\n", i, test_group.name, *frbp, *dst);

         if ( *dst != exp_result) {
            printf("Error: Expected result %016llx; actual result %016llx\n", exp_result, *dst);
            errors++;
         }
      }
      k++;
      printf("\n");
   }
   printf( "\n" );
}

static void do_load_test(ldst_test_t loadTest)
{
   test_func_t func;
   unsigned int *src, *dst;
   int splat = loadTest.type == VSX_LOAD_SPLAT ? 1: 0;
   int i, j, m, equality;
   i = j = 0;

   func = loadTest.test_func;
   for (i = 0, r14 = (HWord_t) loadTest.base_addr; i < NUM_VIARGS_VECS; i++) {
      int again;
      j = 0;
       r14 += i * 16;
      do {
         unsigned int * pv = (unsigned int *)&vec_out;
         int idx;
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv+=idx)
            *pv = 0;

         again = 0;
         r15 = j;

         // execute test insn
         (*func)();

         src = (unsigned int*) (((unsigned char *)r14) + j);
         dst = (unsigned int*) &vec_out;

         printf( "%s:", loadTest.name);
         for (m = 0; m < loadTest.num_words_to_process; m++) {
            printf( " %08x", src[splat ? m % 2 : m]);
         }
         printf( " =>");
         for (m = 0; m < loadTest.num_words_to_process; m++) {
            printf( " %08x", dst[m]);
         }
         printf("\n");
         equality = 1;
         for (m = 0; m < loadTest.num_words_to_process; m++) {
            equality = equality && (src[splat ? m % 2 : m] == dst[m]);
         }

         if (!equality) {
            printf("FAILED: loaded vector is incorrect\n");
            errors++;
         }

         if (j == 0 && loadTest.offset) {
            again = 1;
            j += loadTest.offset;
         }
      }
      while (again);
   }
}

static void
do_store_test ( ldst_test_t storeTest )
{
   test_func_t func;
   unsigned int *src, *dst;
   int i, j, m, equality;
   i = j = 0;

   func = storeTest.test_func;
   r14 = (HWord_t) storeTest.base_addr;
   r15 = (HWord_t) storeTest.offset;
   unsigned int * pv = (unsigned int *) storeTest.base_addr;
   int idx;
   // clear out storage destination
   for (idx = 0; idx < 4; idx++, pv += idx)
      *pv = 0;

   memcpy(&vec_inA, &viargs[0], sizeof(vector unsigned char));

   // execute test insn
   (*func)();
   src = &viargs[0];
   dst = (unsigned int*) (((unsigned char *) r14) + storeTest.offset);

   printf( "%s:", storeTest.name );
   for (m = 0; m < storeTest.num_words_to_process; m++) {
      printf( " %08x", src[m] );
   }
   printf( " =>" );
   for (m = 0; m < storeTest.num_words_to_process; m++) {
      printf( " %08x", dst[m] );
   }
   printf( "\n" );
   equality = 1;
   for (m = 0; m < storeTest.num_words_to_process; m++) {
      equality = equality && (src[m] == dst[m]);
   }

   if (!equality) {
      printf( "FAILED: vector store result is incorrect\n" );
      errors++;
   }

}


static void test_ldst(void)
{
   int k = 0;

   while (ldst_tests[k].test_func) {
      if (ldst_tests[k].type == VSX_STORE)
         do_store_test(ldst_tests[k]);
      else
         do_load_test(ldst_tests[k]);
      k++;
      printf("\n");
   }
}

static void test_ftdiv(void)
{
   int i, num_tests, crx;
   unsigned int flags;
   unsigned long long * frap, * frbp;
   build_special_fargs_table();

   num_tests = sizeof ftdiv_tests/sizeof ftdiv_tests[0];

   for (i = 0; i < num_tests; i++) {
      ftdiv_test_args_t aTest = ftdiv_tests[i];
      f14 = spec_fargs[aTest.fra_idx];
      f15 = spec_fargs[aTest.frb_idx];
      frap = (unsigned long long *)&spec_fargs[aTest.fra_idx];
      frbp = (unsigned long long *)&spec_fargs[aTest.frb_idx];
      SET_FPSCR_ZERO;
      SET_CR_XER_ZERO;
      __asm__ __volatile__ ("ftdiv           cr1, %0, %1" : : "d" (f14), "d" (f15));
      GET_CR(flags);
      crx = (flags & 0x0f000000) >> 24;
      printf( "ftdiv: %016llx <=> %016llx ? %x (CRx)\n", *frap, *frbp, crx);
//      printf("\tFRA: %e;  FRB: %e\n", f14, f15);
      if ( crx != aTest.cr_flags) {
         printf("Error: Expected CR flags 0x%x; actual flags: 0x%x\n", aTest.cr_flags, crx);
         errors++;
      }
   }
   printf( "\n" );
}


static void test_p7_fpops ( void )
{
   int k = 0;
   test_func_t func;

   build_fargs_table();
   while ((func = fp_tests[k].test_func)) {
      float res;
      double resd;
      unsigned long long u0;
      int i;
      int res32 = strcmp(fp_tests[k].name, "fcfidu");

      for (i = 0; i < nb_fargs; i++) {
         u0 = *(unsigned long long *) (&fargs[i]);
         f14 = fargs[i];
         (*func)();
         if (res32) {
            res = f17;
            printf( "%s %016llx => (raw sp) %08x)",
                    fp_tests[k].name, u0, *((unsigned int *)&res));
         } else {
            resd = f17;
            printf( "%s %016llx => (raw sp) %016llx)",
                    fp_tests[k].name, u0, *(unsigned long long *)(&resd));
         }
         printf( "\n" );
      }

      k++;
      printf( "\n" );
   }
}

static void test_vsx_logic(void)
{
   logic_test_t aTest;
   test_func_t func;
   int equality, k;
   k = 0;

   while ((func = logic_tests[k].test_func)) {
      unsigned int * pv;
      int startA, startB;
      unsigned int * inA, * inB, * dst;
      int idx, i;
      startA = 0;
      aTest = logic_tests[k];
      for (i = 0; i <= (NUM_VIARGS_INTS - (NUM_VIARGS_VECS * sizeof(int))); i++, startA++) {
         startB = startA + 4;
         pv = (unsigned int *)&vec_out;
         inA = &viargs[startA];
         inB = &viargs[startB];
         memcpy(&vec_inA, inA, sizeof(vector unsigned char));
         memcpy(&vec_inB, inB, sizeof(vector unsigned char));
         // clear vec_out
         for (idx = 0; idx < 4; idx++, pv++)
            *pv = 0;

         // execute test insn
         (*func)();
         dst = (unsigned int*) &vec_out;

         printf( "%s:", aTest.name);
         printf( " %08x %08x %08x %08x %s", inA[0], inA[1], inA[2], inA[3], aTest.name);
         printf( " %08x %08x %08x %08x", inB[0], inB[1], inB[2], inB[3]);
         printf(" => %08x %08x %08x %08x\n", dst[0], dst[1], dst[2], dst[3]);

         equality = 1;
         for (idx = 0; idx < 4; idx++) {
            switch (aTest.op) {
               case VSX_AND:
                  equality &= (dst[idx] == (inA[idx] & inB[idx]));
                  break;
               case VSX_ANDC:
                  equality &= (dst[idx] == (inA[idx] & ~inB[idx]));
                  break;
               case VSX_NOR:
                  equality &= (dst[idx] == ~(inA[idx] | inB[idx]));
                  break;
               case VSX_XOR:
                  equality &= (dst[idx] == (inA[idx] ^ inB[idx]));
                  break;
               case VSX_OR:
                  equality &= (dst[idx] == (inA[idx] | inB[idx]));
                  break;
               default:
                  fprintf(stderr, "Error in test_vsx_logic(): unknown VSX logical op %d\n", aTest.op);
                  exit(1);
            }
         }
         if (!equality) {
            printf( "FAILED: vector out is incorrect\n" );
            errors++;
         }
      }
      k++;
   }
   printf( "\n" );
}

static void test_move_ops (void)
{
   move_test_t aTest;
   test_func_t func;
   int equality, k;
   k = 0;

   while ((func = move_tests[k].test_func)) {
      unsigned int * pv;
      int startA, startB;
      unsigned int * inA, * inB, * dst;
      unsigned long long exp_out;
      int idx;
      aTest = move_tests[k];
      exp_out = aTest.expected_result;
      startA = aTest.xa_idx;
      startB = aTest.xb_idx;
      pv = (unsigned int *)&vec_out;
      inA = &viargs[startA];
      inB = &viargs[startB];
      memcpy(&vec_inA, inA, sizeof(vector unsigned char));
      memcpy(&vec_inB, inB, sizeof(vector unsigned char));
      // clear vec_out
      for (idx = 0; idx < 4; idx++, pv++)
         *pv = 0;

      // execute test insn
      (*func)();
      dst = (unsigned int*) &vec_out;

      printf( "%s:", aTest.name);
      printf( " %08x %08x %s", inA[0], inA[1], aTest.name);
      printf( " %08x %08xx", inB[0], inB[1]);
      printf(" => %08x %08x\n", dst[0], dst[1]);

      equality = 1;
      pv = (unsigned int *)&exp_out;
      for (idx = 0; idx < 2; idx++) {
         equality &= (dst[idx] == pv[idx]);
      }
      if (!equality) {
         printf( "FAILED: vector out is incorrect\n" );
         errors++;
      }
      k++;
      printf( "\n" );
   }
}

static void test_permute_ops (void)
{
  permute_test_t *aTest;
  unsigned int *dst = (unsigned int *) &vec_out;

  for (aTest = &(permute_tests[0]); aTest->test_func != NULL; aTest++)
    {
      /* Grab test input and clear output vector.  */
      memcpy(&vec_inA, aTest->xa, sizeof(vec_inA));
      memcpy(&vec_inB, aTest->xb, sizeof(vec_inB));
      memset(dst, 0, sizeof(vec_out));

      /* execute test insn */
      aTest->test_func();

      printf( "%s:\n", aTest->name);
      printf( "        XA[%08x,%08x,%08x,%08x]\n",
              aTest->xa[0], aTest->xa[1], aTest->xa[2], aTest->xa[3]);
      printf( "        XB[%08x,%08x,%08x,%08x]\n",
              aTest->xb[0], aTest->xb[1], aTest->xb[2], aTest->xb[3]);
      printf( "   =>   XT[%08x,%08x,%08x,%08x]\n",
              dst[0], dst[1], dst[2], dst[3]);

      if (memcmp (dst, &aTest->expected_output, sizeof(vec_out)))
       {
         printf( "FAILED: vector out is incorrect\n" );
         errors++;
       }
    }
  printf( "\n" );
}

static test_table_t all_tests[] = { { &test_ldst,
                                       "Test VSX load/store instructions" },
                                     { &test_vsx_logic,
                                       "Test VSX logic instructions" },
#ifdef __powerpc64__
                                     { &test_ldbrx,
                                       "Test ldbrx instruction" },
                                     { &test_popcntd,
                                       "Test popcntd instruction" },
#endif
                                     { &test_lfiwzx,
                                       "Test lfiwzx instruction" },
                                     { &test_p7_fpops,
                                       "Test P7 floating point convert instructions"},
                                     { &test_ftdiv,
                                       "Test ftdiv instruction" },
                                     { &test_move_ops,
                                       "Test VSX move instructions"},
                                     { &test_permute_ops,
                                       "Test VSX permute instructions"},
                                     { &test_vx_fp_ops,
                                       "Test VSX floating point instructions"},
                                     { &test_xs_conv_ops,
                                       "Test VSX scalar integer conversion instructions" },
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
   if (errors)
      printf("Testcase FAILED with %d errors \n", errors);
   else
      printf("Testcase PASSED\n");

#endif // HAS _VSX

   return 0;
}
