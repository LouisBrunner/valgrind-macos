/*
 * ppc64_helpers.h
 * Copyright (C) 2016-2017 Will Schmidt <will_schmidt@vnet.ibm.com>
 *
 * This file contains helper functions for the ISA 3.0 test suite.
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

#include "tests/malloc.h"       // memalign32

typedef uint64_t  HWord_t;

#if defined (DEBUG_ARGS_BUILD)
#define AB_DPRINTF(fmt, args...) do { fprintf(stderr, fmt , ##args); } while (0)
#else
#define AB_DPRINTF(fmt, args...) do { } while (0)
#endif

/* Exhaustive tests?
 * Due to the excessive size of the test results, allow a #ifdef to
 * enable/disable most of the input values.
 * Off by default.
 */
// #define EXHAUSTIVE_TESTS 1


#define ALLCR "cr0","cr1","cr2","cr3","cr4","cr5","cr6","cr7"

#define SET_CR(_arg) \
      __asm__ __volatile__ ("mtcr  %0" : : "b"(_arg) : ALLCR );

#define SET_CR0_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x80,%0 " : : "b" (_arg):"cr0");
#define SET_CR1_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x40,%0 " : : "b" (_arg):"cr1");
#define SET_CR2_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x20,%0 " : : "b" (_arg):"cr2");
#define SET_CR3_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x10,%0 " : : "b" (_arg):"cr3");
#define SET_CR4_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x08,%0 " : : "r" (_arg):"cr4");
#define SET_CR5_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x04,%0 " : : "r" (_arg):"cr5");
#define SET_CR6_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x02,%0 " : : "r" (_arg):"cr6");
#define SET_CR7_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x01,%0 " : : "r" (_arg):"cr7");

#define SET_XER(_arg) \
      __asm__ __volatile__ ("mtxer %0" : : "b"(_arg) : "xer" );

#define GET_CR(_lval) \
      __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) )

#define GET_XER(_lval) \
      __asm__ __volatile__ ("mfxer %0" : "=b"(_lval) )

#define SET_CR_ZERO \
      SET_CR(0)

#define SET_FPSCR_ZERO                                        \
   do {                                                       \
      double _d = 0.0;                                        \
      __asm__ __volatile__ ("mtfsf 0xFF, %0" : : "f"(_d) );   \
   } while (0);

#define GET_FPSCR(_arg) \
  __asm__ __volatile__ ("mffs %0"  : "=f"(_arg) );

/*  The bit definitions for the FPSCR are as follows.
Bit(s) Description
0:31 Reserved
32 Floating-Point Exception Summary (FX)
33 Floating-Point Enabled Exception Summary (FEX)
34 Floating-Point Invalid Operation Exception Summary (VX)
35 Floating-Point Overflow Exception (OX)
36 Floating-Point Underflow Exception (UX)
37 Floating-Point Zero Divide Exception (ZX)
38 Floating-Point Inexact Exception (XX)
39 Floating-Point Invalid Operation Exception (SNaN) (VXSNAN)
40 Floating-Point Invalid Operation Exception (∞ - ∞) (VXISI)
41 Floating-Point Invalid Operation Exception (∞ ÷ ∞) (VXIDI)
42 Floating-Point Invalid Operation Exception (0 ÷ 0) (VXZDZ)
43 Floating-Point Invalid Operation Exception (∞ × 0) (VXIMZ)
44 Floating-Point Invalid Operation Exception (Invalid Compare) (VXVC)
45 Floating-Point Fraction Rounded (FR)
46 Floating-Point Fraction Inexact (FI)
47:51 Floating-Point Result Flags (FPRF)
47 Floating-Point Result Class Descriptor (C)
48:51 Floating-Point Condition Code (FPCC)
      48 Floating-Point Less Than or Negative (FL or <)
      49 Floating-Point Greater Than or Positive (FG or >)
      50 Floating-Point Equal or Zero (FE or =)
      51 Floating-Point Unordered or NaN (FU or ?)
52 Reserved
53 Floating-Point Invalid Operation Exception (Software-Defined Condition) (VXSOFT)
54 Floating-Point Invalid Operation Exception (Invalid Square Root) (VXSQRT)
55 Floating-Point Invalid Operation Exception (Invalid Integer Convert) (VXCVI)
56 Floating-Point Invalid Operation Exception Enable (VE)
57 Floating-Point Overflow Exception Enable (OE)
58 Floating-Point Underflow Exception Enable (UE)
59 Floating-Point Zero Divide Exception Enable (ZE)
60 Floating-Point Inexact Exception Enable (XE)
61 Floating-Point Non-IEEE Mode (NI)
62:63 Floating-Point Rounding Control (RN)
   00 Round to Nearest
   01 Round toward Zero
   10 Round toward +Infinity
   11 Round toward -Infinity
*/
/* NOTE, currently Valgrind only tracks the rounding mode, C and FPCC fields in the
 *       FPSCR register.
 */

static char * fpscr_strings[] = {
" 0-RSVD", " 1-RSVD", " 2-RSVD", " 3-RSVD", " 4-RSVD", " 5-RSVD", " 6-RSVD",
" 7-RSVD", " 8-RSVD", " 9-RSVD", "10-RSVD", "11-RSVD", "12-RSVD", "13-RSVD",
"14-RSVD", "15-RSVD", "16-RSVD", "17-RSVD", "18-RSVD", "19-RSVD", "20-RSVD",
"21-RSVD", "22-RSVD", "23-RSVD", "24-RSVD", "25-RSVD", "26-RSVD", "27-RSVD",
"28-RSVD", "29-DRN0", "30-DRN1", "31-DRN2",
/* 32 */ "FX", "FEX", "VX",
/* 35 */ "OX", "UX", "ZX", "XX", "VXSNAN",
/* 40 */ "VXISI (inf-inf)", "VXIDI (inf/inf)", "VXZDZ (0/0)",
/* 43 */ "VXIMZ (inf*0)", "VXVC",
/* 45 */ "FR", "FI",
/* 47 */ "FPRF-C", "FPCC-FL", "FPCC-FG",
/* 50 */ "FPCC-FE", "FPCC-FU",
/* 52 */ "52-RSVD", "FXSOFT", "VXSQRT",
/* 55 */ "VXCVI", "VE", "OE", "UE", "ZE",
/* 60 */ "XE", "NI", "RN-bit62", "RN-bit63"
};

#define FPCC_C_BIT    (0x1 << (63-47))
#define FPCC_FL_BIT   (0x1 << (63-48))
#define FPCC_FG_BIT   (0x1 << (63-49))
#define FPCC_FE_BIT   (0x1 << (63-50))
#define FPCC_FU_BIT   (0x1 << (63-51))
#define FPCC_FPRF_MASK  FPCC_C_BIT|FPCC_FL_BIT|FPCC_FG_BIT|FPCC_FE_BIT|FPCC_FU_BIT

#define FPSCR_RN_BIT62   (0x1 << (63-62))
#define FPSCR_RN_BIT63   (0x1 << (63-63))

#define CRFIELD_BIT0 0x8
#define CRFIELD_BIT1 0x4
#define CRFIELD_BIT2 0x2
#define CRFIELD_BIT3 0x1

/* dissect_cr*:
 * display the condition register bits in a
 * human readable format.
 */

static inline int cr_overflow_set(unsigned this_cr) {
   return (this_cr & CRFIELD_BIT3);
}

static inline int cr_zero_set(unsigned this_cr) {
   return (this_cr & CRFIELD_BIT2);
}

static inline int cr_positive_set(unsigned this_cr) {
   return (this_cr & CRFIELD_BIT1);
}

static inline int cr_negative_set(unsigned this_cr) {
   return (this_cr & CRFIELD_BIT0);
}

/* __dissect_cr takes a bitfield directly, not the full condition register.
 * This is a helper for dissect_cr_rn.
 */
inline static void __dissect_cr(unsigned this_cr) {
if (cr_negative_set(this_cr))
   printf("%s(LT)", verbose ? " 0x1=Negative" : "");

if (cr_positive_set(this_cr))
   printf("%s(GT)", verbose ? " 0x2=Positive" : "");

if (cr_zero_set(this_cr))
   printf("%s(EQ)", verbose ? " 0x4=Zero" : "");

if (cr_overflow_set(this_cr))
   printf("%s(SO)", verbose ? " 0x8=Overflow" : "");
}

/* Extract one CR field */
static int extract_cr_rn(unsigned long local_cr,unsigned long rn) {
   unsigned int masked_cr;
   unsigned long shifted_value;

   shifted_value = local_cr >> ( ( (7 - rn) * 4 ) );
   masked_cr = shifted_value & 0xf;
   return masked_cr;
}

/* Display one CR field */
static void dissect_cr_rn(unsigned long local_cr, unsigned long rn) {
   unsigned int masked_cr;

   masked_cr = extract_cr_rn(local_cr, rn);
   __dissect_cr(masked_cr);
}

/* Display all of the CR fields... */
static void dissect_cr(unsigned long local_cr) {
   unsigned int crn;

   for (crn = 0; crn < 8; crn++) {
      dissect_cr_rn(local_cr, crn);
   }
}

/* dissect the fpscr bits that are valid under valgrind.
 * Valgrind tracks the C (FPSCR[47]), FPCC (FPSCR[48:51)
 * DRN (FPSCR[29:31]) and RN (FPSCR[62:63]).
 */
static void dissect_fpscr_valgrind(unsigned long local_fpscr) {
   int i;
   long mybit;

   /* Print DRN fields */
   for (i = 29; i < 32; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf(" %s",fpscr_strings[i]);
      }
   }

   /* Print C and FPCC fields */
   for (i = 47; i < 52; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf(" %s",fpscr_strings[i]);
      }
   }

   /* Print RN field */
   for (i = 62; i < 64; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf(" %s",fpscr_strings[i]);
      }
   }
}

/* dissect the fpscr bits.
 * This prints the entire FPSCR field.  This is only called under higher
 * verbosities, as valgrind does not track most of these bits.
 */
static void dissect_fpscr_raw(unsigned long local_fpscr) {
/* Due to the additional involved logic, the rounding mode (RN) bits 61-62
 * are handled within dissect_fpscr_rounding_mode(). */
   int i;
   long mybit;

   for (i = 0; i < 61; i++) {
      /* also note that the bit numbering is backwards. */
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf(" %s", fpscr_strings[i]);
      }
   }
}

static void dissect_fpscr(unsigned long local_fpscr) {
   if (verbose > 1) {
      printf(" [[ fpscr:%lx ]] ", local_fpscr);
      dissect_fpscr_raw(local_fpscr);
   } else {
      dissect_fpscr_valgrind(local_fpscr);
   }
}

/* Display the rounding mode */
static void dissect_fpscr_rounding_mode(unsigned long local_fpscr) {
   /* special case handing for the rounding mode round-nearest (RN) bits. 62:63 */
   printf("Rounding Mode: ");

   if (local_fpscr & FPSCR_RN_BIT62)
      if (local_fpscr & FPSCR_RN_BIT63)
         /* 0b11 */ printf("RN-to--INF");
      else
         /* 0b10 */ printf("RN-to-+INF");
   else
      if (local_fpscr & FPSCR_RN_BIT63)
         /* 0b01 */ printf("RN-to-Nearest");
      else
         /* 0b00 */ printf("RN-to-Zero");
}

/*
 * Arithmetic, rounding, and Convert From Integer instructions will set
 * bits in the FPCC field to indicate the class of the result.
 * The table is described as follows;
     flags / Result value class
  C < > = ?
  1 0 0 0 1 Quiet NaN
  0 1 0 0 1 -Infinity
  0 1 0 0 0 -Normalized Number
  1 1 0 0 0 -Denormalized Number
  1 0 0 1 0 -Zero
  0 0 0 1 0 +Zero
  1 0 1 0 0 +Denormalized Number
  0 0 1 0 0 +Normalized Number
  0 0 1 0 1 +Infinity
*/

static void dissect_fpscr_result_value_class(unsigned long local_fpscr) {
   if (local_fpscr & FPCC_C_BIT) {
      if (local_fpscr & FPCC_FL_BIT)
            printf("-Denormalized");

      else if (local_fpscr & FPCC_FG_BIT)
            printf("+Denormalized");

      else if (local_fpscr & FPCC_FE_BIT)
            printf("-Zero        ");

      else if (local_fpscr & FPCC_FU_BIT)
            printf("Quiet NaN    ");

   } else {
      if (local_fpscr & FPCC_FL_BIT) {
         if (local_fpscr & FPCC_FU_BIT)
            printf("-Infinity    ");

         else
            printf("-Normalized  ");

      } else if (local_fpscr & FPCC_FG_BIT) {
         if (local_fpscr & FPCC_FU_BIT)
            printf("+Infinity    ");

         else
            printf("+Normalized  ");

         if (local_fpscr & FPCC_FE_BIT)
            printf("+Zero        ");
      }
   }
}

/* Interpret the fields in the FPCC as they apply to the DCMX checks.
 * The 'Match' indicator will typically be evaluated by the caller.
 *
 *  DMCX:
    * DCMX bit / 0x value / Data Class
    *        0       0x01  NaN
    *        1       0x02  +Infinity
    *        2       0x04  -Infinity
    *        3       0x08  +Zero
    *        4       0x10  -Zero
    *        5       0x20  +Denormal
    *        6       0x40  -Denormal
    *        7       0x7f  ALL bits set.
*/

static void dissect_fpscr_dcmx_indicator(unsigned long local_fpscr) {
   if (verbose > 2) printf("fpscr_cc:%lx ", local_fpscr & (FPCC_FPRF_MASK) );

   // See if the data class of the src value matches the set DCMX bits.
   if (verbose > 1) printf("%s ", (local_fpscr&FPCC_FE_BIT) ? "Match":"");

   // Display the sign bit of the src value.
   if (verbose > 1) printf("SRC sign:%s ", (local_fpscr&FPCC_FL_BIT) ? "-" : "+");

   // The src value can be either a SP or DP value, this indicates
   // if it is a valid SP value.
   if (verbose > 1) printf("%s ", (local_fpscr&FPCC_FE_BIT) ? "SP" : "");
}

/* dissect_xer helpers*/
static char * xer_strings[] = {
" 0-RSVD", " 1-RSVD", " 2-RSVD", " 3-RSVD", " 4-RSVD", " 5-RSVD", " 6-RSVD",
" 7-RSVD", " 8-RSVD", " 9-RSVD", "10-RSVD", "11-RSVD", "12-RSVD", "13-RSVD",
"14-RSVD", "15-RSVD", "16-RSVD", "17-RSVD", "18-RSVD", "19-RSVD",
"20-RSVD", "21-RSVD", "22-RSVD", "23-RSVD", "24-RSVD", "25-RSVD",
"26-RSVD", "27-RSVD", "28-RSVD", "29-RSVD", "30-RSVD", "31-RSVD",
/* 32 */ "SO", "OV", "CA",
/* 35 */ "35-RSVD", "36-RSVD", "37-RSVD", "38-RSVD", "39-RSVD",
/* 40 */ "40-RSVD", "41-RSVD", "42-RSVD", "43-RSVD",
/* 44 */ "OV32", "CA32",
/* 46 */ "46-RSVD", "47-RSVD", "48-RSVD", "49-RSVD", "50-RSVD", "51-RSVD",
         "52-RSVD", "53-RSVD", "54-RSVD", "55-RSVD", "56-RSVD",
/* 57:63 # bytes transferred by a Load/Store String Indexed instruction. */
         "LSI/SSI-0", "LSI/SSI-1", "LSI/SSI-2", "LSI/SSI-3",
         "LSI/SSI-4", "LSI/SSI-5", "LSI/SSI-6",
};

/* Dissect the XER register contents.
 */
static void dissect_xer_raw(unsigned long local_xer) {
   int i;
   long mybit;

   for (i = 0; i <= 63; i++) {
      mybit = 1ULL << (63 - i); /* compensate for reversed bit numbering. */
      if (mybit & local_xer)
         printf(" %s", xer_strings[i]);
   }
}
/* Display only the XER contents that are relevant for our tests.
 * this is currently the OV and OV32 bits.  */
static void dissect_xer_valgrind(unsigned long local_xer) {
   int i;
   long mybit;
      i = 33;  // OV
      mybit = 1ULL << (63 - i);
      if (mybit & local_xer) printf(" %s", xer_strings[i]);
      i = 44;  // OV32
      mybit = 1ULL << (63 - i);
      if (mybit & local_xer) printf(" %s", xer_strings[i]);
}


/* */
static void dissect_xer(unsigned long local_xer) {
   if (verbose > 1)
      printf(" [[ xer:%lx ]]", local_xer);
   if (verbose > 2 )
      dissect_xer_raw(local_xer);
   else
      dissect_xer_valgrind(local_xer);
}


/* DFP helpers for bcd-to-dpd, dpd-to-bcd, misc.
 * pulled from vex/.../host_generic_simd64.c
 */
/*------------------------------------------------------------------*/
/* Decimal Floating Point (DFP) helper functions */
/*------------------------------------------------------------------*/
#define NOT( x )    ( ( ( x ) == 0) ? 1 : 0)
#define GET( x, y ) ( ( ( x ) & ( 0x1UL << ( y ) ) ) >> ( y ) )
#define PUT( x, y ) ( ( x )<< ( y ) )

static unsigned long dpb_to_bcd( unsigned long chunk )
{
   int a, b, c, d, e, f, g, h, i, j, k, m;
   int p, q, r, s, t, u, v, w, x, y;
   unsigned long value;

   /* convert 10 bit densely packed BCD to BCD */
   p = GET( chunk, 9 );
   q = GET( chunk, 8 );
   r = GET( chunk, 7 );
   s = GET( chunk, 6 );
   t = GET( chunk, 5 );
   u = GET( chunk, 4 );
   v = GET( chunk, 3 );
   w = GET( chunk, 2 );
   x = GET( chunk, 1 );
   y = GET( chunk, 0 );

   /* The BCD bit values are given by the following boolean equations.*/
   a = ( NOT(s) & v & w ) | ( t & v & w & s ) | ( v & w & NOT(x) );
   b = ( p & s & x & NOT(t) ) | ( p & NOT(w) ) | ( p & NOT(v) );
   c = ( q & s & x & NOT(t) ) | ( q & NOT(w) ) | ( q & NOT(v) );
   d = r;
   e = ( v & NOT(w) & x ) | ( s & v & w & x ) | ( NOT(t) & v & x & w );
   f = ( p & t & v & w & x & NOT(s) ) | ( s & NOT(x) & v ) | ( s & NOT(v) );
   g = ( q & t & w & v & x & NOT(s) ) | ( t & NOT(x) & v ) | ( t & NOT(v) );
   h = u;
   i = ( t & v & w & x ) | ( s & v & w & x ) | ( v & NOT(w) & NOT(x) );
   j = ( p & NOT(s) & NOT(t) & w & v ) | ( s & v & NOT(w) & x )
      | ( p & w & NOT(x) & v ) | ( w & NOT(v) );
   k = ( q & NOT(s) & NOT(t) & v & w ) | ( t & v & NOT(w) & x )
      | ( q & v & w & NOT(x) ) | ( x & NOT(v) );
   m = y;

   value = PUT(a, 11) | PUT(b, 10) | PUT(c, 9) | PUT(d, 8) | PUT(e, 7)
               | PUT(f, 6) | PUT(g, 5) | PUT(h, 4) | PUT(i, 3) | PUT(j, 2)
               | PUT(k, 1) | PUT(m, 0);
    return value;
}
#undef NOT
#undef GET
#undef PUT


typedef union dfp_union {
   _Decimal128  dec_val128;
   struct {
#if defined(VGP_ppc64le_linux)
   unsigned long vall;
   unsigned long valu;
#else
   unsigned long valu;
   unsigned long vall;
#endif
   } u128;
} dfp_val_t;

/* Based on and enhanced from the dfp128_vals table in test_dfp5.c.
 * Todo: Refine/refactor and turn into a build_table function.
 */

static unsigned long dfp128_vals[] = {
#ifdef EXHAUSTIVE_TESTS
   // Some finite numbers
   0x2208000000000000ULL, 0x0000000000000001ULL, //  1 *10^0
   0xa208800000000000ULL, 0x0000000000000001ULL, // -1 *10^1
   0x0000000000000000ULL, 0x0000000000000001ULL, //  1 *10^-6176. (smallest exp)
   0x43ffc00000000000ULL, 0x0000000000000001ULL, //  1 *10^6111
   0x6fffc00000000000ULL, 0x0000000000000001ULL, // foo *10^2015.
   0x67ffc00000000000ULL, 0x0000000000000001ULL, // foo *10^-2081.
   0x77ffc00000000000ULL, 0x0000000000000001ULL, //  1 *10^6111 (largest exp)
   0x77ffffffffffffffULL, 0xffffffffffffffffULL, // max possible value *10^6111 (largest exp)
   0x0000000000000000ULL, 0x0000000000000001ULL, // min possible value 1 *10^-6176. (smallest exp)
   0x8000000000000000ULL, 0x0000000000000001ULL, // -1 *10^-6176. (smallest exp)

   /* data bits sprinkled across the significand field. */
   0xa208800001000000ULL, 0x0000000000010000ULL, //-foo *10^1
   0xa208800000000100ULL, 0x0000000000000100ULL, //-foo *10^1
   0xa208800000000000ULL, 0x0000100000000000ULL, //-foo *10^1
   0xa208800000000000ULL, 0x0000000001000000ULL, //-foo *10^1
   0xa208800000000000ULL, 0x0000000000000001ULL, //-foo *10^1

   // pre-existing dfp128 values:
   0x2207c00000000000ULL, 0x0000000000000e50ULL, // foo * 10^-1
   0x2207c00000000000ULL, 0x000000000014c000ULL, // foo * 10^-1
   0xa207c00000000000ULL, 0x00000000000000e0ULL, // foo * 10^-1
   0x2206c00000000000ULL, 0x00000000000000cfULL, // foo * 10^-5
   0xa205c00000000000ULL, 0x000000010a395bcfULL, // foo * 10^-9
   0x6209400000fd0000ULL, 0x00253f1f534acdd4ULL, // foo * 10^-4091
   0x000400000089b000ULL, 0x0a6000d000000049ULL, // very small number // foo * 10^-6160

   // flavors of zero
   0x2208000000000000ULL, 0x0000000000000000ULL, // 0*10^256
   0xa208000000000000ULL, 0x0000000000000000ULL, // -0*10^0
   0xa248000000000000ULL, 0x0000000000000000ULL, // 0*10^256

   // flavors of NAN
   0x7c00000000000000ULL, 0x0000000000000000ULL, // quiet
   0xfc00000000000000ULL, 0xc00100035b007700ULL, // NAN
   0x7e00000000000000ULL, 0xfe000000d0e0a0d0ULL, // signaling NAN

   // flavors of Infinity
   0x7800000000000000ULL, 0x0000000000000000ULL, // +inf
   0xf800000000000000ULL, 0x0000000000000000ULL, // -inf
   0xf900000000000000ULL, 0x0000000000000000ULL  // -inf
#else
   0x2208000000000000ULL, 0x0000000000000001ULL, //  1 *10^0
   0x77ffffffffffffffULL, 0xffffffffffffffffULL, // max possible value *10^6111 (largest exp)
   0xa208000000000000ULL, 0x0000000000000000ULL, // -0*10^0
   0xfc00000000000000ULL, 0xc00100035b007700ULL, // NAN
   0x7e00000000000000ULL, 0xfe000000d0e0a0d0ULL, // signaling NAN
   0xf800000000000000ULL, 0x0000000000000000ULL, // -inf
#endif
};

#define NUM_DFP128_VALS (sizeof(dfp128_vals) / sizeof(unsigned long))
unsigned long nb_dfp128_vals = NUM_DFP128_VALS;

/* Todo: update dfp64_vals to match dfp128_vals content. */

static unsigned long dfp64_vals[] = {
#ifdef EXHAUSTIVE_TESTS
   0x77fcffffffffffffULL, // max possible value 9..9 *10^369 (largest exp)
   0x0000000000000001ULL, // min possible nonzero value 1 *10^-398. (smallest exp)
   0x4248000000000001ULL, // 1*10^260
   0x2234000000000e50ULL, // foo*10^-1
   0x223400000014c000ULL, //
   0xa2340000000000e0ULL, //
   0x22240000000000cfULL, // foo*10^-5
   0xa21400010a395bcfULL, // negative -foo*10^-9
   0x6e4d3f1f534acdd4ULL, // huge number foo*10^5
   0x000400000089b000ULL, // very small number foo*10^-397

   // flavors of zero
   0x2238000000000000ULL,
   0xa238000000000000ULL, // 0 * 10 ^0
   0x4248000000000000ULL, // 0 * 10 ^260

   // flavors of NAN
   0x7e34000000000111ULL, //signaling NaN
   0xfe000000d0e0a0d0ULL, //signaling NaN
   0xfc00000000000000ULL, //quiet NaN

   // flavors of Infinity
   0x7800000000000000ULL, //+Inf
   0xf800000000000000ULL, //-Inf
   0x7a34000000000000ULL, //+Inf
#else
   0x77fcffffffffffffULL, // max possible value 9..9 *10^369 (largest exp)
   0x4248000000000000ULL, // 0 * 10 ^260
   0xfe000000d0e0a0d0ULL, //signaling NaN
   0xf800000000000000ULL, //-Inf
#endif
};

#define NUM_DFP64_VALS (sizeof(dfp64_vals) / sizeof(unsigned long))
unsigned long nb_dfp64_vals = NUM_DFP64_VALS;

/* shift helpers */
#define SH_0  0
#define SH_1  1
#define SH_2  15
#define SH_3  63

static uint64_t shift_amounts[] = {
   SH_0,
   SH_1,
   SH_2,
   SH_3,
#define SHIFT_ARRAY_SIZE 4
};

/* vector splat helpers */
#define SPLAT0 0
#define SPLAT1 1
#define SPLAT2 0xaa
#define SPLAT3 0x55
#define SPLAT4 0xff

static uint64_t splat_values[] = {
   SPLAT0,
   SPLAT1,
   SPLAT2,
   SPLAT3,
   SPLAT4,
#define SPLAT_ARRAY_SIZE 5
};

/* a small memory range used to test load-from and store-to vsx */
#define BUFFER_SIZE 4
#define MAX_BUFFER_PATTERNS 6
unsigned long buffer[BUFFER_SIZE];

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

#define PATTERN_SIZE 5
unsigned long pattern[PATTERN_SIZE] = {
	0xffffffffffffffff,
	0xaaaaaaaaaaaaaaaa,
	0x5152535455565758,
	0x0000000000000000,
	0xffaa5599113377cc,
};


static void dump_small_buffer(void) {
   int x;

   printf("[ ");

   for (x = 0; x < BUFFER_SIZE; x++)
      printf("%016lx ", buffer[x] );

   printf("]");
}

/* value to be shifted */
static uint64_t values_to_shift[] = {
                  0x0,
                  0x1,
                 0x10,
                0x100,
               0x1000,
              0x10000,
             0x100000,
            0x1000000,
           0x10000000,
          0x100000000,
         0x1000000000,
        0x10000000000,
       0x100000000000,
      0x1000000000000,
     0x10000000000000,
    0x100000000000000,
   0x1000000000000000,
                  0xf,
                 0x1f,
                0x10f,
               0x100f,
              0x1000f,
             0x10000f,
            0x100000f,
           0x1000000f,
          0x10000000f,
         0x100000000f,
        0x1000000000f,
       0x10000000000f,
      0x100000000000f,
     0x1000000000000f,
    0x10000000000000f,
   0x100000000000000f,
                  0x7,
                 0x70,
                0x700,
               0x7000,
              0x70000,
             0x700000,
            0x7000000,
           0x70000000,
          0x700000000,
         0x7000000000,
        0x70000000000,
       0x700000000000,
      0x7000000000000,
     0x70000000000000,
    0x700000000000000,
   0x7000000000000000,
                  0x8,
                 0x80,
                0x800,
               0x8000,
              0x80000,
             0x800000,
            0x8000000,
           0x80000000,
          0x800000000,
         0x8000000000,
        0x80000000000,
       0x800000000000,
      0x8000000000000,
     0x80000000000000,
    0x800000000000000,
   0x8000000000000000,
   0xffffffffffffffff,
   0
#define SHIFT_VALUES_SIZE 66
};

/* DFP related helper functions: */

/* For DFP finite numbers, the combination field (G field) is a
 * combination of the exponent and the LMD (Left Most Digit) of the
 * significand.  The fields are encoded/decoded as described in the
 * table here.
 *       00       01      10   -< Exponent bits.
 * 0:   00000   01000   10000
 * ...
 * 7:   00111   01111   10111
 * 8:   11000   11010   11100
 * 9:   11001   11011   11101  (encoded special field).
 * |
 * ^ LMD value.
*/
#define DFP_GFIELD_MASK  0x7c00000000000000UL
#define DFP_GFIELD_SHIFT 58

static unsigned int special_field_LMD(uint64_t dword1) {
   unsigned long g_field_specials;
   int left_two_bits;
   int right_three_bits;

   g_field_specials = (dword1 & DFP_GFIELD_MASK) >> DFP_GFIELD_SHIFT;
   left_two_bits = (g_field_specials & 0x18) >> 3;
   right_three_bits = g_field_specials & 0x07;

   /* The LMD result maps directly to the right_three_bits value as
    * long as the left two bits are 0b00,0b01,0b10.  So a compare
    * against 3 is sufficient to determine if we can return the right
    * three bits directly.  (LMD values 0..7).
    */
   if (left_two_bits < 3) {
      return (right_three_bits);
   }

   /* LMD values of 8 or 9 require a bit of swizzle, but a check of
    * the right-most bit is sufficient to determine whether LMD value
    * is 8 or 9.
    */
   if (right_three_bits & 0x1)
      return 9;
   else
      return 8;
}

/* Returns the exponent bits, as decoded from the G field. */
static inline int special_field_exponent_bits(unsigned long dword1) {
   unsigned long g_field_specials;
   int left_two_bits;
   int right_three_bits;

   g_field_specials = (dword1 & DFP_GFIELD_MASK) >> DFP_GFIELD_SHIFT;
   left_two_bits = (g_field_specials & 0x18) >> 3;
   right_three_bits = g_field_specials & 0x07;

   /* The special field exponent bits maps directly to the left_two_bits
    * value as long as the left two bits are 0b00,0b01,0b10.  So a compare
    * against 3 is sufficient for those values.
    */
   if (left_two_bits < 3) {
      return (left_two_bits);
   }

   switch(right_three_bits) {
      case 0:
      case 1: return 0x0;
      case 2:
      case 3: return 0x1;
      case 4:
      case 5: return 0x2;
      case 6: /* Infinity */ return 0x0;
      case 7: /* NaN */  return 0x0;
   }
   return -1;  /* should never hit this */
}

/* get_declet().  Return a 10-bit declet, beginning at the 'start'
 * offset.
 *
 * | dword1 | dword0 |
 * | 0    63|64   127|
 */
#define TEN_BITS 0x03ffULL

static inline int get_declet(int start, uint64_t dword1, uint64_t dword0) {
   unsigned long local_declet;
   unsigned int dword0_shift;
   unsigned int dword1_shift;

   dword1_shift = 63 - (start + 9);
   dword0_shift = 127 - (start + 9);

   if (verbose>5) printf("\n%s (%d) %016lx %016lx",
                         __FUNCTION__, start, dword1, dword0);

   if ((start + 9) < 63) { /* fully within dword1 */
      local_declet = (dword1 >> dword1_shift) & TEN_BITS;

   } else if (start >= 65) {/* fully within dword0 */
      local_declet = (dword0 >> dword0_shift) & TEN_BITS;

   } else { /* straddling the two dwords*/
      unsigned long mask_dword0;
      unsigned long mask_dword1;

      mask_dword1 = TEN_BITS >> (64 - dword0_shift);
      mask_dword0 = TEN_BITS << (dword0_shift);
      local_declet =
         ((dword1 & mask_dword1) << (64-dword0_shift)) +
         ((dword0 & mask_dword0) >> dword0_shift);
   }
   return local_declet;
}

static int get_bcd_digit_from_dpd(int start, uint64_t dword1,
                                  uint64_t dword0) {
   long bcd_digit;
   long declet;

   declet = get_declet(start, dword1, dword0);
   bcd_digit = dpb_to_bcd(declet);
   return bcd_digit;
}


/* The 'exponent left' shift is for moving the leftmost two bits
 * of the exponent down to where they can be easily merged with the
 * rest of the exponent.
 */
#define DFP128_EXPONENT_RIGHT_MASK       0x03ffc00000000000
#define DFP64_EXPONENT_RIGHT_MASK        0x03fc000000000000
#define DFP128_EXPONENT_RIGHT_MASK_SHIFT 46
#define DFP64_EXPONENT_RIGHT_MASK_SHIFT  50
#define DFP128_EXPONENT_LEFT_SHIFT       12
#define DFP64_EXPONENT_LEFT_SHIFT         8

#define DFP_NAN                          0x1f
#define DFP_INF                          0x1e
#define DFP_SIGNALING_NAN_BIT            0x0200000000000000

/* Start of the Trailing Significand field is at bit # .. */
#define DFP128_T_START         18
#define DFP64_T_START          14

//The exponent bias value is 101 for DFP Short, 398
//for DFP Long, and 6176 for DFP Extended.
#define DFP128_EXPONENT_BIAS 6176
#define DFP64_EXPONENT_BIAS   398

/* return the dfp exponent from the leading dword. */
static inline signed long dfp128_exponent(unsigned long dword1) {
   unsigned long exponent_left;
   unsigned long exponent_right;
   unsigned long biased_exponent;
   signed long exponent;

   exponent_left = special_field_exponent_bits(dword1);
   exponent_right = (dword1 & DFP128_EXPONENT_RIGHT_MASK);
   biased_exponent = (exponent_left << DFP128_EXPONENT_LEFT_SHIFT) +
                     (exponent_right >> DFP128_EXPONENT_RIGHT_MASK_SHIFT);

   /* Unbias the exponent. */
   exponent = biased_exponent - DFP128_EXPONENT_BIAS;

   return exponent;
}

/* Interpret the paired 64-bit values as a extended (quad) 128 bit DFP.
 *
 * | Significand | Combination Field/    |                          |
 * | sign bit    | Encoded Exponent      | remainder of significand |
 * |0            |1                    17|18                     127|
 *  ^ (bit0) Significand sign bit.
 *                ^ (bit 1:17) Combination field. Contains high bits of
 *                  exponent (encoded), LMD of significand (encoded),
 *                  and the remainder of the exponent.  First five bits
 *                  will indicate special cases NAN or INF.
 *                                   ^ (bit 18:127) Remainder of the
 *                                     significand.
 */

#define DFP128_COMBINATION_MASK      0x7fffc
#define DFP64_COMBINATION_MASK       0x7ffc
#define DFP128_COMBINATION_SHIFT     46
#define DFP64_COMBINATION_SHIFT      50
#define DFP_SPECIAL_SYMBOLS_MASK     0x1f
#define DFP_SPECIAL_SYMBOLS_SHIFT    58

static inline void dissect_dfp128_float(uint64_t dword1, uint64_t dword0) {
   long signbit;
   signed long exponent;
   unsigned long gfield_special_symbols;
   unsigned long lmd_digit;
   unsigned long bcd_digits[13];
   int i;
   int silent=0; // suppress leading zeros from the output.

   if (verbose > 5) printf("RAW128: %016lx %016lx ", dword1, dword0);

   signbit = (dword1 >> 63);

   if (signbit) printf("-");
   else         printf("+");

   gfield_special_symbols =
      ((dword1 >> DFP_SPECIAL_SYMBOLS_SHIFT) & DFP_SPECIAL_SYMBOLS_MASK);

   switch (gfield_special_symbols) {
      case DFP_INF:
         printf(   "inf      ");
         break;

      case DFP_NAN:
         if (dword1 & DFP_SIGNALING_NAN_BIT)
            printf("SNaN     ");
         else
            printf("QNaN     ");
         break;

      default:
         printf(   "Finite   ");
         exponent  = dfp128_exponent(dword1);
         lmd_digit = special_field_LMD(dword1);

         for (i = 0; i < 11; i++) {
            bcd_digits[i] = get_bcd_digit_from_dpd((DFP128_T_START
                                                    + 10 * i), dword1, dword0);
         }

         if (lmd_digit) {
            silent++;
            printf("%01lx", lmd_digit);

         } else {
            printf(" ");
         }

         for (i = 0; i < 11; i++) {
            if (bcd_digits[i] || silent ) {
               silent++;
               printf("%03lx", bcd_digits[i]);

            } else {
               /* always print at least the last zero */
               if (i == 10)
                  printf("  0");

               else
                  printf("   ");
            }
         }
         printf(" * 10 ^ ");
         printf("%ld ", exponent);
   }
}

/* Interpret the 64-bit values as a 64 bit DFP.
*
* | Significand | Combination Field/    |                          |
* | sign bit    | Encoded Exponent      | remainder of significand |
* |0            |1                    13|14                      63|
*  ^ (bit0) Significand sign bit.
*                ^ (bit 1:13) Combination field. Contains high bits of
*                  exponent (encoded), LMD of significand (encoded),
*                  and the remainder of the exponent.  First five bits
*                  will indicate special cases NAN or INF.
*                                        ^ (bit 14:63) Remainder of the
*                                          significand.
*/

/* return the dfp exponent from the leading dword. */
static inline signed long dfp64_exponent(unsigned long dword1) {
   unsigned long exponent_left;
   unsigned long exponent_right;
   unsigned long biased_exponent;
   signed long exponent;

   exponent_left = special_field_exponent_bits(dword1);
   exponent_right = (dword1 & DFP64_EXPONENT_RIGHT_MASK);
   biased_exponent = (exponent_left << DFP64_EXPONENT_LEFT_SHIFT) +
                     (exponent_right >> DFP64_EXPONENT_RIGHT_MASK_SHIFT);

   /* Unbias the exponent. */
   exponent = biased_exponent - DFP64_EXPONENT_BIAS;
   return exponent;
}

static inline void dissect_dfp64_float(uint64_t dword1) {
   long signbit;
   signed long exponent;
   unsigned long gfield_special_symbols;
   unsigned long lmd_digit;
   unsigned long bcd_digits[13];
   int i;
   int silent=0; // suppress leading zeros from the output.

   if (verbose > 5) printf("RAW64: %016lx ", dword1);

   signbit = (dword1 >> 63);

   if (signbit) printf("-");
   else         printf("+");

   gfield_special_symbols =
      ((dword1 >> DFP_SPECIAL_SYMBOLS_SHIFT) & DFP_SPECIAL_SYMBOLS_MASK);

   switch (gfield_special_symbols) {
      case DFP_INF:
         printf(   "inf      ");
         break;

      case DFP_NAN:
         if (dword1 & DFP_SIGNALING_NAN_BIT)
            printf("SNaN     ");
         else
            printf("QNaN     ");
         break;

      default:
         printf(   "Finite   ");
         exponent  = dfp64_exponent(dword1);
         lmd_digit = special_field_LMD(dword1);

         for (i = 0; i < 5; i++)
            bcd_digits[i] = get_bcd_digit_from_dpd((DFP64_T_START + 10 * i),
                                                   dword1, 0);

         if (lmd_digit) {
            silent++;
            printf("%01lx", lmd_digit);

         } else {
            printf(" ");
         }

         for (i = 0; i < 5; i++) {
            if (bcd_digits[i] || silent) {
               silent++;
               printf("%03lx", bcd_digits[i]);

            } else { // suppress leading zeros.
               /* always print at least the last zero */
               if (i == 4)
                  printf("  0");

               else
                  printf("   ");
            }
         }
         printf(" * 10 ^ ");
         printf("%ld ", exponent);
   }
}

static void dump_dfp128_table(void) {
   int i;

   printf("DFP 128 table:\n");

   for (i = 0; i < nb_dfp128_vals; i += 2) {
      printf("i=:%2d ", i);
      dissect_dfp128_float(dfp128_vals[i], dfp128_vals[i+1]);
      printf("\n");
   }
}

static void dump_dfp64_table(void) {
   int i;

   printf("DFP 64 table:\n");

   for (i = 0; i<nb_dfp64_vals; i++) {
      printf("i=:%2d ", i);
      dissect_dfp64_float(dfp64_vals[i]);
      printf("\n");
   }
}


/* Data Formats for floating point.
 * Floating point values include the following:
 *  -INF -NOR -DEN -0 +0 +DEN +NOR +INF
 *  INFinite: When the biased exponent is the MAX possible value, and
 *   the fraction field is 0.
 *  ZERo.    biased exponent is zero, fraction is 0.
 *  DENormalized.   biased exponent is 0, and fraction is non-zero.
 *  NORmalized. All other values that are neither Zero, Denormalized,
 *   or Infinite.  Biased exponent=1..MAX-1.
 */

/* Quad (128bit):
 * | Sign | EXPonent+Bias  | FRACTION/Mantissa |
 *  0      1             15 16              127
 *  exponent is 15 bits. ranging from:  0x0000 .. 0x7fff
 *     0 = (zero if fraction==0, DeNormal if fraction !=0 )
 *     1...0x7ffe = normalized
 *     7fff  =  (infinite if fraction==0, NaN if fraction !=0)
 */
#define QUAD_EXP_MASK 0x7fff

/* This assumes we are working on the top half of a quad stored in a 64-bit
 *  register.
 */
#define QUAD_EXP_SHIFT 48
#define QUAD_MANTISSA_MASK 0x0000ffffffffffff
static inline unsigned long build_binary128_float(unsigned long signbit,
                                                  unsigned long exponent,
                                                  unsigned long mantissa) {
   unsigned long thevalue;

   thevalue = (unsigned long) (signbit << 63) |
      ((exponent & QUAD_EXP_MASK) << QUAD_EXP_SHIFT) |
      (mantissa & QUAD_MANTISSA_MASK);

   if (verbose > 3)
      printf("%s %lx \n", __FUNCTION__, (unsigned long)thevalue);

   return thevalue;
}

 /* double (64bit):
 * | Sign | EXPonent+Bias  | FRACTION/Mantissa |
 *  0      1             11 12              63
 * exponent is 11 bits. ranging from:  0x000 .. 0x7ff
 *    0 = (zero if fraction==0, DeNormal if fraction !=0 )
 *    1...0x7fe = normalized
 *    7ff  =  (infinite if fraction==0, NaN if fraction !=0)
*/
#define DOUBLE_EXP_MASK 0x7ff
#define DOUBLE_EXP_SHIFT 52
#define DOUBLE_MANTISSA_MASK 0x000fffffffffffff

static inline unsigned long build_binary64_float(unsigned long signbit,
                                                 unsigned long exponent,
                                                 unsigned long mantissa) {
   unsigned long  thevalue;

   thevalue = (unsigned long ) (signbit << 63) |
      ((exponent & DOUBLE_EXP_MASK) << DOUBLE_EXP_SHIFT) |
      (mantissa & DOUBLE_MANTISSA_MASK );

   if (verbose > 3)
      printf("%s %lx \n", __FUNCTION__, (unsigned long)thevalue);

   return thevalue;
}

 /* floating point single (32bit):
 * | Sign | EXPonent+Bias  | FRACTION/Mantissa |
 *  0      1              8 9                31
 * exponent is 8 bits. ranging from:  0x00 .. 0xff
 *    0 = (zero if fraction==0, DeNormal if fraction !=0 )
 *    1...0x7e = normalized
 *    7f = (infinite if fraction==0, NaN if fraction !=0) */
#define SINGLE_EXP_MASK 0xff
#define SINGLE_EXP_SHIFT 23
#define SINGLE_MANTISSA_MASK 0x007fffff

/* This is building the 32-bit float. */
static inline unsigned long build_binary32_float(unsigned long signbit,
                                                 unsigned long exponent,
                                                 unsigned long mantissa) {
   unsigned long thevalue;
   unsigned long local_signbit;
   unsigned long local_exponent;
   unsigned long local_mantissa;

   local_signbit  = (signbit != 0) << 31;
   local_exponent = ((exponent & SINGLE_EXP_MASK) << SINGLE_EXP_SHIFT);
   local_mantissa = (mantissa & SINGLE_MANTISSA_MASK);

   thevalue = (unsigned long) (local_signbit) |
      (local_exponent) |
      (local_mantissa);

   if (verbose > 3)
      printf("%s %lx \n", __FUNCTION__, (unsigned long)thevalue);

   return thevalue;
}

/* floating point half (16bit):
 * | Sign | EXPonent+Bias  | FRACTION/Mantissa |
 *  0      1              6 7               15
 * exponent is 6 bits.  0x00 .. 0x7e masked with EXP_MASK
 *    0 = (zero if fraction==0, DeNormal if fraction !=0 )
 *    1...0x7d = normalized
 *    7e = (infinite if fraction==0, NaN if fraction !=0) */
/* when extracting the exponent from the 16-bit half-word, use this mask. */
#define HALF_EXP_MASK 0x7e00

/* when building the 16-bit half-word, mask against this,
 * then shift into place
 */
#define HALF_EXP_MASK_NORMALIZED 0x3f
#define HALF_EXP_SHIFT 9
#define HALF_MANTISSA_MASK 0x01ff

/* This is building the 16-bit float. */
static inline unsigned long build_binary16_float(unsigned long in_signbit,
                                                 unsigned long exponent,
                                                 unsigned mantissa) {
   unsigned long thevalue;
   unsigned long local_signbit;
   unsigned long local_exponent;
   unsigned long local_mantissa;

   local_signbit = (in_signbit != 0) << 15;

   local_exponent= ((exponent & HALF_EXP_MASK_NORMALIZED) << HALF_EXP_SHIFT);
   local_mantissa = (mantissa & HALF_MANTISSA_MASK);

   thevalue = (unsigned long) (local_signbit) | (local_exponent)
      | (local_mantissa);

   if (verbose > 3)
      printf("%s %lx \n", __FUNCTION__, (unsigned long)thevalue);

   return thevalue;
}

/* dissect_binary128_float:
 * Interpret the (high half) 64-bit value as normal/denormal/inf/NaN.
 * This is as it would be interpreted as the MSB portion of
 *  a 128-bit wide QUAD.
 */
static inline void dissect_binary128_float(uint64_t value) {
   unsigned long signbit;
   unsigned long exponent;
   unsigned long  mantissa;

   signbit  = (value >> 63);
   exponent = ( QUAD_EXP_MASK & (value >> QUAD_EXP_SHIFT));
   mantissa = ( QUAD_MANTISSA_MASK & value);

   if (verbose > 4) printf("128 bit:");

   if (signbit) printf("-");
   else         printf("+");

   switch (exponent) {
      case 0x0:
         if (mantissa == 0) printf("zero     ");
         else               printf("denormal ");
         break;

      case QUAD_EXP_MASK:
         if (mantissa == 0) printf("inf      ");
         else               printf("NaN      ");
         break;

      default:              printf("Normal   ");
   }

   if (verbose > 4)
      printf("%lx %4lx %16lx %16lx \n", signbit, exponent, mantissa, value);
}

/* Interpret the 64-bit value as normal/denormal/inf/NaN
 * this is as interpreted as the 64-bit float
 */
static inline void dissect_binary64_float(uint64_t value) {
   unsigned long signbit;
   unsigned long exponent;
   unsigned long mantissa;

   signbit  = (value >> 63); // bit0
   exponent = ( DOUBLE_EXP_MASK & (value >> DOUBLE_EXP_SHIFT));
   mantissa = ( DOUBLE_MANTISSA_MASK & value);

   if (verbose > 4) printf(" 64 bit:");

   if (signbit) printf("-");
   else         printf("+");

   switch (exponent) {
      case 0x0:
         if (mantissa == 0) printf("zero     ");
         else               printf("denormal ");
         break;

      case DOUBLE_EXP_MASK:
         if (mantissa == 0) printf("inf      ");
         else               printf("NaN      ");
         break;

      default:              printf("Normal   ");
   }

   if (verbose>4)
      printf("%lx %4lx %16lx %16lx\n", signbit, exponent, mantissa, value);
}

/* interpret the 32-bit value as normal/denormal/inf/NaN.
 * Note that the value is stored in the upper half of a
 * 64-bit, which is itself in the upper half of a quad.
 */
static inline void dissect_binary32_float(uint64_t value) {
   unsigned long signbit;
   unsigned long exponent;
   unsigned long mantissa;
   unsigned long adj_value;

   /* shift down to where the offsets make more sense.*/
   adj_value = value;   //>>32;
   signbit  = (adj_value >> 31);
   exponent = ( SINGLE_EXP_MASK & (adj_value >> SINGLE_EXP_SHIFT));
   mantissa = ( SINGLE_MANTISSA_MASK & adj_value);

   if (verbose > 4) printf(" 32 bit:");

   if (signbit) printf("-");
   else         printf("+");

   switch (exponent) {
      case 0x0:
         if (mantissa == 0) printf("zero     ");
         else               printf("denormal ");
         break;

      case SINGLE_EXP_MASK:
         if (mantissa == 0) printf("inf      ");
         else               printf("NaN      ");
         break;

      default:              printf("Normal   ");
   }

   if (verbose>4)
      printf("%lx %4lx %16lx %16lx \n", signbit, exponent, mantissa, adj_value);
}

/* Interpret the 16-bit value as normal/denormal/inf/NaN. */
static inline void dissect_binary16_float(uint64_t value) {
   unsigned long signbit;
   unsigned long exponent;
   unsigned long mantissa;
   unsigned long adj_value;

   adj_value = (value & 0xffff);
   signbit  = ((adj_value & 0x8000) > 1);
   exponent = ((adj_value & HALF_EXP_MASK ) >> HALF_EXP_SHIFT) ;
   mantissa = (adj_value & HALF_MANTISSA_MASK);

   if (verbose > 4) printf(" 16 bit:");

   if (signbit) printf("-");
   else         printf("+");

   switch (exponent) {
      case 0x0:
         if (mantissa == 0) printf("zero     ");
         else               printf("denormal ");
         break;

      case HALF_EXP_MASK:
         if (mantissa == 0) printf("inf      ");
         else               printf("NaN      ");
         break;

      default:              printf("Normal   ");
   }

   if (verbose > 4)
      printf("%lx %4lx %16lx %16lx \n",
             signbit, exponent>>HALF_EXP_SHIFT, mantissa, adj_value);
}

#define dissect_double_as_32s(vec_foo) \
  printf(" "); \
  dissect_binary16_float((vec_foo & 0xffffffff)); \
  printf(" "); \
  dissect_binary16_float((vec_foo >> 32) & 0xffffffff);

#define dissect_double_as_16s(vec_foo) \
  printf(" "); \
  dissect_binary16_float((vec_foo&0xffff)); \
  printf(" "); \
  dissect_binary16_float((vec_foo>>16)&0xffff); \
  printf(" "); \
  dissect_binary16_float((vec_foo>>32)&0xffff); \
  printf(" "); \
  dissect_binary16_float((vec_foo>>48)&0xffff);

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

unsigned long mantissa_table[] = {
#ifdef EXHAUSTIVE_TESTS
  0xbeefbeefbeef, /* NOR or DEN or NaN */
  0x000000000000, /* ZERO or INF */
  0x7fffffffffff, /* NOR or DEN or NaN */
#else
  0x000000000000, /* ZERO or INF */
  0x7fffffffffff, /* NOR or DEN or NaN */
#endif
};
#define MAX_MANTISSAS (sizeof(mantissa_table) / sizeof(unsigned long))

/* build in 64-bit chunks, low doubleword is zero. */
static unsigned long * float_vsxargs;
static unsigned long * binary128_float_vsxargs = NULL;
static unsigned long * binary64_float_vsxargs = NULL;
static unsigned long * binary32_float_vsxargs = NULL;
static unsigned long * binary16_float_vsxargs = NULL;

unsigned long nb_float_vsxargs;

#define MAX_FLOAT_VSX_ARRAY_SIZE (((MAX_EXPONENTS * MAX_MANTISSAS) * 2 + 1) * 2)

void build_float_vsx_tables (void)
{
   long i = 0;
   unsigned long signbit;
   unsigned long exponent;
   unsigned long mantissa;/* also referred to as FRACTION in the ISA.*/
   unsigned long exponent_index;
   unsigned long mantissa_index;

   if (verbose > 2) printf("%s\n", __FUNCTION__);

   binary128_float_vsxargs = malloc(MAX_FLOAT_VSX_ARRAY_SIZE
                                    * sizeof(unsigned long));

   float_vsxargs = binary128_float_vsxargs;

   binary64_float_vsxargs = malloc(MAX_FLOAT_VSX_ARRAY_SIZE
                                   * sizeof(unsigned long));

   binary32_float_vsxargs = malloc(MAX_FLOAT_VSX_ARRAY_SIZE
                                   * sizeof(unsigned long));
   binary16_float_vsxargs = malloc(MAX_FLOAT_VSX_ARRAY_SIZE
                                   * sizeof(unsigned long));

   for (signbit = 0; signbit < 2; signbit++) {
      for (exponent_index = 0; exponent_index < MAX_EXPONENTS;
           exponent_index++) {

         for (mantissa_index = 0; mantissa_index < MAX_MANTISSAS;
              mantissa_index++) {

            exponent = exponent_table[exponent_index];
            mantissa = mantissa_table[mantissa_index];

         if (verbose > 2) {
            printf("signbit:%lx ", signbit);
            printf("exponent:%4lx ", exponent);
            printf("mantissa:%lx ", mantissa);
            printf("\n");
         }

         binary128_float_vsxargs[i] = build_binary128_float(signbit, exponent,
                                                            mantissa);

         binary128_float_vsxargs[i+1] = 0;

         binary64_float_vsxargs[i] = build_binary64_float(signbit, exponent,
                                                          mantissa);

         binary64_float_vsxargs[i+1] = build_binary64_float(signbit, exponent,
                                                            mantissa);

         binary32_float_vsxargs[i] = build_binary32_float(signbit, exponent,
                                                          mantissa);

         binary32_float_vsxargs[i+1] = build_binary32_float(signbit, exponent,
                                                            mantissa);

         binary16_float_vsxargs[i] = build_binary16_float(signbit, exponent,
                                                          mantissa);

         binary16_float_vsxargs[i+1] = build_binary16_float(signbit, exponent,
                                                            mantissa);
         i += 2;
         }
      }
   }
   nb_float_vsxargs = i;
}

/* Display entries stored in the float_vsx table.  These are used as
 * quad/double/singles, stored as quads. */
void dump_float_vsx_table (void) {
   int i;

   printf("Float VSX Table:");
   printf("128-bit (quad):\n");

   for (i = 0; i < nb_float_vsxargs; i += 2) {
      printf("i =: %2d ", i);
      dissect_binary128_float(binary128_float_vsxargs[i]);
   }

   printf("64-bit (double):\n");

   for (i = 0; i< nb_float_vsxargs; i += 2) {
      printf("i = %2d ", i);
      dissect_binary64_float(binary64_float_vsxargs[i]);
   }

   printf("32-bit (single):\n");

   for (i = 0; i < nb_float_vsxargs; i += 2) {
      printf("i = %2d ", i);
      dissect_binary32_float(binary32_float_vsxargs[i]);
   }

   printf("16-bit (half):\n");

   for (i = 0; i < nb_float_vsxargs; i += 2) {
      printf("i =% 2d ", i);
      dissect_binary16_float(binary16_float_vsxargs[i]);
   }

   printf("\n");
}

static void print_dcmx_field(unsigned long local_dcmx) {
   /* Note - this splats out the local_dxmc field from the form used to
    * globally pass it, with a single set bit, into the functions that use
    * it.  The actual DCMX field is a bit-field from 0x00 to 0x3f. If
    * multiple bits are ever set, this function and the way it is passed
    * into the users will need to be updated.  This does not handle
    * multiple bits being set.
    */

   printf(" DCMX=[");

   switch(local_dcmx) {
      case 0: printf("ALL"); break;
      case 1: printf("NaN"); break;
      case 2: printf("+inf"); break;
      case 3: printf("-inf"); break;
      case 4: printf("+zero"); break;
      case 5: printf("-zero"); break;
      case 6: printf("+denormal"); break;
      case 7: printf("-denormal"); break;
      default: printf("other"); break;
   }

   if (verbose > 3)
      printf(" %lx", local_dcmx);

   printf("] ");
}

#define MAX_CHAR_ARGS_ARRAY_SIZE 128

static unsigned char * char_args;
unsigned long nb_char_args;

static void build_char_table(void) {
   long i = 0;
   char ichar;

   char_args = memalign(32, MAX_CHAR_ARGS_ARRAY_SIZE * sizeof(char));

#ifdef EXHAUSTIVE_TESTS
   for (ichar = 'a'; ichar <= 'z'; ichar++) { char_args[i++] = ichar; }
   for (ichar = '0'; ichar <= '9'; ichar++) { char_args[i++] = ichar; }
   for (ichar = 'A'; ichar <= 'Z'; ichar++) { char_args[i++] = ichar; }
#else
   for (ichar = 'a'; ichar <= 'z'; ichar+=6) { char_args[i++] = ichar; }
   for (ichar = '0'; ichar <= '9'; ichar+=6) { char_args[i++] = ichar; }
   for (ichar = 'A'; ichar <= 'Z'; ichar+=6) { char_args[i++] = ichar; }
#endif

   char_args[i++] = ' ';
   char_args[i++] = '+';
   char_args[i++] = '-';
   char_args[i++] = '/';
   char_args[i++] = '[';
   char_args[i++] = ']';
   char_args[i++] = '`';
   char_args[i++] = '_';
   nb_char_args = i;
}

static void dump_char_table() {
   int i;

   printf("Char Table:");

   for (i = 0; i<nb_char_args; i++)
      printf("%c ", char_args[i]);

   printf("\n");
}

#define MAX_CHAR_RANGES_SIZE 128

static unsigned char * char_ranges;
unsigned long nb_char_ranges;

static void build_char_range_table(void) {
/* ... in groups of four. */

   long i = 0;
   char char_start, char_end;

   char_ranges = memalign(32, MAX_CHAR_RANGES_SIZE * sizeof(char));
   char_start = 'a';
   char_end   = 'z';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   char_start = 'A';
   char_end   = 'Z';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   char_start = '0';
   char_end   = '9';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   char_start = 'f';
   char_end   = 'z';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   char_start = 'a';
   char_end   = 'e';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   char_start = 'A';
   char_end   = 'E';
   char_ranges[i++] = char_start;
   char_ranges[i++] = char_end;

   nb_char_ranges = i;
}

static void dump_char_range_table()
{
   int i;

   printf("Char Range Table:");

   for (i = 0; i < nb_char_ranges; i += 4) {
       printf(" [ %c-%c %c-%c ] ",
                char_ranges[i], char_ranges[i+1],
                char_ranges[i+2], char_ranges[i+3] );
   }

   printf("\n");
}

static HWord_t *iargs = NULL;
static int nb_iargs = 0;

static void build_iargs_table (void) {
   uint64_t tmp;
   int i = 0;

   iargs = malloc(20 * sizeof(HWord_t));

   for (tmp = 0; ; tmp = 123456789*tmp + 123456789999) {
      if ((long)tmp < 0 )
         tmp = 0xFFFFFFFFFFFFFFFFULL;

      iargs[i++] = tmp;
      AB_DPRINTF("val %016lx\n", tmp);

      if (tmp == 0xFFFFFFFFFFFFFFFFULL)
         break;
   }

   AB_DPRINTF("Registered %d iargs values\n", i);
   nb_iargs = i;
}

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

   AB_DPRINTF("Registered %d vargs values\n", i/2);
   nb_vargs = i;
}

/* VPCV = Vector Permute Control Vector */
unsigned long nb_vpcv;
static unsigned long * vpcv = NULL;

#define MAX_VPCV_SIZE 20

static void build_vector_permute_table(void)
{
   int i=0;

   vpcv = memalign(16, MAX_VPCV_SIZE * sizeof(unsigned long));

#ifdef EXHAUSTIVE_TESTS
   /* These two lines are complementary pairs of each other. */
   vpcv[i++]=0x12021a0817141317ULL; vpcv[i++]=0x100d1b05070f0205ULL;
   vpcv[i++]=0x0d1d0517080b0c08ULL; vpcv[i++]=0x0f12041a18101d1cULL;
   vpcv[i++]=0x100d1b070f020505ULL; vpcv[i++]=0x0e201f1400130105ULL;
   vpcv[i++]=0x0705030a0b01ea0cULL; vpcv[i++]=0x0e0c09010602080dULL;
#else
   vpcv[i++]=0x12021a0817141317ULL; vpcv[i++]=0x100d1b05070f0205ULL;
   vpcv[i++]=0x0705030a0b01ea0cULL; vpcv[i++]=0x0e0c09010602080dULL;
#endif
   nb_vpcv=i;
   AB_DPRINTF("Registered %d permute control vectors \n", nb_vpcv);

   if (i >= MAX_VPCV_SIZE)
      printf("Warning! Exceeded size of table building the vector permute control . \n");
}

/* Decimal Encodings...
 * Packed, National, Zoned decimal content follows.
 * Note: Watch the conversions in and out of the
 *       dwords / vectors for reverses with respect to
 *       top/bottom low/high
 */

/* Packed Decimals:
 * A valid encoding of a packed decimal integer value requires the following
 * properties:
 *   – Each of the 31 4-bit digits of the operand’s magnitude (bits 0:123)
 *     must be in the range 0-9.
 *   – The sign code (bits 124:127) must be in the range 10-15. (0xa-0xf).
 * Source operands with sign codes of 0b1010, 0b1100, 0b1110, and 0b1111 are
 * interpreted as positive values.  Source operands with sign codes of
 * 0b1011 and 0b1101 are interpreted as negative values.
 * Positive and zero results are encoded with a either sign code of
 * 0b1100 or 0b1111, depending on the preferred sign (indicated as an
 * immediate operand).  Negative results are encoded with a sign code
 * of 0b1101.
 * PS - This is the 'preferred sign' bit encoded in some BCD associated
 * instructions.
 */

// Note: table content is limited to values encoded, not interpreted.
unsigned int packed_decimal_sign_codes[] = {
   /* positive operands */
   0xc, 0xf,  // 0b1100, 0b1111

   /* negative operands */
   0xd  // 0b1101
};

#define NR_PACKED_DECIMAL_SIGNS 3
#define MAX_PACKED_DECIMAL_TABLE_SIZE 8 * 16 * 2 + 20

static unsigned long * packed_decimal_table;

/* build into a pair of doubles */
unsigned long nb_packed_decimal_entries;

static void dissect_packed_decimal_sign(unsigned long local_sign) {
  switch(local_sign) {
     case 0xa: /*0b1010:*/ printf("[ + ]"); break;
     case 0xb: /*0b1011:*/ printf("[ - ]"); break;
     case 0xc: /*0b1100:*/ printf("(+|0)"); break;
     case 0xd: /*0b1101:*/ printf("( - )"); break;
     case 0xe: /*0b1110:*/ printf("[ + ]"); break;
     case 0xf: /*0b1111:*/ printf("(+|0)"); break;
     default: printf("(?%02lx)", local_sign);
  }
}

int extract_packed_decimal_sign(unsigned long dword1, unsigned long dword0) {
   return  (dword1 & 0xf);
}

static void dissect_packed_decimal(unsigned long dword1,unsigned long dword0)
{
   int i;
   int local_sign;
   int nibble;

   local_sign = extract_packed_decimal_sign(dword1, dword0);
   printf("packed_decimal: [");

   for (i = 60; i >= 0; i -= 4) {
      nibble=(dword1 >> (i)) & 0xf;
      printf(" %x", nibble);
   }

   for (i = 60; i >= 0; i -= 4) {
      nibble=(dword0 >> (i)) & 0xf;
      printf(" %x", nibble);
   }

   printf(" ");
   dissect_packed_decimal_sign(local_sign);
   printf(" ] ");
}

static void build_packed_decimal_table(void)
{
   long sign_index;
   long sign_value;
   unsigned long i = 0;
   unsigned long value;
 #ifdef EXHAUSTIVE_TESTS
   int scramble;
#endif

   if (verbose) printf("%s\n", __FUNCTION__);

   packed_decimal_table = malloc((MAX_PACKED_DECIMAL_TABLE_SIZE + 2)
                                 * sizeof (unsigned long));

   for (sign_index = 0; sign_index < NR_PACKED_DECIMAL_SIGNS; sign_index++) {
      sign_value = packed_decimal_sign_codes[sign_index];

      for (value = 0; value <= 9; value++) {
        packed_decimal_table[i]    = 0x1111111111111111 * value;
        packed_decimal_table[i+1]  = sign_value;
        packed_decimal_table[i+1] += 0x1111111111111110 * value;

        if (verbose>3) dissect_packed_decimal(packed_decimal_table[i+1],
                                              packed_decimal_table[i]);
        if (verbose>3) printf("\n");
        i+=2;
      }

#ifdef EXHAUSTIVE_TESTS
      for (scramble = 1; scramble <= 4; scramble++) {
        packed_decimal_table[i]    = 0x3210321032103210 * scramble;
        packed_decimal_table[i+1]  = sign_value;
        packed_decimal_table[i+1] += 0x0123012301230120 * scramble;

        if (verbose>3) dissect_packed_decimal(packed_decimal_table[i+1],
                                              packed_decimal_table[i]);
        if (verbose>3) printf("\n");
        i+=2;
      }
#endif

      /* Add some entries that will provide interesting output from
       * the convert TO tests.
       */
      packed_decimal_table[i]    = 0x0000000000000000;
      packed_decimal_table[i+1]  = sign_value;
      packed_decimal_table[i+1] += 0x0000000012345670;

      if (verbose > 3) dissect_packed_decimal(packed_decimal_table[i+1],
                                              packed_decimal_table[i]);

      if (verbose>3) printf("\n");

      i += 2;

#ifdef EXHAUSTIVE_TESTS
      packed_decimal_table[i]    = 0x0000000000000000;
      packed_decimal_table[i+1]  = sign_value;
      packed_decimal_table[i+1] += 0x0000000098765430;

      if (verbose > 3) dissect_packed_decimal(packed_decimal_table[i+1],
                                              packed_decimal_table[i]);

      if (verbose > 3) printf("\n");

      i += 2;

      packed_decimal_table[i]    = 0x000000000000000b;
      packed_decimal_table[i+1]  = sign_value;
      packed_decimal_table[i+1] += 0x0000000000000000;

      if (verbose > 3) dissect_packed_decimal(packed_decimal_table[i+1],
                                              packed_decimal_table[i]);

      if (verbose>3) printf("\n");

      i += 2;
#endif

      packed_decimal_table[i]    = 0x0030000000000000;
      packed_decimal_table[i+1]  = sign_value;
      packed_decimal_table[i+1] += 0x0000000000000000;

      if (verbose > 3) dissect_packed_decimal(packed_decimal_table[i+1],
                                            packed_decimal_table[i]);

      if (verbose > 3) printf("\n");

      i += 2;
   }

   if (verbose>2) printf("\n");

   nb_packed_decimal_entries = i;
}

static void dump_packed_decimal_table(void) {
   int i;

   printf("packed_decimal_table:\n");

   for (i = 0; i < nb_packed_decimal_entries; i += 2) {
      printf("i =: %2d ", i);
      dissect_packed_decimal(packed_decimal_table[i+1],
                             packed_decimal_table[i]);
      printf("\n");
   }
}

/* National decimals:
 * A valid encoding of a national decimal value requires the following.
 * – The contents of halfword 7 (sign code) must be
 *   either 0x002B or 0x002D.
 * – The contents of halfwords 0 to 6 must be in the
 *   range 0x0030 to 0x0039.
 * National decimal values having a sign code of 0x002B
 * are interpreted as positive values.
 * National decimal values having a sign code of 0x002D
 * are interpreted as negative values.
 */
unsigned int national_decimal_sign_codes[] = {
   /* positive */  0x002b,
   /* negative */  0x002d
};

#define NR_NATIONAL_DECIMAL_SIGNS 2

unsigned int national_decimal_values[] = {
#ifdef EXHAUSTIVE_TESTS
   0x0030, 0x0031, 0x0032, 0x0033, 0x0034,
   0x0035, 0x0036, 0x0037, 0x0038, 0x0039
#else
   0x0030, 0x0031, 
   0x0035, 0x0039
#endif
};

#define NR_NATIONAL_DECIMAL_VALUES (sizeof(national_decimal_values) / sizeof(unsigned int))

static unsigned long * national_decimal_table;

#define MAX_NATIONAL_DECIMAL_TABLE_SIZE 10 * NR_NATIONAL_DECIMAL_VALUES * NR_NATIONAL_DECIMAL_SIGNS

unsigned long nb_national_decimal_entries;

static void dissect_national_decimal_sign(unsigned long local_sign) {
   switch(local_sign) {
      case 0x002b:
            printf("( + )");
            break;

      case 0x002d:
            printf("( - )");
            break;

     default: printf("unhandled sign value: %lx", local_sign);
   }
}

int extract_national_decimal_sign(unsigned long dword1, unsigned long dword0) {
   return (dword1 & 0x0ff);
}

static void dissect_national_decimal(unsigned long dword1,
                                     unsigned long dword0)
{
   int i;
   int local_sign;
   long hword;

   printf("national_decimal: [");

   if (verbose>4) printf("raw: [%016lx %016lx] ", dword1, dword0);

   for (i = 48;i >= 0; i -= 16) {
      hword = dword1 >> (i) & 0x00ff;

      /* validity of national decimal value */
      /* the i>0 clause skips the validity check against the sign value. */
      if (((i > 0) && (hword < 0x30)) || (hword > 0x39)) printf("!");

      printf("%04lx ", hword);
   }

   for (i = 48; i >= 0; i -= 16) {
      hword = dword0 >> (i) & 0x00ff;

      if ((hword < 0x30) || (hword > 0x39)) printf("!");

      printf("%04lx ", hword);
   }

   local_sign = extract_national_decimal_sign(dword1, dword0);
   dissect_national_decimal_sign(local_sign);
   printf(" ] ");
}

static void build_national_decimal_table(void)
{
   long sign_index;
   long sign_value;
   unsigned long i = 0;
   int index;
   unsigned long value;

   if (verbose) printf("%s\n",__FUNCTION__);
   national_decimal_table = malloc(MAX_NATIONAL_DECIMAL_TABLE_SIZE
                                   * sizeof (unsigned long));

   for (sign_index = 0; sign_index < NR_NATIONAL_DECIMAL_SIGNS; sign_index++) {
      sign_value = national_decimal_sign_codes[sign_index];

      for (index = 0; index < NR_NATIONAL_DECIMAL_VALUES; index++) {
         value = national_decimal_values[index];

         national_decimal_table[i]    = 0x0001000100010001 * value;
         national_decimal_table[i+1]  = 0x0001000100010000 * value;
         national_decimal_table[i+1] += sign_value ;

         if (verbose > 3) {
            dissect_national_decimal(national_decimal_table[i+1],
                                     national_decimal_table[i]);
            printf("\n");
         }
         i += 2;
      }
#ifdef EXHAUSTIVE_TESTS
      { /* a few more for fun */
         national_decimal_table[i]    = 0x0031003200330034;
         national_decimal_table[i+1]  = 0x0035003600370000;
         national_decimal_table[i+1] += sign_value ;

         if (verbose > 3) {
            dissect_national_decimal(national_decimal_table[i+1],
                                     national_decimal_table[i]);
            printf("\n");
         }

         i += 2;
         national_decimal_table[i]    = 0x0031003200330034;
         national_decimal_table[i+1]  = 0x0035003600370000;
         national_decimal_table[i+1] += sign_value ;

         if (verbose > 3) {
            dissect_national_decimal(national_decimal_table[i+1],
                                     national_decimal_table[i]);
            printf("\n");
         }
         i += 2;
      }
#endif
   }

   if (verbose > 2) printf("\n");

   nb_national_decimal_entries = i;
}

static void dump_national_decimal_table(void) {
   int i;

   printf("national_decimal_table:\n");

   for (i = 0; i < nb_national_decimal_entries; i += 2) {
      printf("#%2d ", i);
      dissect_national_decimal(national_decimal_table[i+1],
                               national_decimal_table[i]);
      printf("\n");
   }
}


/* Zoned Decimals:
 *
 * When PS=0, do the following.
 *  A valid encoding of a zoned decimal value requires the following.
 *  – The contents of bits 0:3 of byte 15 (sign code) can be any
 *    value in the range 0x0 to 0xF.
 *  – The contents of bits 0:3 of bytes 0 to 14 (zone) must
 *    be the value 0x3.
 *  – The contents of bits 4:7 of bytes 0 to 15 must
 *    be a value in the range 0x0 to 0x9.
 *  Zoned decimal values having a sign code of 0x0, 0x1, 0x2, 0x3,
 *  0x8, 0x9, 0xA, or 0xB are interpreted as positive values.
 *  Zoned decimal values having a sign code of 0x4, 0x5, 0x6, 0x7,
 *  0xC, 0xD, 0xE, or 0xF are interpreted as negative values.
    :: 0,1,2,3,        8,9,a,b,         are interpreted as positive.
    ::         4,5,6,7,        c,d,e,f  are interpreted as negative.
 * When PS=1, do the following.
 *  A valid encoding of a zoned decimal source operand requires the following.
 *  – The contents of bits 0:3 of byte 15 (sign code) must be a value in the
 *    range 0xA to 0xF.
 *  – The contents of bits 0:3 of bytes 0 to 14 (zone) must be the value 0xF.
 *  – The contents of bits 4:7 of bytes 0 to 15 must be a value in the
 *    range 0x0 to 0x9.
 *  Zoned decimal source operands having a sign code of 0xA, 0xC, 0xE,
 *  or 0xF are interpreted as positive values.
 *  Zoned decimal source operands having a sign code of 0xB or 0xD are
 *  interpreted as negative values.
    ::                     a,  c,  e,f  are interpreted as positive.
    ::                       b,  d,     are interpreted as negative.
 */

/* a valid sign is anything in range 0-9,a-f,
 * For coverage that does not overwhelm, we have chosen to use  0,1,4,a,b,f. */
#define NM_ZONED_DECIMAL_SIGNS 6
#define NM_ZONED_VALUES 5   /* 0,2,4,6,9 */
#define NM_PS_VALUES 2      /* 0,1 */
#define NM_ZONED_ADDITIONAL_PATTERNS 4
#define MAX_ZONED_DECIMAL_TABLE_SIZE  NM_ZONED_DECIMAL_SIGNS * NM_ZONED_VALUES * NM_ZONED_ADDITIONAL_PATTERNS * NM_PS_VALUES + 10

static unsigned long zoned_decimal_table_[MAX_ZONED_DECIMAL_TABLE_SIZE];
static unsigned long * zoned_decimal_table;
unsigned long nb_zoned_decimal_entries;

static void dissect_zoned_decimal_sign(unsigned long local_sign, int ps) {
   if (ps == 0) {
      switch(local_sign) {
         case 0x0: case 0x1: case 0x2: case 0x3:
         case 0x8: case 0x9: case 0xa: case 0xb:
            printf("( + )");
            break;

         case 0x4: case 0x5: case 0x6: case 0x7:
         case 0xc: case 0xd: case 0xe: case 0xf:
            printf("( - )");
            break;
         default: printf("zoned decimal (ps=%d). Unhandled sign value: %lx",
                         ps, local_sign);
      }
   }

   if (ps == 1) {
      switch(local_sign) {
         case 0xa: case 0xc: case 0xe: case 0xf:
            printf("( + )");
            break;

         case 0xb: case 0xd:
            printf("( - )");
            break;

         default: printf("zoned decimal (ps=%d). Unhandled sign value: %lx",
                         ps, local_sign);
      }
   }
}

/* Valid byte values within a zoned decimal are in the ranges of
 * 0x30..0x39 when PS==0, or 0xf0..0xff when PS==1.
 */
static void check_zoned_byte_validity(int byte, int ps) {
   if (ps == 0) {
      /* check the zone */
      if (((byte & 0x30) != 0x30))
         printf("!=30");

   } else { /* ps==1 */
      if (((byte & 0xf0) != 0xf0))
         printf("%x !=f0 ", byte );
   }

   /* check the numeric value */
   if ((byte & 0x0f) > 0x9)
      printf("!(0..9)");
}

int extract_zoned_decimal_sign(unsigned long dword1, unsigned long dword0) {
   return ((dword1 & 0xf0) >> 4);
}

static void dissect_zoned_decimal(unsigned long dword1, unsigned long dword0,
                                  int ps)
{
   int i;
   int local_sign;
   int byte;

   printf("zoned_decimal: [");

   for (i = 56; i >= 0; i -= 8) {
      byte = (dword1 >> (i)) & 0xff;
      check_zoned_byte_validity(byte, ps);
      printf(" %02x", byte);
   }

   for (i = 56; i >= 0; i -= 8) {
      byte = (dword0 >> (i)) & 0x00ff;
      check_zoned_byte_validity(byte, ps);

      if ((byte & 0xf) > 0x9) printf(" !(>9)");
      printf(" %02x", byte);
   }

   local_sign = extract_zoned_decimal_sign(dword1, dword0);
   dissect_zoned_decimal_sign(local_sign, ps);
   printf(" ]");
}

#ifdef EXHAUSTIVE_TESTS
// Randomly chosen exhaustive coverage for k includes values: 0,2,4,7,9
# define SELECTIVE_INCREMENT_ZONED(k) \
   if (k == 7) k = 9;                    \
      else if (k == 4) k = 7;            \
         else if (k == 2) k = 4;         \
            else if (k == 0) k = 2;      \
               else k++;
// Randomly chosen exhaustive coverage for signs includes values: 0,1,4,a,b,f
# define SELECTIVE_INCREMENT_SIGNS(signs)              \
         if (signs == 0x0) signs = 0x1;                   \
         else if (signs == 0x1) signs = 0x4;              \
            else if (signs == 0x4) signs = 0xa;           \
               else if (signs == 0xa) signs = 0xb;        \
                  else if (signs == 0xb) signs = 0xf;     \
                     else signs++;
#else
// Randomly chosen coverage for k includes values: 0,7,9
# define SELECTIVE_INCREMENT_ZONED(k) \
   if (k == 7) k = 9;                 \
      else if (k == 0) k = 7;      \
         else k++;
// Randomly chosen coverage for signs includes values: 0,4,b,f
# define SELECTIVE_INCREMENT_SIGNS(signs)                  \
      if (signs == 0x0) signs = 0x4;                      \
            else if (signs == 0x4) signs = 0xb;           \
                  else if (signs == 0xb) signs = 0xf;     \
                     else signs++;
#endif


static void build_zoned_decimal_table(void)
{
   unsigned long signs;
   unsigned long i;
   int k;
   int ps;
   int signs_start,signs_end;

   if (verbose) printf("%s\n", __FUNCTION__);

   zoned_decimal_table = zoned_decimal_table_;
   i = 0;

   for (ps = 0; ps <= 1; ps++) {
      if (ps == 0) {
         signs_start = 0;
         signs_end   = 0xf;

      } else {
         signs_start = 0xa;
         signs_end   = 0xf;
      }

      for (signs = signs_start;
           signs <= signs_end;  /* signs selectively updated below */) {

         if (verbose > 2) printf("ps=%d sign:%lx\n", ps, signs);

         for (k = 0 ; k < 9;  /* k selectively updated below */) {
            if (ps == 0) {
               zoned_decimal_table[i]   = 0x3030303030303030;  // set bits 0:3 of bytes 0..7.
               zoned_decimal_table[i+1] = 0x3030303030303000;  // bits 0:3 of bytes 8..14 must be 0x3

            } else {
               zoned_decimal_table[i]   = 0xf0f0f0f0f0f0f0f0;  // set bits 0:3 of bytes 0..7.
               zoned_decimal_table[i+1] = 0xf0f0f0f0f0f0f000;  // bits 0:3 of bytes 8..14 must be 0x3
            }

            zoned_decimal_table[i]   += 0x010101010101010 * k; // set bits 4..7 of bytes 0..7.
            zoned_decimal_table[i+1] += 0x010101010101000 * k; // bits 4:7 of bytes 8..15 must be 0..9.
            zoned_decimal_table[i+1] += (signs << 4); // bits 0:3 of byte 15 is the sign.
            if (verbose > 3) {
               dissect_zoned_decimal(zoned_decimal_table[i+1],
                                     zoned_decimal_table[i], ps);
               printf("\n");
            }
            i += 2;
            SELECTIVE_INCREMENT_ZONED(k)
         }

         /* add a few more patterns outside of the k patterns. */
         if (ps == 0) {
            zoned_decimal_table[i]   = 0x3030303030303030;
            zoned_decimal_table[i+1] = 0x3030303030303000;

         } else  {
            zoned_decimal_table[i]   = 0xf0f0f0f0f0f0f0f0;
            zoned_decimal_table[i+1] = 0xf0f0f0f0f0f0f000;
         }

         zoned_decimal_table[i]      += 0x0908070605040302;
         zoned_decimal_table[i+1]    += 0x0102030405060700;
         zoned_decimal_table[i+1]    += (signs<<4); // bits 0:3 of byte 15.

         if (verbose > 3) {
            dissect_zoned_decimal(zoned_decimal_table[i+1],
                                  zoned_decimal_table[i], ps);
            printf("\n");
         }

         i += 2;
         SELECTIVE_INCREMENT_SIGNS(signs)
      } /* signs loop */
   } /* ps loop */

   nb_zoned_decimal_entries = i;
}

static void dump_zoned_decimal_table(void) {
   int i;
   int ps;

   for (ps = 0; ps <= 1; ps++) {
      printf("zoned_decimal_table ps=%d:\n", ps);

      for (i = 0; i < nb_zoned_decimal_entries; i += 2) {
         printf("#%2d ", i);
         dissect_zoned_decimal(zoned_decimal_table[i+1],
                               zoned_decimal_table[i], ps);
         printf("\n");
      }
   }
}

/* Build table containing shift and truncate values */
#define MAX_DECIMAL_SHIFT_TABLE_SIZE 64

static unsigned long * decimal_shift_table;
unsigned long nb_decimal_shift_entries;

static void build_decimal_shift_table(void) {
   unsigned long i = 0;
   unsigned long value;

   if (verbose) printf("%s\n",__FUNCTION__);

   decimal_shift_table = malloc(MAX_DECIMAL_SHIFT_TABLE_SIZE
                                * sizeof (unsigned long));

   for (value = 0; value <= 31; value++) {
        decimal_shift_table[i]   = value;
        decimal_shift_table[i+1] = 0;
        i += 2;
   }

   if (verbose>2) printf("\n");

   nb_decimal_shift_entries = i;
}

static void dump_decimal_shift_table(void) {
   int i;

   printf("decimal_shift_table:\n");

   for (i = 0; i < nb_decimal_shift_entries; i += 2) {
      printf("i=:%2d ", i);
      printf(" 0x%2lx 0x%2lx ", decimal_shift_table[i],
             decimal_shift_table[i+1]);
      printf("\n");
   }
}
