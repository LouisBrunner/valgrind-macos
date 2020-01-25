
/* HOW TO COMPILE:

 * 32bit build:
   gcc -Winline -Wall -g -O -mregnames -maltivec -m32
 * 64bit build:
   gcc -Winline -Wall -g -O -mregnames -maltivec -m64


 * test_isa_2_07_part1.c:
 * PPC tests for the ISA 2.07.  This file is based on the
 * jm-insns.c file for the new instructions in the ISA 2.07.  The
 * test structure has been kept the same as the original file to
 * the extent possible.
 *
 * Copyright (C) 2013 IBM
 *
 *   Authors: Carl Love <carll@us.ibm.com>
 *            Maynard Johnson <maynardj@us.ibm.com>
 *
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License as
 *   published by the Free Software Foundation; either version 2 of the
 *   License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 */

/*
 * Operation details
 * -----------------
 *
 * The 'loops' (e.g. int_loops) do the actual work:
 *  - loops over as many arguments as the insn needs (regs | imms)
 *     - sets up the environment (reset cr,xer, assign src regs...)
 *     - maybe modifies the asm instn to test different imm args
 *     - calls the test function
 *     - retrieves relevant register data (rD,cr,xer,...)
 *     - prints argument and result data.
 *
 * More specifically...
 *
 * all_tests[i] holds insn tests
 *  - of which each holds: {instn_test_arr[], description, flags}
 *
 * flags hold 3 instn classifiers: {family, type, arg_type}
 *
 * // The main test loop:
 * do_tests( user_ctl_flags ) {
 *    foreach(curr_test = all_test[i]) {
 *
 *       // flags are used to control what tests are run:
 *       if (curr_test->flags && !user_ctl_flags)
 *          continue;
 *
 *       // a 'loop_family_arr' is chosen based on the 'family' flag...
 *       switch(curr_test->flags->family) {
 *       case x: loop_family_arr = int_loops;
 *      ...
 *       }
 *
 *       // ...and the actual test_loop to run is found by indexing into
 *       // the loop_family_arr with the 'arg_type' flag:
 *       test_loop = loop_family[curr_test->flags->arg_type]
 *
 *       // finally, loop over all instn tests for this test:
 *       foreach (instn_test = curr_test->instn_test_arr[i]) {
 *
 *          // and call the test_loop with the current instn_test function,name
 *          test_loop( instn_test->func, instn_test->name )
 *       }
 *    }
 * }
 *
 */


/**********************************************************************/

/* Uncomment to enable output of CR flags for float tests */
//#define TEST_FLOAT_FLAGS

/* Uncomment to enable debug output */
//#define DEBUG_ARGS_BUILD
//#define DEBUG_FILTER

/**********************************************************************/
#include <stdio.h>

#ifdef HAS_ISA_2_07

#include "config.h"
#include <altivec.h>
#include <stdint.h>

#include <assert.h>
#include <ctype.h>     // isspace
#include <stdlib.h>
#include <string.h>
#include <unistd.h>    // getopt

#if !defined (__TEST_PPC_H__)
#define __TEST_PPC_H__

#include "tests/sys_mman.h"
#include "tests/malloc.h"       // memalign16

#define STATIC_ASSERT(e) sizeof(struct { int:-!(e); })

/* Something of the same size as void*, so can be safely be coerced
 * to/from a pointer type. Also same size as the host's gp registers.
 * According to the AltiVec section of the GCC manual, the syntax does
 * not allow the use of a typedef name as a type specifier in conjunction
 * with the vector keyword, so typedefs uint[32|64]_t are #undef'ed here
 * and redefined using #define.
 */
#undef uint32_t
#undef uint64_t
#define uint32_t unsigned int
#define uint64_t unsigned long long int

#ifndef __powerpc64__
typedef uint32_t  HWord_t;
#define ZERO 0
#else
typedef uint64_t  HWord_t;
#define ZERO 0ULL
#endif /* __powerpc64__ */

#ifdef VGP_ppc64le_linux
#define isLE 1
#else
#define isLE 0
#endif

typedef uint64_t Word_t;

enum {
    compile_time_test1 = STATIC_ASSERT(sizeof(uint32_t) == 4),
    compile_time_test2 = STATIC_ASSERT(sizeof(uint64_t) == 8),
};

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

#define DEFAULT_VSCR 0x0

static vector unsigned long long vec_out, vec_inA, vec_inB, vec_inC;
static vector unsigned int vec_inA_wd, vec_inB_wd;

/* XXXX these must all be callee-save regs! */
register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");

typedef void (*test_func_t) (void);
typedef struct _test test_t;
typedef struct _test_table test_table_t;
struct _test {
    test_func_t func;
    const char *name;
};

struct _test_table {
    test_t *tests;
    const char *name;
    uint32_t flags;
};

typedef void (*test_loop_t) (const char *name, test_func_t func,
                             uint32_t flags);

enum test_flags {
    /* Nb arguments */
    PPC_ONE_ARG    = 0x00000001,
    PPC_TWO_ARGS   = 0x00000002,
    PPC_THREE_ARGS = 0x00000003,
    PPC_CMP_ARGS   = 0x00000004,  // family: compare
    PPC_CMPI_ARGS  = 0x00000005,  // family: compare
    PPC_TWO_I16    = 0x00000006,  // family: arith/logical
    PPC_SPECIAL    = 0x00000007,  // family: logical
    PPC_LD_ARGS    = 0x00000008,  // family: ldst
    PPC_LDX_ARGS   = 0x00000009,  // family: ldst
    PPC_ST_ARGS    = 0x0000000A,  // family: ldst
    PPC_STX_ARGS   = 0x0000000B,  // family: ldst
    PPC_STQ_ARGS   = 0x0000000C,  // family: ldst, two args, imm
    PPC_LDQ_ARGS   = 0x0000000D,  // family: ldst, two args, imm
    PPC_STQX_ARGS  = 0x0000000E,  // family: ldst, three args
    PPC_LDQX_ARGS  = 0x0000000F,  // family: ldst, three_args
    PPC_NB_ARGS    = 0x0000000F,
    /* Type */
    PPC_ARITH      = 0x00000100,
    PPC_LOGICAL    = 0x00000200,
    PPC_COMPARE    = 0x00000300,
    PPC_CROP       = 0x00000400,
    PPC_LDST       = 0x00000500,
    PPC_POPCNT     = 0x00000600,
    PPC_ARITH_DRES = 0x00000700,
    PPC_DOUBLE_IN_IRES = 0x00000800,
    PPC_MOV        = 0x00000A00,
    PPC_SHA_OR_BCD = 0x00000B00,
    PPC_TYPE       = 0x00000F00,
    /* Family */
    PPC_INTEGER    = 0x00010000,
    PPC_FLOAT      = 0x00020000,
    PPC_405        = 0x00030000,  // Leave so we keep numbering consistent
    PPC_ALTIVEC    = 0x00040000,
    PPC_FALTIVEC   = 0x00050000,
    PPC_ALTIVECD   = 0x00060000,    /* double word Altivec tests */
    PPC_ALTIVECQ   = 0x00070000,
    PPC_FAMILY     = 0x000F0000,
    /* Flags: these may be combined, so use separate bitfields. */
    PPC_CR         = 0x01000000,
    PPC_XER_CA     = 0x02000000,
};

#endif /* !defined (__TEST_PPC_H__) */

/* -------------- END #include "test-ppc.h" -------------- */


#if defined (DEBUG_ARGS_BUILD)
#define AB_DPRINTF(fmt, args...) do { fprintf(stderr, fmt , ##args); } while (0)
#else
#define AB_DPRINTF(fmt, args...) do { } while (0)
#endif


#if defined (DEBUG_FILTER)
#define FDPRINTF(fmt, args...) do { fprintf(stderr, fmt , ##args); } while (0)
#else
#define FDPRINTF(fmt, args...) do { } while (0)
#endif

#define unused __attribute__ (( unused ))

typedef struct special {
   const char *name;
   void (*test_cb)(const char* name, test_func_t func,
                   unused uint32_t test_flags);
} special_t;

static void test_stq(void)
{
  __asm__ __volatile__ ("stq  %0, 0(%1)" : :"r" (r14), "r" (r16));
}

static test_t tests_istq_ops_two_i16[] = {
    { &test_stq             , "stq", },
    { NULL,                   NULL,           },
};

static void test_lq(void)
{
  __asm__ __volatile__ ("lq  %0, 0(%1)" : :"r" (r14), "r" (r16));
}

static test_t tests_ildq_ops_two_i16[] = {
    { &test_lq              , "lq", },
    { NULL,                   NULL,          },
};

#ifdef HAS_ISA_2_07
Word_t * mem_resv;
static void test_stbcx(void)
{
  /* Have to do the lbarx to the memory address to create the reservation
   * or the store will not occur.
   */
  __asm__ __volatile__ ("lbarx  %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
  r14 = (HWord_t) 0xABEFCD0145236789ULL;
  r15 = (HWord_t) 0x1155337744226688ULL;
  __asm__ __volatile__ ("stbcx. %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
}

static void test_sthcx(void)
{
  /* Have to do the lharx to the memory address to create the reservation
   * or the store will not occur.
   */
  __asm__ __volatile__ ("lharx  %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
  r14 = (HWord_t) 0xABEFCD0145236789ULL;
  r15 = (HWord_t) 0x1155337744226688ULL;
  __asm__ __volatile__ ("sthcx. %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
}
#endif

static void test_stqcx(void)
{
  /* Have to do the lqarx to the memory address to create the reservation
   * or the store will not occur.
   */
  __asm__ __volatile__ ("lqarx  %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
  r14 = (HWord_t) 0xABEFCD0145236789ULL;
  r15 = (HWord_t) 0x1155337744226688ULL;
  __asm__ __volatile__ ("stqcx. %0, %1, %2" : :"r" (r14), "r" (r16),"r" (r17));
}

static test_t tests_stq_ops_three[] = {
#ifdef HAS_ISA_2_07
    { &test_stbcx           , "stbcx.", },
    { &test_sthcx           , "sthcx.", },
#endif
    { &test_stqcx           , "stqcx.", },
    { NULL,                   NULL,           },
};

#ifdef HAS_ISA_2_07
static void test_lbarx(void)
{
  __asm__ __volatile__ ("lbarx  %0, %1, %2, 0" : :"r" (r14), "r" (r16),"r" (r17));
}
static void test_lharx(void)
{
  __asm__ __volatile__ ("lharx  %0, %1, %2, 0" : :"r" (r14), "r" (r16),"r" (r17));
}
#endif
static void test_lqarx(void)
{
  __asm__ __volatile__ ("lqarx  %0, %1, %2, 0" : :"r" (r14), "r" (r16),"r" (r17));
}

static test_t tests_ldq_ops_three[] = {
#ifdef HAS_ISA_2_07
    { &test_lbarx           , "lbarx", },
    { &test_lharx           , "lharx", },
#endif
    { &test_lqarx           , "lqarx", },
    { NULL,                   NULL,           },
};

static void test_fmrgew (void)
{
    __asm__ __volatile__ ("fmrgew        17,14,15");
};

static void test_fmrgow (void)
{
    __asm__ __volatile__ ("fmrgow        17,14,15");
};



// VSX move instructions
static void test_mfvsrd (void)
{
   __asm__ __volatile__ ("mfvsrd %0,%x1" : "=r" (r14) : "wa" (vec_inA));
};

static void test_mfvsrwz (void)
{
   __asm__ __volatile__ ("mfvsrwz %0,%x1" : "=r" (r14) : "wa" (vec_inA));
};

static void test_mtvsrd (void)
{
   __asm__ __volatile__ ("mtvsrd %x0,%1" : "=wa" (vec_out) : "r" (r14));
};

static void test_mtvsrwz (void)
{
   __asm__ __volatile__ ("mtvsrwz %x0,%1" : "=wa" (vec_out) : "r" (r14));
};

static void test_mtvsrwa (void)
{
   __asm__ __volatile__ ("mtvsrwa %x0,%1" : "=wa" (vec_out) : "r" (r14));
};

static void test_mtfprwa (void)
{
   __asm__ __volatile__ ("mtfprwa %x0,%1" : "=d" (vec_out) : "r" (r14));
};

static void test_mtvrwa (void)
{
   __asm__ __volatile__ ("mtvrwa %0,%1" : "=v" (vec_out) : "r" (r14));
};

static void test_mtvrd (void)
{
   __asm__ __volatile__ ("mtvrd %0,%1" : "=v" (vec_out) : "r" (r14));
};

static void test_mtfprd (void)
{
   __asm__ __volatile__ ("mtfprd %0,%1" : "=d" (vec_out) : "r" (r14));
};

static test_t tests_move_ops_spe[] = {
  { &test_mfvsrd          , "mfvsrd" },
  { &test_mfvsrwz         , "mfvsrwz" },
  { &test_mtvsrd          , "mtvsrd" },
  { &test_mtvsrwz         , "mtvsrwz" },
  { &test_mtfprwa         , "mtfprwa" },
  { &test_mtvsrwa         , "mtvsrwa" },
  { &test_mtfprd          , "mtfprd" },
  { &test_mtvrwa          , "mtvrwa" },
  { &test_mtvrd           , "mtvrd" },
  { NULL,                   NULL }
};

/* NOTE: Since these are "vector" instructions versus VSX, we must use
 * vector constraints.
 *
 * Vector Double Word tests.
 */
static void test_vpkudum (void)
{
   __asm__ __volatile__ ("vpkudum %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vaddudm (void)
{
   __asm__ __volatile__ ("vaddudm %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsubudm (void)
{
   __asm__ __volatile__ ("vsubudm %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vmaxud (void)
{
   __asm__ __volatile__ ("vmaxud %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vmaxsd (void)
{
   __asm__ __volatile__ ("vmaxsd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vminud (void)
{
   __asm__ __volatile__ ("vminud %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vminsd (void)
{
   __asm__ __volatile__ ("vminsd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vcmpequd (void)
{
   __asm__ __volatile__ ("vcmpequd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vcmpgtud (void)
{
   __asm__ __volatile__ ("vcmpgtud %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vcmpgtsd (void)
{
   __asm__ __volatile__ ("vcmpgtsd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vrld (void)
{
   __asm__ __volatile__ ("vrld %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsld (void)
{
   __asm__ __volatile__ ("vsld %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsrad (void)
{
   __asm__ __volatile__ ("vsrad %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsrd (void)
{
   __asm__ __volatile__ ("vsrd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

/* Vector Double Word saturate tests.*/

static void test_vpkudus (void)
{
   __asm__ __volatile__ ("vpkudus %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vpksdus (void)
{
   __asm__ __volatile__ ("vpksdus %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vpksdss (void)
{
   __asm__ __volatile__ ("vpksdss %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}


/* Vector unpack two words from one vector arg */
static void test_vupkhsw (void)
{
    __asm__ __volatile__ ("vupkhsw %0, %1" : "=v" (vec_out): "v" (vec_inB_wd));
}

static void test_vupklsw (void)
{
    __asm__ __volatile__ ("vupklsw %0, %1" : "=v" (vec_out): "v" (vec_inB_wd));
}


/* Vector Integer Word tests.*/
static void test_vmulouw (void)
{
  __asm__ __volatile__ ("vmulouw %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmuluwm (void)
{
    __asm__ __volatile__ ("vmuluwm %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmulosw (void)
{
    __asm__ __volatile__ ("vmulosw %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmuleuw (void)
{
    __asm__ __volatile__ ("vmuleuw %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmulesw (void)
{
    __asm__ __volatile__ ("vmulesw %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmrgew (void)
{
    __asm__ __volatile__ ("vmrgew %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vmrgow (void)
{
    __asm__ __volatile__ ("vmrgow %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vpmsumb (void)
{
    __asm__ __volatile__ ("vpmsumb %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vpmsumh (void)
{
    __asm__ __volatile__ ("vpmsumh %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vpmsumw (void)
{
    __asm__ __volatile__ ("vpmsumw %0, %1, %2" : "=v" (vec_out): "v" (vec_inA_wd),"v" (vec_inB_wd));
}

static void test_vpermxor (void)
{
  __asm__ __volatile__ ("vpermxor %0, %1, %2, %3" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB),"v" (vec_inC));
}

static void test_vpmsumd (void)
{
    __asm__ __volatile__ ("vpmsumd %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vnand (void)
{
    __asm__ __volatile__ ("vnand %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vorc (void)
{
    __asm__ __volatile__ ("vorc %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_veqv (void)
{
    __asm__ __volatile__ ("veqv %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vcipher (void)
{
    __asm__ __volatile__ ("vcipher %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vcipherlast (void)
{
    __asm__ __volatile__ ("vcipherlast %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vncipher (void)
{
    __asm__ __volatile__ ("vncipher %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vncipherlast (void)
{
    __asm__ __volatile__ ("vncipherlast %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vclzb (void)
{
    __asm__ __volatile__ ("vclzb %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vclzw (void)
{
    __asm__ __volatile__ ("vclzw %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vclzh (void)
{
    __asm__ __volatile__ ("vclzh %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vclzd (void)
{
    __asm__ __volatile__ ("vclzd %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vpopcntb (void)
{
    __asm__ __volatile__ ("vpopcntb %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vpopcnth (void)
{
    __asm__ __volatile__ ("vpopcnth %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vpopcntw (void)
{
    __asm__ __volatile__ ("vpopcntw %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vpopcntd (void)
{
    __asm__ __volatile__ ("vpopcntd %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static void test_vsbox (void)
{
    __asm__ __volatile__ ("vsbox %0, %1" : "=v" (vec_out): "v" (vec_inB));
}

static int st_six;
static void test_vshasigmad (void)
{
   switch (st_six) {
   case 0x00:
      __asm__ __volatile__ ("vshasigmad %0, %1, 0, 0" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x0f:
      __asm__ __volatile__ ("vshasigmad %0, %1, 0, 15" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x10:
      __asm__ __volatile__ ("vshasigmad %0, %1, 1, 0" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x1f:
      __asm__ __volatile__ ("vshasigmad %0, %1, 1, 15" : "=v" (vec_out): "v" (vec_inA));
      break;
   }
}

static void test_vshasigmaw (void)
{
   switch (st_six) {
   case 0x00:
      __asm__ __volatile__ ("vshasigmaw %0, %1, 0, 0" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x0f:
      __asm__ __volatile__ ("vshasigmaw %0, %1, 0, 15" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x10:
      __asm__ __volatile__ ("vshasigmaw %0, %1, 1, 0" : "=v" (vec_out): "v" (vec_inA));
      break;
   case 0x1f:
      __asm__ __volatile__ ("vshasigmaw %0, %1, 1, 15" : "=v" (vec_out): "v" (vec_inA));
      break;
   }
}

static int PS_bit;
static void test_bcdadd (void)
{
   if (PS_bit)
      __asm__ __volatile__ ("bcdadd. %0, %1, %2, 1" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
   else
      __asm__ __volatile__ ("bcdadd. %0, %1, %2, 0" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_bcdsub (void)
{
   if (PS_bit)
      __asm__ __volatile__ ("bcdsub. %0, %1, %2, 1" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
   else
      __asm__ __volatile__ ("bcdsub. %0, %1, %2, 0" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vaddcuq (void)
{
   __asm__ __volatile__ ("vaddcuq %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vadduqm (void)
{
   __asm__ __volatile__ ("vadduqm %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vaddecuq (void)
{
  __asm__ __volatile__ ("vaddecuq %0, %1, %2, %3" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB),"v" (vec_inC));
}

static void test_vaddeuqm (void)
{
  __asm__ __volatile__ ("vaddeuqm %0, %1, %2, %3" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB),"v" (vec_inC));
}

static void test_vsubcuq (void)
{
   __asm__ __volatile__ ("vsubcuq %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsubuqm (void)
{
   __asm__ __volatile__ ("vsubuqm %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vsubecuq (void)
{
  __asm__ __volatile__ ("vsubecuq %0, %1, %2, %3" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB),"v" (vec_inC));
}

static void test_vsubeuqm (void)
{
  __asm__ __volatile__ ("vsubeuqm %0, %1, %2, %3" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB),"v" (vec_inC));
}

static void test_vbpermq (void)
{
   __asm__ __volatile__ ("vbpermq %0, %1, %2" : "=v" (vec_out): "v" (vec_inA),"v" (vec_inB));
}

static void test_vgbbd (void)
{
    __asm__ __volatile__ ("vgbbd %0, %1" : "=v" (vec_out): "v" (vec_inB));
}


static test_t tests_aa_quadword_two_args[] = {
  { &test_vaddcuq       , "vaddcuq" },
  { &test_vadduqm       , "vadduqm" },
  { &test_vsubcuq       , "vsubcuq" },
  { &test_vsubuqm       , "vsubuqm" },
  { &test_vbpermq       , "vbpermq" },
  { NULL                , NULL      },
};

static test_t tests_aa_quadword_three_args[] = {
  { &test_vaddecuq      , "vaddecuq" },
  { &test_vaddeuqm      , "vaddeuqm" },
  { &test_vsubecuq      , "vsubecuq" },
  { &test_vsubeuqm      , "vsubeuqm" },
  { NULL                , NULL      },
};

static test_t tests_aa_bcd_ops[] = {
  { &test_bcdadd        , "bcdadd." },
  { &test_bcdsub        , "bcdsub." },
  { NULL                , NULL      },
};

static test_t tests_aa_SHA_ops[] = {
  { &test_vshasigmad    , "vshasigmad" },
  { &test_vshasigmaw    , "vshasigmaw" },
  { NULL                , NULL         },
};

static test_t tests_aa_ops_three[] = {
  { &test_vpermxor        , "vpermxor" },
  { NULL                  , NULL       },
};

static test_t tests_aa_word_ops_one_arg_dres[] = {
  { &test_vupkhsw         , "vupkhsw" },
  { &test_vupklsw         , "vupklsw" },
  { NULL                  , NULL      }
};

static test_t tests_aa_word_ops_two_args_dres[] = {
  { &test_vmulouw         , "vmulouw" },
  { &test_vmuluwm         , "vmuluwm" },
  { &test_vmulosw         , "vmulosw" },
  { &test_vmuleuw         , "vmuleuw" },
  { &test_vmulesw         , "vmulesw" },
  { &test_vmrgew          , "vmrgew" },
  { &test_vmrgow          , "vmrgow" },
  { &test_vpmsumb         , "vpmsumb" },
  { &test_vpmsumh         , "vpmsumh" },
  { &test_vpmsumw         , "vpmsumw" },
  { NULL                  , NULL      }
};

static test_t tests_aa_dbl_ops_two_args[] = {
  { &test_vaddudm         , "vaddudm", },
  { &test_vsubudm         , "vsubudm", },
  { &test_vmaxud          , "vmaxud", },
  { &test_vmaxsd          , "vmaxsd", },
  { &test_vminud          , "vminud", },
  { &test_vminsd          , "vminsd", },
  { &test_vcmpequd        , "vcmpequd", },
  { &test_vcmpgtud        , "vcmpgtud", },
  { &test_vcmpgtsd        , "vcmpgtsd", },
  { &test_vrld            , "vrld", },
  { &test_vsld            , "vsld", },
  { &test_vsrad           , "vsrad", },
  { &test_vsrd            , "vsrd", },
  { &test_vpkudum         , "vpkudum", },
  { &test_vpmsumd         , "vpmsumd", },
  { &test_vnand           , "vnand", },
  { &test_vorc            , "vorc", },
  { &test_veqv            , "veqv", },
  { &test_vcipher         , "vcipher" },
  { &test_vcipherlast     , "vcipherlast" },
  { &test_vncipher        , "vncipher" },
  { &test_vncipherlast    , "vncipherlast" },
  { NULL                  , NULL,      },
};

static test_t tests_aa_dbl_ops_one_arg[] = {
  { &test_vclzb           , "vclzb" },
  { &test_vclzw           , "vclzw" },
  { &test_vclzh           , "vclzh" },
  { &test_vclzd           , "vclzd" },
  { &test_vpopcntb        , "vpopcntb" },
  { &test_vpopcnth        , "vpopcnth" },
  { &test_vpopcntw        , "vpopcntw" },
  { &test_vpopcntd        , "vpopcntd" },
  { &test_vsbox           , "vsbox" },
  { &test_vgbbd           , "vgbbd" },
  { NULL                  , NULL,      }
};

static test_t tests_aa_dbl_to_int_two_args[] = {
  { &test_vpkudus         , "vpkudus", },
  { &test_vpksdus         , "vpksdus", },
  { &test_vpksdss         , "vpksdss", },
  { NULL                  , NULL,      },
};

static int verbose = 0;
static int arg_list_size = 0;
static unsigned long long * vdargs = NULL;
static unsigned long long * vdargs_x = NULL;
#define NB_VDARGS 9
#define NB_VDARGS_X 4

static void build_vdargs_table (void)
{
   // Each VSX register holds two doubleword integer values
   vdargs = memalign16(NB_VDARGS * sizeof(unsigned long long));
   vdargs[0] = 0x0102030405060708ULL;
   vdargs[1] = 0x090A0B0C0E0D0E0FULL;
   vdargs[2] = 0xF1F2F3F4F5F6F7F8ULL;
   vdargs[3] = 0xF9FAFBFCFEFDFEFFULL;
   vdargs[4] = 0x00007FFFFFFFFFFFULL;
   vdargs[5] = 0xFFFF000000000000ULL;
   vdargs[6] = 0x0000800000000000ULL;
   vdargs[7] = 0x0000000000000000ULL;
   vdargs[8] = 0xFFFFFFFFFFFFFFFFULL;

   vdargs_x = memalign16(NB_VDARGS_X * sizeof(unsigned long long));
   vdargs_x[0] = 0x000000007c118a2bULL;
   vdargs_x[1] = 0x00000000f1112345ULL;
   vdargs_x[2] = 0x01F2F3F4F5F6F7F8ULL;
   vdargs_x[3] = 0xF9FAFBFCFEFDFEFFULL;
}

static unsigned int * vwargs = NULL;
#define NB_VWARGS 8

static void build_vwargs_table (void)
{
   // Each VSX register holds 4 integer word values
   size_t i = 0;
   vwargs = memalign(8, 8 * sizeof(int));
   assert(vwargs);
   assert(0 == ((8-1) & (unsigned long)vwargs));
   vwargs[i++] = 0x01020304;
   vwargs[i++] = 0x05060708;
   vwargs[i++] = 0x090A0B0C;
   vwargs[i++] = 0x0E0D0E0F;
   vwargs[i++] = 0xF1F2F3F4;
   vwargs[i++] = 0xF5F6F7F8;
   vwargs[i++] = 0xF9FAFBFC;
   vwargs[i++] = 0xFEFDFEFF;
}

static unsigned long long vbcd_args[] __attribute__ ((aligned (16))) = {
   0x8045090189321003ULL, // Negative BCD value
   0x001122334556677dULL,
   0x0000107600000001ULL, // Positive BCD value
   0x319293945142031aULL,
   0x0ULL,                // Valid BCD zero
   0xaULL,
   0x0ULL,                // Invalid BCD zero (no sign code)
   0x0ULL
};
//#define NUM_VBCD_VALS (sizeof vbcd_args/sizeof vbcd_args[0])
#define NUM_VBCD_VALS 8 

static void build_vargs_table (void)
{
   build_vdargs_table();
   build_vwargs_table();
}

static double *fargs = NULL;
static int nb_fargs = 0;

static inline void register_farg (void *farg,
                                  int s, uint16_t _exp, uint64_t mant)
{
   uint64_t tmp;

   tmp = ((uint64_t)s << 63) | ((uint64_t)_exp << 52) | mant;
   *(uint64_t *)farg = tmp;
   AB_DPRINTF("%d %03x %013llx => %016llx %0e\n",
              s, _exp, mant, *(uint64_t *)farg, *(double *)farg);
}

static void build_fargs_table (void)
{
   /* Double precision:
    * Sign goes from zero to one               (1 bit)
    * Exponent goes from 0 to ((1 << 12) - 1)  (11 bits)
    * Mantissa goes from 1 to ((1 << 52) - 1)  (52 bits)
    * + special values:
    * +0.0      : 0 0x000 0x0000000000000 => 0x0000000000000000
    * -0.0      : 1 0x000 0x0000000000000 => 0x8000000000000000
    * +infinity : 0 0x7FF 0x0000000000000 => 0x7FF0000000000000
    * -infinity : 1 0x7FF 0x0000000000000 => 0xFFF0000000000000
    * +QNaN     : 0 0x7FF 0x8000000000000 => 0x7FF8000000000000
    * -QNaN     : 1 0x7FF 0x8000000000000 => 0xFFF8000000000000
    * +SNaN     : 0 0x7FF 0x7FFFFFFFFFFFF => 0x7FF7FFFFFFFFFFFF
    * -SNaN     : 1 0x7FF 0x7FFFFFFFFFFFF => 0xFFF7FFFFFFFFFFFF
    * (8 values)

    * Ref only:
    * Single precision
    * Sign:     1 bit
    * Exponent: 8 bits
    * Mantissa: 23 bits
    * +0.0      : 0 0x00 0x000000 => 0x00000000
    * -0.0      : 1 0x00 0x000000 => 0x80000000
    * +infinity : 0 0xFF 0x000000 => 0x7F800000
    * -infinity : 1 0xFF 0x000000 => 0xFF800000
    * +QNaN     : 0 0xFF 0x400000 => 0x7FC00000
    * -QNaN     : 1 0xFF 0x400000 => 0xFFC00000
    * +SNaN     : 0 0xFF 0x3FFFFF => 0x7FBFFFFF
    * -SNaN     : 1 0xFF 0x3FFFFF => 0xFFBFFFFF
    */
   uint64_t mant;
   uint16_t _exp, e0, e1;
   int s;
   int i=0;

   /* Note: VEX isn't so hot with denormals, so don't bother
      testing them: set _exp > 0
   */

   if ( arg_list_size == 1 ) {   // Large
      fargs = malloc(200 * sizeof(double));
      for (s=0; s<2; s++) {
         for (e0=0; e0<2; e0++) {
            for (e1=0x001; ; e1 = ((e1 + 1) << 2) + 6) {
               if (e1 >= 0x400)
                  e1 = 0x3fe;
               _exp = (e0 << 10) | e1;
               for (mant = 0x0000000000001ULL; mant < (1ULL << 52);
                    /* Add 'random' bits */
                    mant = ((mant + 0x4A6) << 13) + 0x359) {
                  register_farg(&fargs[i++], s, _exp, mant);
               }
               if (e1 == 0x3fe)
                  break;
            }
         }
      }
   } else {                      // Default
      fargs = malloc(16 * sizeof(double));
      for (s=0; s<2; s++) {                                // x2
            for (e1=0x001; ; e1 = ((e1 + 1) << 13) + 7) {  // x2
               if (e1 >= 0x400)
                  e1 = 0x3fe;
               _exp = e1;
               for (mant = 0x0000000000001ULL; mant < (1ULL << 52);
                    /* Add 'random' bits */
                    mant = ((mant + 0x4A6) << 29) + 0x359) {  // x2
                  register_farg(&fargs[i++], s, _exp, mant);
               }
               if (e1 == 0x3fe)
                  break;
            }
      }
   }

   /* Special values */
   /* +0.0      : 0 0x000 0x0000000000000 */
   s = 0;
   _exp = 0x000;
   mant = 0x0000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* -0.0      : 1 0x000 0x0000000000000 */
   s = 1;
   _exp = 0x000;
   mant = 0x0000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* +infinity : 0 0x7FF 0x0000000000000  */
   s = 0;
   _exp = 0x7FF;
   mant = 0x0000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* -infinity : 1 0x7FF 0x0000000000000 */
   s = 1;
   _exp = 0x7FF;
   mant = 0x0000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* +QNaN     : 0 0x7FF 0x7FFFFFFFFFFFF */
   s = 0;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* -QNaN     : 1 0x7FF 0x7FFFFFFFFFFFF */
   s = 1;
   _exp = 0x7FF;
   mant = 0x7FFFFFFFFFFFFULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* +SNaN     : 0 0x7FF 0x8000000000000 */
   s = 0;
   _exp = 0x7FF;
   mant = 0x8000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   /* -SNaN     : 1 0x7FF 0x8000000000000 */
   s = 1;
   _exp = 0x7FF;
   mant = 0x8000000000000ULL;
   register_farg(&fargs[i++], s, _exp, mant);
   AB_DPRINTF("Registered %d fargs values\n", i);

   nb_fargs = i;
}



static int check_filter (char *filter)
{
   char *c;
   int ret = 1;

   if (filter != NULL) {
      c = strchr(filter, '*');
      if (c != NULL) {
         *c = '\0';
         ret = 0;
      }
   }
   return ret;
}

static int check_name (const char* name, const char *filter,
                       int exact)
{
   int nlen, flen;
   int ret = 0;

   if (filter != NULL) {
      for (; isspace(*name); name++)
         continue;
      FDPRINTF("Check '%s' againt '%s' (%s match)\n",
               name, filter, exact ? "exact" : "starting");
      nlen = strlen(name);
      flen = strlen(filter);
      if (exact) {
         if (nlen == flen && memcmp(name, filter, flen) == 0)
            ret = 1;
      } else {
         if (flen <= nlen && memcmp(name, filter, flen) == 0)
            ret = 1;
      }
   } else {
      ret = 1;
   }
   return ret;
}


typedef struct insn_sel_flags_t_struct {
   int one_arg, two_args, three_args;
   int arith, logical, compare, ldst;
   int integer, floats, altivec, faltivec;
   int cr;
} insn_sel_flags_t;

static void test_float_two_args (const char* name, test_func_t func,
                                 unused uint32_t test_flags)
{
   double res;
   Word_t u0, u1, ur;
   volatile uint32_t flags;
   int i, j;

   for (i=0; i<nb_fargs; i+=3) {
      for (j=0; j<nb_fargs; j+=5) {
         u0 = *(Word_t *)(&fargs[i]);
         u1 = *(Word_t *)(&fargs[j]);
         f14 = fargs[i];
         f15 = fargs[j];

         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)();
         GET_CR(flags);
         res = f17;
         ur = *(uint64_t *)(&res);

         printf("%s %016llx, %016llx => %016llx",
                name, u0, u1, ur);
#if defined TEST_FLOAT_FLAGS
         printf(" (%08x)", flags);
#endif
         printf("\n");
      }
      if (verbose) printf("\n");
   }
}


static void mfvs(const char* name, test_func_t func,
                 unused uint32_t test_flags)
{
   /* This test is for move instructions where the input is a scalar register
    * and the destination is a vector register.
    */
   int i;
   volatile Word_t result;
   result = 0ULL;

   for (i=0; i < NB_VDARGS; i++) {
      r14 = ZERO;
      if (isLE)
         vec_inA = (vector unsigned long long){ 0ULL, vdargs[i] };
      else
         vec_inA = (vector unsigned long long){ vdargs[i], 0ULL };

      (*func)();
      result = r14;
      printf("%s: %016llx => %016llx\n", name, vdargs[i], result);
   }
}

static void mtvs(const char* name, test_func_t func,
                 unused uint32_t test_flags)
{
   /* This test is for move instructions where the input is a scalar register
    * and the destination is a vector register.
    */
   unsigned long long *dst;
   int i;

   for (i=0; i < NB_VDARGS; i++) {
      r14  = vdargs[i];
      vec_out = (vector unsigned long long){ 0ULL, 0ULL };

      (*func)();
      dst = (unsigned long long *) &vec_out;
      if (isLE)
         dst++;
      printf("%s: %016llx => %016llx\n", name, vdargs[i], *dst);
   }
}

static void mtvs2s(const char* name, test_func_t func,
                 unused uint32_t test_flags)
{
   /* This test is the mtvsrwa instruction.
    */
   unsigned long long *dst;
   int i;

   for (i=0; i < NB_VDARGS; i++) {
      // Only the lower half of the vdarg doubleword arg will be used as input by mtvsrwa
      unsigned int * src = (unsigned int *)&vdargs[i];
      if (!isLE)
         src++;
      r14  = vdargs[i];
      vec_out = (vector unsigned long long){ 0ULL, 0ULL };

      (*func)();
      // Only doubleword 0 is used in output
      dst = (unsigned long long *) &vec_out;
      if (isLE)
         dst++;
      printf("%s: %08x => %016llx\n", name, *src, *dst);
   }
}

static void test_special (special_t *table,
                          const char* name, test_func_t func,
                          unused uint32_t test_flags)
{
   const char *tmp;
   int i;

   for (tmp = name; isspace(*tmp); tmp++)
      continue;
   for (i=0; table[i].name != NULL; i++) {
      if (strcmp(table[i].name, tmp) == 0) {
         (*table[i].test_cb)(name, func, test_flags);
         return;
      }
   }
   fprintf(stderr, "ERROR: no test found for op '%s'\n", name);
}

static special_t special_move_ops[] = {
   {
      "mfvsrd",  /* move from vector to scalar reg doubleword */
      &mfvs,
   },
   {
      "mtvsrd",  /* move from scalar to vector reg doubleword */
      &mtvs,
   },
   {
      "mtvsrwa", /* mtvsrwa move from scalar to vector reg  */
      &mtvs2s,
   },
   {
      "mtfprwa", /* (extended mnemonic for mtvsrwa) move from scalar to vector
		    reg */
      &mtvs2s,
   },
   {
      "mfvsrwz", /* move from vector to scalar reg word */
      &mfvs,
   },
   {
      "mtvsrwz", /* move from scalar to vector reg word */
      &mtvs2s,
   },
   {
      "mtvrwa", /* (extended mnemonic for mtvsrwa) move to vsr word */
      &mtvs2s,
   },
   {
      "mtvrd", /* (extended mnemonic for mtvsrd) move to vsr double word */
      &mtvs,
   },
   {
      "mtfprd", /* (extended mnemonic for mtvsrd) move to float word */
      &mtvs,
   }
};

static void test_move_special(const char* name, test_func_t func,
                                uint32_t test_flags)
{
   test_special(special_move_ops, name, func, test_flags);
}

/* Vector Double Word tests */

static void test_av_dint_two_args (const char* name, test_func_t func,
                                   unused uint32_t test_flags)
{

   unsigned long long * dst;
   unsigned int * dst_int;
   int i,j;
   int family = test_flags & PPC_FAMILY;
   int is_vpkudum, is_vpmsumd;
   if (strcmp(name, "vpkudum") == 0)
      is_vpkudum = 1;
   else
      is_vpkudum = 0;

   if (strcmp(name, "vpmsumd") == 0)
      is_vpmsumd = 1;
   else
      is_vpmsumd = 0;

   for (i = 0; i < NB_VDARGS - 1; i+=2) {
      if (isLE && family == PPC_ALTIVECQ)
         vec_inA = (vector unsigned long long){ vdargs[i+1], vdargs[i] };
      else
         vec_inA = (vector unsigned long long){ vdargs[i], vdargs[i+1] };
      for (j = 0; j < NB_VDARGS - 1; j+=2) {
         if (isLE && family == PPC_ALTIVECQ)
            vec_inB = (vector unsigned long long){ vdargs[j+1], vdargs[j] };
         else
            vec_inB = (vector unsigned long long){ vdargs[j], vdargs[j+1] };
         vec_out = (vector unsigned long long){ 0,0 };

         (*func)();
         dst_int = (unsigned int *)&vec_out;
         dst  = (unsigned long long*)&vec_out;

         printf("%s: ", name);

         if (is_vpkudum) {
            printf("Inputs: %08llx %08llx %08llx %08llx\n", vdargs[i] & 0x00000000ffffffffULL,
                   vdargs[i+1] & 0x00000000ffffffffULL, vdargs[j] & 0x00000000ffffffffULL,
                   vdargs[j+1] & 0x00000000ffffffffULL);
            if (isLE)
               printf("         Output: %08x %08x %08x %08x\n", dst_int[2], dst_int[3],
                      dst_int[0], dst_int[1]);
            else
               printf("         Output: %08x %08x %08x %08x\n", dst_int[0], dst_int[1],
                      dst_int[2], dst_int[3]);
         } else if (is_vpmsumd) {
            printf("%016llx @@ %016llx ", vdargs[i], vdargs[j]);
            if (isLE)
               printf(" ==> %016llx\n", dst[1]);
            else
               printf(" ==> %016llx\n", dst[0]);
            printf("\t%016llx @@ %016llx ", vdargs[i+1], vdargs[j+1]);
            if (isLE)
               printf(" ==> %016llx\n", dst[0]);
            else
               printf(" ==> %016llx\n", dst[1]);
         } else if (family == PPC_ALTIVECQ) {
            if (isLE)
               printf("%016llx%016llx @@ %016llx%016llx ==> %016llx%016llx\n",
                      vdargs[i], vdargs[i+1], vdargs[j], vdargs[j+1],
                      dst[1], dst[0]);
            else
               printf("%016llx%016llx @@ %016llx%016llx ==> %016llx%016llx\n",
                      vdargs[i], vdargs[i+1], vdargs[j], vdargs[j+1],
                      dst[0], dst[1]);
         } else {
            printf("%016llx @@ %016llx ", vdargs[i], vdargs[j]);
            printf(" ==> %016llx\n", dst[0]);
            printf("\t%016llx @@ %016llx ", vdargs[i+1], vdargs[j+1]);
            printf(" ==> %016llx\n", dst[1]);
         }
      }
   }
}

static void test_av_dint_one_arg (const char* name, test_func_t func,
                                  unused uint32_t test_flags)
{

   unsigned long long * dst;
   int i;

   for (i = 0; i < NB_VDARGS - 1; i+=2) {
      vec_inB = (vector unsigned long long){ vdargs[i], vdargs[i+1] };
      vec_out = (vector unsigned long long){ 0,0 };

      (*func)();
      dst  = (unsigned long long*)&vec_out;

      printf("%s: ", name);
      printf("%016llx @@ %016llx ", vdargs[i], vdargs[i + 1]);
      printf(" ==> %016llx%016llx\n", dst[0], dst[1]);
   }
}

static void test_av_dint_one_arg_SHA (const char* name, test_func_t func,
                                      unused uint32_t test_flags)
{
   unsigned long long * dst;
   int i, st, six;

   for (i = 0; i < NB_VDARGS - 1; i+=2) {
      vec_inA = (vector unsigned long long){ vdargs[i], vdargs[i+1] };
      vec_out = (vector unsigned long long){ 0,0 };

      for (st = 0; st < 2; st++) {
         for (six = 0; six < 16; six+=15) {
            st_six = (st << 4) | six;
            (*func)();
            dst  = (unsigned long long*)&vec_out;

            printf("%s: ", name);
            printf("%016llx @@ %016llx ", vdargs[i], vdargs[i + 1]);
            printf(" ==> %016llx || %016llx\n", dst[0], dst[1]);
         }
      }
   }
}

static void test_av_bcd (const char* name, test_func_t func,
                         unused uint32_t test_flags)
{
   unsigned long long * dst;
   int i, j;

   for (i = 0; i < NUM_VBCD_VALS - 1; i+=2) {
      if (isLE)
         vec_inA = (vector unsigned long long){ vbcd_args[i+1], vbcd_args[i]};
      else
         vec_inA = (vector unsigned long long){ vbcd_args[i], vbcd_args[i+1] };
      for (j = 0; j < NUM_VBCD_VALS - 1; j+=2) {
         if (isLE)
            vec_inB = (vector unsigned long long){ vbcd_args[j+1] , vbcd_args[j] };
         else
            vec_inB = (vector unsigned long long){ vbcd_args[j], vbcd_args[j+1] };
         vec_out = (vector unsigned long long){ 0, 0 };

         for (PS_bit = 0; PS_bit < 2; PS_bit++) {
            (*func)();
            dst  = (unsigned long long*)&vec_out;
            printf("%s: ", name);
            printf("%016llx || %016llx @@ %016llx || %016llx",
                   vbcd_args[i], vbcd_args[i + 1],
                   vbcd_args[j], vbcd_args[j + 1]);
            if (isLE)
               printf(" ==> %016llx || %016llx\n", dst[1], dst[0]);
            else
               printf(" ==> %016llx || %016llx\n", dst[0], dst[1]);
         }
      }
   }
}

/* Vector doubleword-to-int tests, two input args, integer result */
static void test_av_dint_to_int_two_args (const char* name, test_func_t func,
                                          unused uint32_t test_flags)
{

   unsigned int * dst_int;
   int i,j;
   for (i = 0; i < NB_VDARGS_X - 1; i+=2) {
      vec_inA = (vector unsigned long long){ vdargs_x[i], vdargs_x[i+1] };
      for (j = 0; j < NB_VDARGS_X - 1; j+=2) {
         vec_inB = (vector unsigned long long){ vdargs_x[j], vdargs_x[j+1] };
         vec_out = (vector unsigned long long){ 0,0 };

         (*func)();
         dst_int = (unsigned int *)&vec_out;

         printf("%s: ", name);
         printf("%016llx, %016llx @@ %016llx, %016llx ",
                vdargs_x[i], vdargs_x[i+1],
                vdargs_x[j], vdargs_x[j+1]);
         if (isLE)
            printf(" ==> %08x %08x %08x %08x\n", dst_int[2], dst_int[3],
                   dst_int[0], dst_int[1]);
         else
            printf(" ==> %08x %08x %08x %08x\n", dst_int[0], dst_int[1],
                   dst_int[2], dst_int[3]);
      }
   }
}

/* Vector Word tests; two integer args, with double word result */

static void test_av_wint_two_args_dres (const char* name, test_func_t func,
                                        unused uint32_t test_flags)
{

   unsigned long long * dst;
   int i,j;

   for (i = 0; i < NB_VWARGS; i+=4) {
      if (isLE)
         vec_inA_wd = (vector unsigned int){ vwargs[i+3], vwargs[i+2], vwargs[i+1], vwargs[i] };
      else
         vec_inA_wd = (vector unsigned int){ vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3] };
      for (j = 0; j < NB_VWARGS; j+=4) {
         if (isLE)
            vec_inB_wd = (vector unsigned int){ vwargs[j+3], vwargs[j+2], vwargs[j+1], vwargs[j] };
         else
            vec_inB_wd = (vector unsigned int){ vwargs[j], vwargs[j+1], vwargs[j+2], vwargs[j+3] };
         vec_out = (vector unsigned long long){ 0, 0 };

         (*func)();
         dst  = (unsigned long long *)&vec_out;
         printf("%s: ", name);
         if (isLE)
            printf("%08x %08x %08x %08x ==> %016llx %016llx\n",
                   vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3], dst[1], dst[0]);
         else
            printf("%08x %08x %08x %08x ==> %016llx %016llx\n",
                   vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3], dst[0], dst[1]);
      }
   }
}

/* Vector Word tests; one input arg, with double word result */

static void test_av_wint_one_arg_dres (const char* name, test_func_t func,
                                       unused uint32_t test_flags)
{
   unsigned long long * dst;
   int i;
   for (i = 0; i < NB_VWARGS; i+=4) {
      if (isLE)
         vec_inB_wd = (vector unsigned int){ vwargs[i+3], vwargs[i+2], vwargs[i+1], vwargs[i] };
      else
         vec_inB_wd = (vector unsigned int){ vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3] };
      vec_out = (vector unsigned long long){ 0, 0 };

      (*func)();
      dst  = (unsigned long long *)&vec_out;
      printf("%s: ", name);
      if (isLE)
         printf("%08x %08x %08x %08x ==> %016llx %016llx\n",
                vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3], dst[1], dst[0]);
      else
         printf("%08x %08x %08x %08x ==> %016llx %016llx\n",
                vwargs[i], vwargs[i+1], vwargs[i+2], vwargs[i+3], dst[0], dst[1]);
   }
}


static void test_int_stq_two_regs_imm16 (const char* name,
                                        test_func_t func_IN,
                                        unused uint32_t test_flags)
{
   /* Store quad word from register pair */
   int offs, k;
   HWord_t base;
   Word_t *iargs_priv;

   // private iargs table to store to, note storing pair of regs
   iargs_priv = memalign16(2 * sizeof(Word_t));

   base = (HWord_t)&iargs_priv[0];
   for (k = 0; k < 2; k++)  // clear array
      iargs_priv[k] = 0;

   offs = 0;

   /* setup source register pair */
   r14 = (HWord_t) 0xABCDEF0123456789ULL;
   r15 = (HWord_t) 0x1133557722446688ULL;

   r16 = base;                 // store to r16 + offs

   (*func_IN)();

#ifndef __powerpc64__
   printf("%s %08x,%08x, %2d => "
#else
   printf("%s %016llx,%016llx, %3d => "
#endif
            "%016llx,%016llx)\n",
            name, r14, r15, offs, iargs_priv[0], iargs_priv[1]);

   if (verbose) printf("\n");
   free(iargs_priv);
}


static void test_int_stq_three_regs (const char* name,
                                     test_func_t func_IN,
                                     unused uint32_t test_flags)
{
   /* Store quad word from register pair */
   volatile uint32_t flags, xer;
   int k;
   HWord_t base;

   base = (HWord_t)&mem_resv[0];
   for (k = 0; k < 2; k++)  // setup array for lqarx inst
      mem_resv[k] = k;

   /* setup source register pair for store */
   r14 = ZERO;
   r15 = ZERO;
   r16 = base;                 // store to r16 + r17
   r17 = ZERO;

   /* In order for the store to occur, the lqarx instruction must first
    * be used to load from the address thus creating a reservation at the
    * memory address.  The lqarx instruction is done in the test_stqcx(),
    * then registers 14, r15 are changed to the data to be stored in memory
    * by the stqcx instruction.
    */
   SET_CR_XER_ZERO;
   (*func_IN)();
   GET_CR_XER(flags,xer);
#ifndef __powerpc64__
   printf("%s %08x,%08x, =>  "
#else
   printf("%s %016llx,%016llx => "
#endif
            "%016llx,%016llx; CR=%08x\n",
            name, r14, r15, mem_resv[0], mem_resv[1], flags);

   if (verbose) printf("\n");
}

static void test_int_ldq_two_regs_imm16 (const char* name,
                                        test_func_t func_IN,
                                        unused uint32_t test_flags)
{
   /* load quad word from register pair */
   volatile uint32_t flags, xer;
   Word_t * mem_priv;
   HWord_t base;

   // private iargs table to store to, note storing pair of regs
   mem_priv = memalign16(2 * sizeof(Word_t));  // want 128-bits

   base = (HWord_t)&mem_priv[0];

   mem_priv[0] = 0xAACCEE0011335577ULL;
   mem_priv[1] = 0xABCDEF0123456789ULL;

   r14 = 0;
   r15 = 0;
   r16 = base;                 // fetch from r16 + offs
   SET_CR_XER_ZERO;
   (*func_IN)();
   GET_CR_XER(flags,xer);

#ifndef __powerpc64__
   printf("%s (0x%016llx, 0x%016llx) =>  (reg_pair = %08x,%08x)\n",
#else
   printf("%s (0x%016llx, 0x%016llx) =>  (reg_pair = 0x%016llx, 0x%016llx)\n",
#endif
          name, mem_priv[0], mem_priv[1], r14, r15);

   if (verbose) printf("\n");

   free(mem_priv);
}

static void test_int_ldq_three_regs (const char* name,
                                     test_func_t func_IN,
                                     unused uint32_t test_flags)
{
   /* load quad word from register pair */
   HWord_t base;

   base = (HWord_t)&mem_resv[0];

   mem_resv[0] = 0xAACCEE0011335577ULL;
   mem_resv[1] = 0xABCDEF0123456789ULL;

   r14 = 0;
   r15 = 0;
   r16 = base;                 // fetch from r16 + r17
   r17 = 0;

   (*func_IN)();

#ifndef __powerpc64__
   printf("%s (0x%016llx, 0x%016llx) =>  (reg_pair = 0x%08x, 0x%08x)\n",
#else
   printf("%s (0x%016llx, 0x%016llx) =>  (reg_pair = 0x%016llx, 0x%016llx)\n",
#endif
          name, mem_resv[0], mem_resv[1], r14, r15);
   if (verbose) printf("\n");

}

static void test_av_dint_three_args (const char* name, test_func_t func,
                                     unused uint32_t test_flags)
{

   unsigned long long * dst;
   int i,j, k;
   int family = test_flags & PPC_FAMILY;
   unsigned long long cin_vals[] = {
                                    // First pair of ULLs have LSB=0, so cin is '0'.
                                    // Second pair of ULLs have LSB=1, so cin is '1'.
                                    0xf000000000000000ULL, 0xf000000000000000ULL,
                                    0xf000000000000000ULL, 0xf000000000000001ULL
   };
   for (i = 0; i < NB_VDARGS - 1; i+=2) {
      if (isLE)
         vec_inA = (vector unsigned long long){ vdargs[i+1], vdargs[i] };
      else
         vec_inA = (vector unsigned long long){ vdargs[i], vdargs[i+1] };
      for (j = 0; j < NB_VDARGS - 1; j+=2) {
         if (isLE)
            vec_inB = (vector unsigned long long){ vdargs[j+1], vdargs[j] };
         else
            vec_inB = (vector unsigned long long){ vdargs[j], vdargs[j+1] };
         for (k = 0; k < 4 - 1; k+=2) {
            if (family == PPC_ALTIVECQ) {
               if (isLE)
                  vec_inC = (vector unsigned long long){ cin_vals[k+1], cin_vals[k] };
               else
                  vec_inC = (vector unsigned long long){ cin_vals[k], cin_vals[k+1] };
            } else {
               if (isLE)
                  vec_inC = (vector unsigned long long){ vdargs[k+1], vdargs[k] };
               else
                  vec_inC = (vector unsigned long long){ vdargs[k], vdargs[k+1] };
            }
            vec_out = (vector unsigned long long){ 0,0 };

            (*func)();
            dst  = (unsigned long long*)&vec_out;
            printf("%s: ", name);
            if (family == PPC_ALTIVECQ) {
               if (isLE)
                  printf("%016llx%016llx @@ %016llx%016llx @@ %llx ==> %016llx%016llx\n",
                         vdargs[i], vdargs[i+1], vdargs[j], vdargs[j+1], cin_vals[k+1],
                         dst[1], dst[0]);
               else
                  printf("%016llx%016llx @@ %016llx%016llx @@ %llx ==> %016llx%016llx\n",
                         vdargs[i], vdargs[i+1], vdargs[j], vdargs[j+1], cin_vals[k+1],
                         dst[0], dst[1]);
            } else {
               printf("%016llx @@ %016llx @@ %016llx ", vdargs[i], vdargs[j], vdargs[k]);
               if (isLE)
                  printf(" ==> %016llx\n", dst[1]);
               else
                  printf(" ==> %016llx\n", dst[0]);
               printf("\t%016llx @@ %016llx @@ %016llx ", vdargs[i+1], vdargs[j+1], vdargs[k+1]);
               if (isLE)
                  printf(" ==> %016llx\n", dst[0]);
               else
                  printf(" ==> %016llx\n", dst[1]);
            }
         }
      }
   }
}


/* The ALTIVEC_LOOPS and altive_loops defined below are used in do_tests.
 * Add new values to the end; do not change order, since the altivec_loops
 * array is indexed using the enumerated values defined by ALTIVEC_LOOPS.
 */
enum ALTIVEC_LOOPS {
   ALTV_MOV,
   ALTV_DINT,
   ALTV_INT_DRES,
   ALTV_DINT_IRES,
   ALTV_ONE_INT_DRES,
   ALTV_DINT_THREE_ARGS,
   ALTV_DINT_ONE_ARG,
   ALTV_SHA,
   ATLV_BCD
};

static test_loop_t altivec_loops[] = {
   &test_move_special,
   &test_av_dint_two_args,
   &test_av_wint_two_args_dres,
   &test_av_dint_to_int_two_args,
   &test_av_wint_one_arg_dres,
   &test_av_dint_three_args,
   &test_av_dint_one_arg,
   &test_av_dint_one_arg_SHA,
   &test_av_bcd,
   NULL
};

/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/
static test_loop_t int_loops[] = {
  /* The #defines for the family, number registers need the array
   * to be properly indexed.  This test is for the new ISA 2.0.7
   * instructions.  The infrastructure has been left for the momemnt
   */
   NULL, //&test_int_one_arg,
   NULL, //&test_int_two_args,
   NULL, //&test_int_three_args,
   NULL, //&test_int_two_args,
   NULL, //&test_int_one_reg_imm16,
   NULL, //&test_int_one_reg_imm16,
   NULL, //&test_int_special,
   NULL, //&test_int_ld_one_reg_imm16,
   NULL, //&test_int_ld_two_regs,
   NULL, //&test_int_st_two_regs_imm16,
   NULL, //&test_int_st_three_regs,
   &test_int_stq_two_regs_imm16,
   &test_int_ldq_two_regs_imm16,
   &test_int_stq_three_regs,
   &test_int_ldq_three_regs,
};

/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
   Must have NULL for last entry.
 */
static test_loop_t float_loops[] = {
   NULL,
   &test_float_two_args,
};


static test_t tests_fa_ops_two[] = {
    { &test_fmrgew          , "fmrgew", },
    { &test_fmrgow          , "fmrgow", },
    { NULL,                   NULL,           },
};

static test_table_t all_tests[] = {
   {
       tests_move_ops_spe,
       "PPC VSR special move insns",
       PPC_ALTIVECD | PPC_MOV | PPC_ONE_ARG,
   },
   {
       tests_aa_dbl_ops_two_args,
       "PPC altivec double word integer insns (arith, compare) with two args",
       PPC_ALTIVECD | PPC_ARITH | PPC_TWO_ARGS,
   },
   {
       tests_aa_word_ops_two_args_dres,
       "PPC altivec integer word instructions with two input args, double word result",
       PPC_ALTIVEC | PPC_ARITH_DRES | PPC_TWO_ARGS,
   },
   {
       tests_aa_dbl_to_int_two_args,
       "PPC altivec doubleword-to-integer instructions with two input args, saturated integer result",
       PPC_ALTIVECD | PPC_DOUBLE_IN_IRES | PPC_TWO_ARGS,
   },
   {
       tests_aa_word_ops_one_arg_dres,
       "PPC altivec integer word instructions with one input arg, double word result",
       PPC_ALTIVEC | PPC_ARITH_DRES | PPC_ONE_ARG,
   },
   {
      tests_istq_ops_two_i16,
      "PPC store quadword insns\n    with one register + one 16 bits immediate args with flags update",
      0x0001050c,
   },
   {
      tests_ildq_ops_two_i16,
      "PPC load quadword insns\n    with one register + one 16 bits immediate args with flags update",
      0x0001050d,
   },
   {
       tests_ldq_ops_three,
       "PPC load quadword insns\n    with three register args",
       0x0001050f,
   },
   {
       tests_stq_ops_three,
       "PPC store quadword insns\n    with three register args",
       0x0001050e,
   },
   {
       tests_fa_ops_two,
       "PPC floating point arith insns with two args",
       0x00020102,
   },
   {
       tests_aa_ops_three    ,
       "PPC altivec integer logical insns with three args",
       0x00060203,
   },
   {
       tests_aa_dbl_ops_one_arg,
       "PPC altivec one vector input arg, hex result",
       0x00060201,
   },
   {
       tests_aa_SHA_ops,
       "PPC altivec SSH insns",
       0x00040B01,
   },
   {
       tests_aa_bcd_ops,
       "PPC altivec BCD insns",
       0x00040B02,
   },
   {
       tests_aa_quadword_two_args,
       "PPC altivec quadword insns, two input args",
       0x00070102,
   },
   {
       tests_aa_quadword_three_args,
       "PPC altivec quadword insns, three input args",
       0x00070103
   },
   { NULL,                   NULL,               0x00000000, },
};

static void do_tests ( insn_sel_flags_t seln_flags,
                       char *filter)
{
   test_loop_t *loop;
   test_t *tests;
   int nb_args, type, family;
   int i, j, n;
   int exact;

   exact = check_filter(filter);
   n = 0;
   for (i=0; all_tests[i].name != NULL; i++) {
      nb_args = all_tests[i].flags & PPC_NB_ARGS;

      /* Check number of arguments */
      if ((nb_args == 1 && !seln_flags.one_arg) ||
          (nb_args == 2 && !seln_flags.two_args) ||
          (nb_args == 3 && !seln_flags.three_args)){
         continue;
      }
      /* Check instruction type */
      type = all_tests[i].flags & PPC_TYPE;
      if ((type == PPC_ARITH   && !seln_flags.arith)   ||
          (type == PPC_LOGICAL && !seln_flags.logical) ||
          (type == PPC_COMPARE && !seln_flags.compare) ||
          (type == PPC_LDST && !seln_flags.ldst)       ||
          (type == PPC_MOV && !seln_flags.ldst)       ||
          (type == PPC_POPCNT && !seln_flags.arith)) {
         continue;
      }

      /* Check instruction family */
      family = all_tests[i].flags & PPC_FAMILY;
      if ((family == PPC_INTEGER  && !seln_flags.integer) ||
          (family == PPC_FLOAT    && !seln_flags.floats)  ||
          (family == PPC_ALTIVEC && !seln_flags.altivec)  ||
          (family == PPC_ALTIVECD && !seln_flags.altivec)  ||
          (family == PPC_ALTIVECQ && !seln_flags.altivec)  ||
          (family == PPC_FALTIVEC && !seln_flags.faltivec)) {
         continue;
      }
      /* Check flags update */
      if (((all_tests[i].flags & PPC_CR)  && seln_flags.cr == 0) ||
          (!(all_tests[i].flags & PPC_CR) && seln_flags.cr == 1))
         continue;

      /* All passed, do the tests */
      tests = all_tests[i].tests;

      loop = NULL;

      /* Select the test loop */
      switch (family) {
      case PPC_INTEGER:
         mem_resv = memalign16(2 * sizeof(HWord_t));  // want 128-bits
         loop = &int_loops[nb_args - 1];
         break;

      case PPC_FLOAT:
         loop = &float_loops[nb_args - 1];
         break;

      case PPC_ALTIVECQ:
         if (nb_args == 2)
            loop = &altivec_loops[ALTV_DINT];
         else if (nb_args == 3)
            loop = &altivec_loops[ALTV_DINT_THREE_ARGS];
         break;
      case PPC_ALTIVECD:
         switch (type) {
         case PPC_MOV:
            loop = &altivec_loops[ALTV_MOV];
            break;
         case PPC_ARITH:
            loop = &altivec_loops[ALTV_DINT];
            break;
         case PPC_DOUBLE_IN_IRES:
            loop = &altivec_loops[ALTV_DINT_IRES];
            break;
         case PPC_LOGICAL:
            if (nb_args == 3)
               loop = &altivec_loops[ALTV_DINT_THREE_ARGS];
            else if (nb_args ==1)
               loop = &altivec_loops[ALTV_DINT_ONE_ARG];
            break;
         default:
            printf("No altivec test defined for type %x\n", type);
         }
         break;

      case PPC_FALTIVEC:
         printf("Currently there are no floating altivec tests in this testsuite.\n");
         break;

      case PPC_ALTIVEC:
         switch (type) {
         case PPC_ARITH_DRES:
         {
            switch (nb_args) {
            case 1:
               loop = &altivec_loops[ALTV_ONE_INT_DRES];
               break;
            case 2:
               loop = &altivec_loops[ALTV_INT_DRES];
               break;
            default:
               printf("No altivec test defined for number args %d\n", nb_args);
            }
            break;
         }
         case PPC_SHA_OR_BCD:
            if (nb_args == 1)
               loop = &altivec_loops[ALTV_SHA];
            else
               loop = &altivec_loops[ATLV_BCD];
            break;
         default:
            printf("No altivec test defined for type %x\n", type);
         }
         break;

      default:
         printf("ERROR: unknown insn family %08x\n", family);
         continue;
      }
      if (1 || verbose > 0)
      for (j=0; tests[j].name != NULL; j++) {
         if (check_name(tests[j].name, filter, exact)) {
            if (verbose > 1)
               printf("Test instruction %s\n", tests[j].name);
            if (loop != NULL)
               (*loop)(tests[j].name, tests[j].func, all_tests[i].flags);
            printf("\n");
            n++;
         }
        }
      if (verbose) printf("\n");
   }
   printf("All done. Tested %d different instructions\n", n);
}


static void usage (void)
{
   fprintf(stderr,
           "Usage: jm-insns [OPTION]\n"
           "\t-i: test integer instructions (default)\n"
           "\t-f: test floating point instructions\n"
           "\t-a: test altivec instructions\n"
           "\t-A: test all (int, fp, altivec) instructions\n"
           "\t-v: be verbose\n"
           "\t-h: display this help and exit\n"
           );
}

#endif

int main (int argc, char **argv)
{
#ifdef HAS_ISA_2_07
   /* Simple usage:
      ./jm-insns -i   => int insns
      ./jm-insns -f   => fp  insns
      ./jm-insns -a   => av  insns
      ./jm-insns -A   => int, fp and avinsns
   */
   char *filter = NULL;
   insn_sel_flags_t flags;
   int c;

   // Args
   flags.one_arg    = 1;
   flags.two_args   = 1;
   flags.three_args = 1;
   // Type
   flags.arith      = 1;
   flags.logical    = 1;
   flags.compare    = 1;
   flags.ldst       = 1;
   // Family
   flags.integer    = 0;
   flags.floats     = 0;
   flags.altivec    = 0;
   flags.faltivec   = 0;
   // Flags
   flags.cr         = 2;

   while ((c = getopt(argc, argv, "ifahvA")) != -1) {
      switch (c) {
      case 'i':
         flags.integer  = 1;
         break;
      case 'f':
         build_fargs_table();
         flags.floats   = 1;
         break;
      case 'a':
         flags.altivec  = 1;
         flags.faltivec = 1;
         break;
      case 'A':
         flags.integer  = 1;
         flags.floats   = 1;
         flags.altivec  = 1;
         flags.faltivec = 1;
         break;
      case 'h':
         usage();
         return 0;
      case 'v':
         verbose++;
         break;
      default:
         usage();
         fprintf(stderr, "Unknown argument: '%c'\n", c);
         return 1;
      }
   }

   arg_list_size = 0;

   build_vargs_table();
   if (verbose > 1) {
      printf("\nInstruction Selection:\n");
      printf("  n_args: \n");
      printf("    one_arg    = %d\n", flags.one_arg);
      printf("    two_args   = %d\n", flags.two_args);
      printf("    three_args = %d\n", flags.three_args);
      printf("  type: \n");
      printf("    arith      = %d\n", flags.arith);
      printf("    logical    = %d\n", flags.logical);
      printf("    compare    = %d\n", flags.compare);
      printf("    ldst       = %d\n", flags.ldst);
      printf("  family: \n");
      printf("    integer    = %d\n", flags.integer);
      printf("    floats     = %d\n", flags.floats);
      printf("    altivec    = %d\n", flags.altivec);
      printf("    faltivec   = %d\n", flags.faltivec);
      printf("  cr update: \n");
      printf("    cr         = %d\n", flags.cr);
      printf("\n");
   }

   do_tests( flags, filter );
#else
   printf("NO ISA 2.07 SUPPORT\n");
#endif
   return 0;
}
