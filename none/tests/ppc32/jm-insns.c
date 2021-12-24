
/* HOW TO COMPILE:

* 32bit build:
   gcc -Winline -Wall -g -O -mregnames -maltivec
* 64bit build:
   gcc -Winline -Wall -g -O -mregnames -maltivec -m64

This program is useful, but the register usage conventions in
it are a complete dog.  In particular, _patch_op_imm has to
be inlined, else you wind up with it segfaulting in
completely different places due to corruption (of r20 in the
case I chased).
*/

/*
 * test-ppc.c:
 * PPC tests for qemu-PPC CPU emulation checks
 *
 * Copyright (c) 2005 Jocelyn Mayer
 *
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

/*
 * Theory of operations:
 * a few registers are reserved for the test program:
 * r14 => r18
 * f14 => f18
 * I do preload test values in r14 thru r17 (or less, depending on the number
 * of register operands needed), patch the test opcode if any immediate
 * operands are required, execute the tested opcode.
 * XER, CCR and FPSCR are cleared before every test.
 * I always get the result in r17 and also save XER and CCR for fixed-point
 * operations. I also check FPSCR for floating points operations.
 *
 * Improvements:
 * a more clever FPSCR management is needed: for now, I always test
 * the round-to-zero case. Other rounding modes also need to be tested.
 */

/*
 * Operation details
 * -----------------
 * The 'test' functions (via all_tests[]) are wrappers of single asm instns
 *
 * The 'loops' (e.g. int_loops) do the actual work:
 *  - loops over as many arguments as the instn needs (regs | imms)
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
 *
 * Details of instruction patching for immediate operands
 * -----------------------------------------------------
 * All the immediate insn test functions are of the form {imm_insn, blr}
 * In order to patch one of these functions, we simply copy both insns
 * to a stack buffer, and rewrite the immediate part of imm_insn.
 * We then execute our stack buffer.
 * All ppc instructions are 32bits wide, which makes this fairly easy.
 *
 * Example:
 * extern void test_addi (void);
 * asm(".section \".text\"\n"
 *     "    .align 2\n"
 *     "    .type test_addi,@function\n"
 *     "test_addi:\n"
 *     "    addi\n"
 *     "    blr\n"
 *     "    .previous\n"
 *     );
 *
 * We are interested only in:
 *      "    addi         17, 14, 0\n"
 *      "    blr\n"
 *
 * In a loop test, we may see:
 * uint32_t func_buf[2];               // our new stack based 'function'
 * for imm...                          // loop over imm
 *   init_function( &func, func_buf );   // copy insns, set func ptr
 *   patch_op_imm16(&func_buf[0], imm);  // patch 'addi' insn
 *   ...
 *   (*func)();                              // exec our rewritten code
 *
 * patch_op_imm16() itself simply takes the uint32_t insn and overwrites
 * the immediate field with the new value (which, for 'addi', is the
 * low 16 bits).
 *
 * So in the loop test, if 'imm' is currently 9, and p[0] is:
 *   0x3A2E0000   => addi 17, 14, 0
 *
 * after patch_op_imm16(), func_buf[0] becomes:
 *   0x3A2E0009   => addi 17, 14, 9
 *
 * Note: init_function() needs to be called on every iteration
 *  - don't ask me why!
*/


/**********************************************************************/
/* Uncomment to enable many arguments for altivec insns */
#define USAGE_SIMPLE

/* Uncomment to enable many arguments for altivec insns */
//#define ALTIVEC_ARGS_LARGE

/* Uncomment to enable output of CR flags for float tests */
//#define TEST_FLOAT_FLAGS

/* Uncomment to enable debug output */
//#define DEBUG_ARGS_BUILD
//#define DEBUG_FILTER

/* These should be set at build time */
//#define NO_FLOAT
//#define HAS_ALTIVEC  // CFLAGS += -maltivec
//#define IS_PPC405
/**********************************************************************/


#include <stdint.h>
#include "tests/sys_mman.h"
#include "tests/malloc.h"       // memalign16
#include "./opcodes.h"

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
#else
typedef uint64_t  HWord_t;
#endif /* __powerpc64__ */

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


/* XXXX these must all be callee-save regs! */
register double f14 __asm__ ("fr14");
register double f15 __asm__ ("fr15");
register double f16 __asm__ ("fr16");
register double f17 __asm__ ("fr17");
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");

#include "config.h"         // HAS_ALTIVEC
#if defined (HAS_ALTIVEC)
#   include <altivec.h>
#endif
#include <assert.h>
#include <ctype.h>     // isspace
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>    // getopt


#ifndef __powerpc64__
#define ASSEMBLY_FUNC(__fname, __insn)     \
asm(".section \".text\"\n"                 \
    "\t.align 2\n"                         \
    "\t.type "__fname",@function\n"        \
    __fname":\n"                           \
    "\t"__insn"\n"                         \
    "\tblr\n"                              \
    "\t.previous\n"                        \
    )
#else
#if defined(VGP_ppc64be_linux)
#define ASSEMBLY_FUNC(__fname, __insn)     \
asm(".section  \".text\"\n"                \
    "\t.align 2\n"                         \
    "\t.global "__fname"\n"                \
    "\t.section \".opd\",\"aw\"\n"         \
    "\t.align 3\n"                         \
    ""__fname":\n"                         \
    "\t.quad ."__fname",.TOC.@tocbase,0\n" \
    "\t.previous\n"                        \
    "\t.type ."__fname",@function\n"       \
    "\t.global  ."__fname"\n"              \
    "."__fname":\n"                        \
    "\t"__insn"\n"                         \
    "\tblr\n"                              \
    )
#elif defined(VGP_ppc64le_linux)
#define ASSEMBLY_FUNC(__fname, __insn)     \
asm(".section  \".text\"\n"         \
    "\t.align 2\n"                         \
    "\t.global "__fname"\n"                \
    ""__fname":\n"                         \
    "\t"__insn"\n"                         \
    "\tblr\n"                              \
    )
#endif // VGP_ppc64 or VGP_ppc64le
#endif // #ifndef __powerpc64__


/* Return a pointer to a 1-page area where is is safe to both write
   and execute instructions.  Area is filled with 'trap' insns. */
static
uint32_t* get_rwx_area ( void )
{
   int i;
   static uint32_t* p = NULL;
   if (p == NULL) {
      p = mmap(NULL, 4096, PROT_READ|PROT_WRITE|PROT_EXEC,
                           MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
      assert(p != MAP_FAILED);
   }

   for (i = 0; i < 4096/sizeof(uint32_t); i++)
      p[i] = 0x7fe00008; /* trap */

   return p;
}


/* -------------- BEGIN #include "test-ppc.h" -------------- */
/*
 * test-ppc.h:
 * PPC tests for qemu-PPC CPU emulation checks - definitions
 * 
 * Copyright (c) 2005 Jocelyn Mayer
 * 
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

#if !defined (__TEST_PPC_H__)
#define __TEST_PPC_H__

#include <stdint.h>

typedef void (*test_func_t) (void);
typedef struct test_t test_t;
typedef struct test_table_t test_table_t;
struct test_t {
    test_func_t func;
    const char *name;
};

struct test_table_t {
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
    PPC_ONE_IMM    = 0x0000000C,  // PPC_MISC family
    PPC_NB_ARGS    = 0x0000000F,
    /* Type */
    PPC_ARITH      = 0x00000100,
    PPC_LOGICAL    = 0x00000200,
    PPC_COMPARE    = 0x00000300,
    PPC_CROP       = 0x00000400,
    PPC_LDST       = 0x00000500,
    PPC_POPCNT     = 0x00000600,
    PPC_ANY        = 0x00000700,
    PPC_TYPE       = 0x00000F00,
    /* Family */
    PPC_INTEGER    = 0x00010000,
    PPC_FLOAT      = 0x00020000,
    PPC_405        = 0x00030000,
    PPC_ALTIVEC    = 0x00040000,
    PPC_FALTIVEC   = 0x00050000,
    PPC_MISC       = 0x00060000,
    PPC_SH_ALGEBRAIC = 0x00070000,
    PPC_MFSPR      = 0x00080000,
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


/* Produce the 64-bit pattern corresponding to the supplied double. */
static uint64_t double_to_bits ( double d )
{
   union { uint64_t i; double d; } u;
   assert(8 == sizeof(uint64_t));
   assert(8 == sizeof(double));
   assert(8 == sizeof(u));
   u.d = d;
   return u.i;
}

#if 0
static float bits_to_float ( uint32_t i )
{
   union { uint32_t i; float f; } u;
   assert(4 == sizeof(uint32_t));
   assert(4 == sizeof(float));
   assert(4 == sizeof(u));
   u.i = i;
   return u.f;
}
#endif


#if defined (HAS_ALTIVEC)
static void AB_DPRINTF_VEC32x4 ( vector unsigned int v )
{
#if defined (DEBUG_ARGS_BUILD)
   int i;
   unsigned int* p_int = (unsigned int*)&v;
   AB_DPRINTF("val");
   for (i=0; i<4; i++) {
      AB_DPRINTF(" %08x", p_int[i]);
   }
   AB_DPRINTF("\n");
#endif
}
#endif


#define unused __attribute__ (( unused ))


/* -------------- BEGIN #include "ops-ppc.c" -------------- */

/* #include "test-ppc.h" */

static void test_add (void)
{
    __asm__ __volatile__ ("add          17, 14, 15");
}

static void test_addo (void)
{
    __asm__ __volatile__ ("addo         17, 14, 15");
}

static void test_addc (void)
{
    __asm__ __volatile__ ("addc         17, 14, 15");
}

static void test_addco (void)
{
    __asm__ __volatile__ ("addco        17, 14, 15");
}

static void test_divw (void)
{
    __asm__ __volatile__ ("divw         17, 14, 15");
}

static void test_divwo (void)
{
    __asm__ __volatile__ ("divwo        17, 14, 15");
}

static void test_divwu (void)
{
    __asm__ __volatile__ ("divwu        17, 14, 15");
}

static void test_divwuo (void)
{
    __asm__ __volatile__ ("divwuo       17, 14, 15");
}

static void test_mulhw (void)
{
    __asm__ __volatile__ ("mulhw        17, 14, 15");
}

static void test_mulhwu (void)
{
    __asm__ __volatile__ ("mulhwu       17, 14, 15");
}

static void test_mullw (void)
{
    __asm__ __volatile__ ("mullw        17, 14, 15");
}

static void test_mullwo (void)
{
    __asm__ __volatile__ ("mullwo       17, 14, 15");
}

static void test_subf (void)
{
    __asm__ __volatile__ ("subf         17, 14, 15");
}

static void test_subfo (void)
{
    __asm__ __volatile__ ("subfo        17, 14, 15");
}

static void test_subfc (void)
{
    __asm__ __volatile__ ("subfc        17, 14, 15");
}

static void test_subfco (void)
{
    __asm__ __volatile__ ("subfco       17, 14, 15");
}

#ifdef __powerpc64__
static void test_mulld (void)
{
    __asm__ __volatile__ ("mulld        17, 14, 15");
}

static void test_mulldo (void)
{
    __asm__ __volatile__ ("mulldo        17, 14, 15");
}

static void test_mulhd (void)
{
    __asm__ __volatile__ ("mulhd        17, 14, 15");
}

static void test_mulhdu (void)
{
    __asm__ __volatile__ ("mulhdu       17, 14, 15");
}

static void test_divd (void)
{
    __asm__ __volatile__ ("divd         17, 14, 15");
}

static void test_divdu (void)
{
    __asm__ __volatile__ ("divdu        17, 14, 15");
}

static void test_divdo (void)
{
    __asm__ __volatile__ ("divdo        17, 14, 15");
}

static void test_divduo (void)
{
    __asm__ __volatile__ ("divduo        17, 14, 15");
}
#endif // #ifdef __powerpc64__

static test_t tests_ia_ops_two[] = {
    { &test_add             , "         add", },
    { &test_addo            , "        addo", },
    { &test_addc            , "        addc", },
    { &test_addco           , "       addco", },
    { &test_divw            , "        divw", },
    { &test_divwo           , "       divwo", },
    { &test_divwu           , "       divwu", },
    { &test_divwuo          , "      divwuo", },
    { &test_mulhw           , "       mulhw", },
    { &test_mulhwu          , "      mulhwu", },
    { &test_mullw           , "       mullw", },
    { &test_mullwo          , "      mullwo", },
    { &test_subf            , "        subf", },
    { &test_subfo           , "       subfo", },
    { &test_subfc           , "       subfc", },
    { &test_subfco          , "      subfco", },
#ifdef __powerpc64__
    { &test_mulhd           , "       mulhd", },
    { &test_mulhdu          , "      mulhdu", },
    { &test_mulld           , "       mulld", },
    { &test_mulldo          , "      mulldo", },
    { &test_divd            , "        divd", },
    { &test_divdu           , "       divdu", },
    { &test_divdo           , "       divdo", },
    { &test_divduo          , "      divduo", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_add_ (void)
{
    __asm__ __volatile__ ("add.         17, 14, 15");
}

static void test_addo_ (void)
{
    __asm__ __volatile__ ("addo.        17, 14, 15");
}

static void test_addc_ (void)
{
    __asm__ __volatile__ ("addc.        17, 14, 15");
}

static void test_addco_ (void)
{
    __asm__ __volatile__ ("addco.       17, 14, 15");
}

static void test_divw_ (void)
{
    __asm__ __volatile__ ("divw.        17, 14, 15");
}

static void test_divwo_ (void)
{
    __asm__ __volatile__ ("divwo.       17, 14, 15");
}

static void test_divwu_ (void)
{
    __asm__ __volatile__ ("divwu.       17, 14, 15");
}

static void test_divwuo_ (void)
{
    __asm__ __volatile__ ("divwuo.      17, 14, 15");
}

static void test_mulhw_ (void)
{
    __asm__ __volatile__ ("mulhw.       17, 14, 15");
}

static void test_mulhwu_ (void)
{
    __asm__ __volatile__ ("mulhwu.      17, 14, 15");
}

static void test_mullw_ (void)
{
    __asm__ __volatile__ ("mullw.       17, 14, 15");
}

static void test_mullwo_ (void)
{
    __asm__ __volatile__ ("mullwo.      17, 14, 15");
}

static void test_subf_ (void)
{
    __asm__ __volatile__ ("subf.        17, 14, 15");
}

static void test_subfo_ (void)
{
    __asm__ __volatile__ ("subfo.       17, 14, 15");
}

static void test_subfc_ (void)
{
    __asm__ __volatile__ ("subfc.       17, 14, 15");
}

static void test_subfco_ (void)
{
    __asm__ __volatile__ ("subfco.      17, 14, 15");
}

#ifdef __powerpc64__
static void test_mulhd_ (void)
{
    __asm__ __volatile__ ("mulhd.       17, 14, 15");
}

static void test_mulhdu_ (void)
{
    __asm__ __volatile__ ("mulhdu.      17, 14, 15");
}

static void test_mulld_ (void)
{
    __asm__ __volatile__ ("mulld.       17, 14, 15");
}

static void test_mulldo_ (void)
{
    __asm__ __volatile__ ("mulldo.       17, 14, 15");
}

static void test_divd_ (void)
{
    __asm__ __volatile__ ("divd.        17, 14, 15");
}

static void test_divdu_ (void)
{
    __asm__ __volatile__ ("divdu.       17, 14, 15");
}

static void test_divdo_ (void)
{
    __asm__ __volatile__ ("divdo.        17, 14, 15");
}

static void test_divduo_ (void)
{
    __asm__ __volatile__ ("divduo.       17, 14, 15");
}
#endif // #ifdef __powerpc64__

static test_t tests_iar_ops_two[] = {
    { &test_add_            , "        add.", },
    { &test_addo_           , "       addo.", },
    { &test_addc_           , "       addc.", },
    { &test_addco_          , "      addco.", },
    { &test_divw_           , "       divw.", },
    { &test_divwo_          , "      divwo.", },
    { &test_divwu_          , "      divwu.", },
    { &test_divwuo_         , "     divwuo.", },
    { &test_mulhw_          , "      mulhw.", },
    { &test_mulhwu_         , "     mulhwu.", },
    { &test_mullw_          , "      mullw.", },
    { &test_mullwo_         , "     mullwo.", },
    { &test_subf_           , "       subf.", },
    { &test_subfo_          , "      subfo.", },
    { &test_subfc_          , "      subfc.", },
    { &test_subfco_         , "     subfco.", },
#ifdef __powerpc64__
    { &test_mulhd_          , "      mulhd.", },
    { &test_mulhdu_         , "     mulhdu.", },
    { &test_mulld_          , "      mulld.", },
    { &test_mulldo_          , "    mulldo.", },
    { &test_divd_           , "       divd.", },
    { &test_divdu_          , "      divdu.", },
    { &test_divdo_          , "      divdo.", },
    { &test_divduo_         , "     divduo.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_adde (void)
{
    __asm__ __volatile__ ("adde         17, 14, 15");
}

static void test_addeo (void)
{
    __asm__ __volatile__ ("addeo        17, 14, 15");
}

static void test_subfe (void)
{
    __asm__ __volatile__ ("subfe        17, 14, 15");
}

static void test_subfeo (void)
{
    __asm__ __volatile__ ("subfeo       17, 14, 15");
}

static test_t tests_iac_ops_two[] = {
    { &test_adde            , "        adde", },
    { &test_addeo           , "       addeo", },
    { &test_subfe           , "       subfe", },
    { &test_subfeo          , "      subfeo", },
    { NULL,                   NULL,           },
};

static void test_adde_ (void)
{
    __asm__ __volatile__ ("adde.        17, 14, 15");
}

static void test_addeo_ (void)
{
    __asm__ __volatile__ ("addeo.       17, 14, 15");
}

static void test_subfe_ (void)
{
    __asm__ __volatile__ ("subfe.       17, 14, 15");
}

static void test_subfeo_ (void)
{
    __asm__ __volatile__ ("subfeo.      17, 14, 15");
}

static test_t tests_iacr_ops_two[] = {
    { &test_adde_           , "       adde.", },
    { &test_addeo_          , "      addeo.", },
    { &test_subfe_          , "      subfe.", },
    { &test_subfeo_         , "     subfeo.", },
    { NULL,                   NULL,           },
};

static void test_and (void)
{
    __asm__ __volatile__ ("and          17, 14, 15");
}

static void test_andc (void)
{
    __asm__ __volatile__ ("andc         17, 14, 15");
}

static void test_eqv (void)
{
    __asm__ __volatile__ ("eqv          17, 14, 15");
}

static void test_nand (void)
{
    __asm__ __volatile__ ("nand         17, 14, 15");
}

static void test_nor (void)
{
    __asm__ __volatile__ ("nor          17, 14, 15");
}

static void test_or (void)
{
    __asm__ __volatile__ ("or           17, 14, 15");
}

static void test_orc (void)
{
    __asm__ __volatile__ ("orc          17, 14, 15");
}

static void test_xor (void)
{
    __asm__ __volatile__ ("xor          17, 14, 15");
}

static void test_slw (void)
{
    __asm__ __volatile__ ("slw          17, 14, 15");
}

static void test_sraw (void)
{
    __asm__ __volatile__ ("sraw         17, 14, 15");
}

static void test_srw (void)
{
    __asm__ __volatile__ ("srw          17, 14, 15");
}

#ifdef __powerpc64__
static void test_sld (void)
{
    __asm__ __volatile__ ("sld          17, 14, 15");
}

static void test_srad (void)
{
    __asm__ __volatile__ ("srad         17, 14, 15");
}

static void test_srd (void)
{
    __asm__ __volatile__ ("srd          17, 14, 15");
}
#endif // #ifdef __powerpc64__

static test_t tests_il_ops_two_sh[] = {
    { &test_sraw            , "        sraw", },
#ifdef __powerpc64__
    { &test_srad            , "        srad", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static test_t tests_il_ops_two[] = {
    { &test_and             , "         and", },
    { &test_andc            , "        andc", },
    { &test_eqv             , "         eqv", },
    { &test_nand            , "        nand", },
    { &test_nor             , "         nor", },
    { &test_or              , "          or", },
    { &test_orc             , "         orc", },
    { &test_xor             , "         xor", },
    { &test_slw             , "         slw", },
    { &test_srw             , "         srw", },
#ifdef __powerpc64__
    { &test_sld             , "         sld", },
    { &test_srd             , "         srd", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_and_ (void)
{
    __asm__ __volatile__ ("and.         17, 14, 15");
}

static void test_andc_ (void)
{
    __asm__ __volatile__ ("andc.        17, 14, 15");
}

static void test_eqv_ (void)
{
    __asm__ __volatile__ ("eqv.         17, 14, 15");
}

static void test_nand_ (void)
{
    __asm__ __volatile__ ("nand.        17, 14, 15");
}

static void test_nor_ (void)
{
    __asm__ __volatile__ ("nor.         17, 14, 15");
}

static void test_or_ (void)
{
    __asm__ __volatile__ ("or.          17, 14, 15");
}

static void test_orc_ (void)
{
    __asm__ __volatile__ ("orc.         17, 14, 15");
}

static void test_xor_ (void)
{
    __asm__ __volatile__ ("xor.         17, 14, 15");
}

static void test_slw_ (void)
{
    __asm__ __volatile__ ("slw.         17, 14, 15");
}

static void test_sraw_ (void)
{
    __asm__ __volatile__ ("sraw.        17, 14, 15");
}

static void test_srw_ (void)
{
    __asm__ __volatile__ ("srw.         17, 14, 15");
}

#ifdef __powerpc64__
static void test_sld_ (void)
{
    __asm__ __volatile__ ("sld.         17, 14, 15");
}

static void test_srad_ (void)
{
    __asm__ __volatile__ ("srad.        17, 14, 15");
}

static void test_srd_ (void)
{
    __asm__ __volatile__ ("srd.         17, 14, 15");
}
#endif // #ifdef __powerpc64__

static test_t tests_ilr_ops_two_sh[] = {
    { &test_sraw_           , "       sraw.", },
#ifdef __powerpc64__
    { &test_srad_           , "       srad.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static test_t tests_ilr_ops_two[] = {
    { &test_and_            , "        and.", },
    { &test_andc_           , "       andc.", },
    { &test_eqv_            , "        eqv.", },
    { &test_nand_           , "       nand.", },
    { &test_nor_            , "        nor.", },
    { &test_or_             , "         or.", },
    { &test_orc_            , "        orc.", },
    { &test_xor_            , "        xor.", },
    { &test_slw_            , "        slw.", },
    { &test_srw_            , "        srw.", },
#ifdef __powerpc64__
    { &test_sld_            , "        sld.", },
    { &test_srd_            , "        srd.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_cmpw (void)
{
    __asm__ __volatile__ ("cmpw         2, 14, 15");
}

static void test_cmplw (void)
{
    __asm__ __volatile__ ("cmplw        2, 14, 15");
}

#ifdef __powerpc64__
static void test_cmpd (void)
{
    __asm__ __volatile__ ("cmpd         2, 14, 15");
}

static void test_cmpld (void)
{
    __asm__ __volatile__ ("cmpld        2, 14, 15");
}
#endif // #ifdef __powerpc64__

static test_t tests_icr_ops_two[] = {
    { &test_cmpw            , "        cmpw", },
    { &test_cmplw           , "       cmplw", },
#ifdef __powerpc64__
    { &test_cmpd            , "        cmpd", },
    { &test_cmpld           , "       cmpld", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_cmpwi (void);
ASSEMBLY_FUNC("test_cmpwi", "cmpwi         2, 14, 0");

extern void test_cmplwi (void);
ASSEMBLY_FUNC("test_cmplwi", "cmplwi        2, 14, 0");

#ifdef __powerpc64__
extern void test_cmpdi (void);
ASSEMBLY_FUNC("test_cmpdi", "cmpdi        2, 14, 0");

extern void test_cmpldi (void);
ASSEMBLY_FUNC("test_cmpldi", "cmpldi       2, 14, 0");
#endif // #ifdef __powerpc64__

static test_t tests_icr_ops_two_i16[] = {
    { &test_cmpwi           , "       cmpwi", },
    { &test_cmplwi          , "      cmplwi", },
#ifdef __powerpc64__
    { &test_cmpdi           , "       cmpdi", },
    { &test_cmpldi          , "      cmpldi", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_addi (void);
ASSEMBLY_FUNC("test_addi", "addi         17, 14, 0");

extern void test_addic (void);
ASSEMBLY_FUNC("test_addic", "addic        17, 14, 0");

extern void test_addis (void);
ASSEMBLY_FUNC("test_addis", "addis        17, 14, 0");

extern void test_mulli (void);
ASSEMBLY_FUNC("test_mulli", "mulli        17, 14, 0");

extern void test_subfic (void);
ASSEMBLY_FUNC("test_subfic", "subfic       17, 14, 0");

static test_t tests_ia_ops_two_i16[] = {
    { &test_addi            , "        addi", },
    { &test_addic           , "       addic", },
    { &test_addis           , "       addis", },
    { &test_mulli           , "       mulli", },
    { &test_subfic          , "      subfic", },
    { NULL,                   NULL,           },
};

extern void test_addic_ (void);
ASSEMBLY_FUNC("test_addic_", "addic.       17, 14, 0");

static test_t tests_iar_ops_two_i16[] = {
    { &test_addic_          , "      addic.", },
    { NULL,                   NULL,           },
};

extern void test_ori (void);
ASSEMBLY_FUNC("test_ori", "ori       17, 14, 0");

extern void test_oris (void);
ASSEMBLY_FUNC("test_oris", "oris       17, 14, 0");

extern void test_xori (void);
ASSEMBLY_FUNC("test_xori", "xori       17, 14, 0");

extern void test_xoris (void);
ASSEMBLY_FUNC("test_xoris", "xoris       17, 14, 0");

static test_t tests_il_ops_two_i16[] = {
    { &test_ori             , "         ori", },
    { &test_oris            , "        oris", },
    { &test_xori            , "        xori", },
    { &test_xoris           , "       xoris", },
    { NULL,                   NULL,           },
};

extern void test_andi_ (void);
ASSEMBLY_FUNC("test_andi_", "andi.       17, 14, 0");

extern void test_andis_ (void);
ASSEMBLY_FUNC("test_andis_", "andis.      17, 14, 0");

static test_t tests_ilr_ops_two_i16[] = {
    { &test_andi_           , "       andi.", },
    { &test_andis_          , "      andis.", },
    { NULL,                   NULL,           },
};

static void test_crand (void)
{
    __asm__ __volatile__ ("crand        17, 14, 15");
}

static void test_crandc (void)
{
    __asm__ __volatile__ ("crandc       17, 14, 15");
}

static void test_creqv (void)
{
    __asm__ __volatile__ ("creqv        17, 14, 15");
}

static void test_crnand (void)
{
    __asm__ __volatile__ ("crnand       17, 14, 15");
}

static void test_crnor (void)
{
    __asm__ __volatile__ ("crnor        17, 14, 15");
}

static void test_cror (void)
{
    __asm__ __volatile__ ("cror         17, 14, 15");
}

static void test_crorc (void)
{
    __asm__ __volatile__ ("crorc        17, 14, 15");
}

static void test_crxor (void)
{
    __asm__ __volatile__ ("crxor        17, 14, 15");
}

static test_t tests_crl_ops_two[] = {
    { &test_crand           , "       crand", },
    { &test_crandc          , "      crandc", },
    { &test_creqv           , "       creqv", },
    { &test_crnand          , "      crnand", },
    { &test_crnor           , "       crnor", },
    { &test_cror            , "        cror", },
    { &test_crorc           , "       crorc", },
    { &test_crxor           , "       crxor", },
    { NULL,                   NULL,           },
};

static void test_addme (void)
{
    __asm__ __volatile__ ("addme        17, 14");
}

static void test_addmeo (void)
{
    __asm__ __volatile__ ("addmeo       17, 14");
}

static void test_addze (void)
{
    __asm__ __volatile__ ("addze        17, 14");
}

static void test_addzeo (void)
{
    __asm__ __volatile__ ("addzeo       17, 14");
}

static void test_subfme (void)
{
    __asm__ __volatile__ ("subfme       17, 14");
}

static void test_subfmeo (void)
{
    __asm__ __volatile__ ("subfmeo      17, 14");
}

static void test_subfze (void)
{
    __asm__ __volatile__ ("subfze       17, 14");
}

static void test_subfzeo (void)
{
    __asm__ __volatile__ ("subfzeo      17, 14");
}

static test_t tests_iac_ops_one[] = {
    { &test_addme           , "       addme", },
    { &test_addmeo          , "      addmeo", },
    { &test_addze           , "       addze", },
    { &test_addzeo          , "      addzeo", },
    { &test_subfme          , "      subfme", },
    { &test_subfmeo         , "     subfmeo", },
    { &test_subfze          , "      subfze", },
    { &test_subfzeo         , "     subfzeo", },
    { NULL,                   NULL,           },
};

static void test_addme_ (void)
{
    __asm__ __volatile__ ("addme.       17, 14");
}

static void test_addmeo_ (void)
{
    __asm__ __volatile__ ("addmeo.      17, 14");
}

static void test_addze_ (void)
{
    __asm__ __volatile__ ("addze.       17, 14");
}

static void test_addzeo_ (void)
{
    __asm__ __volatile__ ("addzeo.      17, 14");
}

static void test_subfme_ (void)
{
    __asm__ __volatile__ ("subfme.      17, 14");
}

static void test_subfmeo_ (void)
{
    __asm__ __volatile__ ("subfmeo.     17, 14");
}

static void test_subfze_ (void)
{
    __asm__ __volatile__ ("subfze.      17, 14");
}

static void test_subfzeo_ (void)
{
    __asm__ __volatile__ ("subfzeo.     17, 14");
}

static test_t tests_iacr_ops_one[] = {
    { &test_addme_          , "      addme.", },
    { &test_addmeo_         , "     addmeo.", },
    { &test_addze_          , "      addze.", },
    { &test_addzeo_         , "     addzeo.", },
    { &test_subfme_         , "     subfme.", },
    { &test_subfmeo_        , "    subfmeo.", },
    { &test_subfze_         , "     subfze.", },
    { &test_subfzeo_        , "    subfzeo.", },
    { NULL,                   NULL,           },
};

static void test_cntlzw (void)
{
    __asm__ __volatile__ ("cntlzw       17, 14");
}

static void test_extsb (void)
{
    __asm__ __volatile__ ("extsb        17, 14");
}

static void test_extsh (void)
{
    __asm__ __volatile__ ("extsh        17, 14");
}

static void test_neg (void)
{
    __asm__ __volatile__ ("neg          17, 14");
}

static void test_nego (void)
{
    __asm__ __volatile__ ("nego         17, 14");
}

#ifdef __powerpc64__
static void test_cntlzd (void)
{
    __asm__ __volatile__ ("cntlzd       17, 14");
}

static void test_extsw (void)
{
    __asm__ __volatile__ ("extsw        17, 14");
}
#endif // #ifdef __powerpc64__

static test_t tests_il_ops_one[] = {
    { &test_cntlzw          , "      cntlzw", },
    { &test_extsb           , "       extsb", },
    { &test_extsh           , "       extsh", },
    { &test_neg             , "         neg", },
    { &test_nego            , "        nego", },
#ifdef __powerpc64__
    { &test_cntlzd          , "      cntlzd", },
    { &test_extsw           , "       extsw", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_cntlzw_ (void)
{
    __asm__ __volatile__ ("cntlzw.      17, 14");
}

static void test_extsb_ (void)
{
    __asm__ __volatile__ ("extsb.       17, 14");
}

static void test_extsh_ (void)
{
    __asm__ __volatile__ ("extsh.       17, 14");
}

static void test_neg_ (void)
{
    __asm__ __volatile__ ("neg.         17, 14");
}

static void test_nego_ (void)
{
    __asm__ __volatile__ ("nego.        17, 14");
}

#ifdef __powerpc64__
static void test_cntlzd_ (void)
{
    __asm__ __volatile__ ("cntlzd.      17, 14");
}

static void test_extsw_ (void)
{
    __asm__ __volatile__ ("extsw.       17, 14");
}
#endif // #ifdef __powerpc64__

static test_t tests_ilr_ops_one[] = {
    { &test_cntlzw_         , "     cntlzw.", },
    { &test_extsb_          , "      extsb.", },
    { &test_extsh_          , "      extsh.", },
    { &test_neg_            , "        neg.", },
    { &test_nego_           , "       nego.", },
#ifdef __powerpc64__
    { &test_cntlzd_         , "     cntlzd.", },
    { &test_extsw_          , "      extsw.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_rlwimi (void);
ASSEMBLY_FUNC("test_rlwimi", "rlwimi      17, 14, 0, 0, 0");

extern void test_rlwinm (void);
ASSEMBLY_FUNC("test_rlwinm", "rlwinm      17, 14, 0, 0, 0");

extern void test_rlwnm (void);
ASSEMBLY_FUNC("test_rlwnm", "rlwnm      17, 14, 15, 0, 0");

extern void test_srawi (void);
ASSEMBLY_FUNC("test_srawi", "srawi      17, 14, 0");

static void test_mfcr (void)
{
    __asm__ __volatile__ ("mfcr         17");
}

static void test_mfspr (void)
{
    __asm__ __volatile__ ("mfspr        17, 1");
}

static void test_mtspr (void)
{
    __asm__ __volatile__ ("mtspr        1, 14");
}

#ifdef __powerpc64__
extern void test_rldcl (void);
ASSEMBLY_FUNC("test_rldcl", "rldcl       17, 14, 15, 0");

extern void test_rldcr (void);
ASSEMBLY_FUNC("test_rldcr", "rldcr       17, 14, 15, 0");

extern void test_rldic (void);
ASSEMBLY_FUNC("test_rldic", "rldic       17, 14, 0, 0");

extern void test_rldicl (void);
ASSEMBLY_FUNC("test_rldicl", "rldicl      17, 14, 0, 0");

extern void test_rldicr (void);
ASSEMBLY_FUNC("test_rldicr", "rldicr      17, 14, 0, 0");

extern void test_rldimi (void);
ASSEMBLY_FUNC("test_rldimi", "rldimi      17, 14, 0, 0");

extern void test_sradi (void);
ASSEMBLY_FUNC("test_sradi", "sradi      17, 14, 0");
#endif // #ifdef __powerpc64__

static test_t tests_il_ops_spe_sh[] = {
    { &test_srawi           , "       srawi", },
#ifdef __powerpc64__
    { &test_sradi           , "       sradi", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};
static test_t tests_il_ops_mfspr[] = {
    { &test_mfspr           , "       mfspr", },
    { &test_mtspr           , "       mtspr", },
    { NULL,                   NULL,           },
};

static test_t tests_il_ops_spe[] = {
    { &test_rlwimi          , "      rlwimi", },
    { &test_rlwinm          , "      rlwinm", },
    { &test_rlwnm           , "       rlwnm", },
    { &test_mfcr            , "        mfcr", },
#ifdef __powerpc64__
    { &test_rldcl           , "       rldcl", },
    { &test_rldcr           , "       rldcr", },
    { &test_rldic           , "       rldic", },
    { &test_rldicl          , "      rldicl", },
    { &test_rldicr          , "      rldicr", },
    { &test_rldimi          , "      rldimi", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_rlwimi_ (void);
ASSEMBLY_FUNC("test_rlwimi_", "rlwimi.      17, 14, 0, 0, 0");

extern void test_rlwinm_ (void);
ASSEMBLY_FUNC("test_rlwinm_", "rlwinm.      17, 14, 0, 0, 0");

extern void test_rlwnm_ (void);
ASSEMBLY_FUNC("test_rlwnm_", "rlwnm.      17, 14, 15, 0, 0");

extern void test_srawi_ (void);
ASSEMBLY_FUNC("test_srawi_", "srawi.      17, 14, 0");

extern void test_mcrf (void);
ASSEMBLY_FUNC("test_mcrf", "mcrf      0, 0");

extern void test_mcrxr (void);
ASSEMBLY_FUNC("test_mcrxr", ".machine push; .machine power6;" \
              "mcrxr 0; .machine pop");

extern void test_mtcrf (void);
ASSEMBLY_FUNC("test_mtcrf", "mtcrf      0, 14");

#ifdef __powerpc64__
extern void test_rldcl_ (void);
ASSEMBLY_FUNC("test_rldcl_", "rldcl.      17, 14, 15, 0");

extern void test_rldcr_ (void);
ASSEMBLY_FUNC("test_rldcr_", "rldcr.      17, 14, 15, 0");

extern void test_rldic_ (void);
ASSEMBLY_FUNC("test_rldic_", "rldic.      17, 14, 0, 0");

extern void test_rldicl_ (void);
ASSEMBLY_FUNC("test_rldicl_", "rldicl.     17, 14, 0, 0");

extern void test_rldicr_ (void);
ASSEMBLY_FUNC("test_rldicr_", "rldicr.     17, 14, 0, 0");

extern void test_rldimi_ (void);
ASSEMBLY_FUNC("test_rldimi_", "rldimi.     17, 14, 0, 0");

extern void test_sradi_ (void);
ASSEMBLY_FUNC("test_sradi_", "sradi.      17, 14, 0");
#endif // #ifdef __powerpc64__

static test_t tests_ilr_ops_spe_sh[] = {
    { &test_srawi_          , "      srawi.", },
#ifdef __powerpc64__
    { &test_sradi_          , "      sradi.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};
static test_t tests_ilr_ops_spe[] = {
    { &test_rlwimi_         , "     rlwimi.", },
    { &test_rlwinm_         , "     rlwinm.", },
    { &test_rlwnm_          , "      rlwnm.", },
    { &test_mcrf            , "        mcrf", },
    { &test_mcrxr           , "       mcrxr", },
    { &test_mtcrf           , "       mtcrf", },
#ifdef __powerpc64__
    { &test_rldcl_          , "      rldcl.", },
    { &test_rldcr_          , "      rldcr.", },
    { &test_rldic_          , "      rldic.", },
    { &test_rldicl_         , "     rldicl.", },
    { &test_rldicr_         , "     rldicr.", },
    { &test_rldimi_         , "     rldimi.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_lbz (void);
ASSEMBLY_FUNC("test_lbz", "lbz          17,0(14)");

extern void test_lbzu (void);
ASSEMBLY_FUNC("test_lbzu", "lbzu          17,0(14)");

extern void test_lha (void);
ASSEMBLY_FUNC("test_lha", "lha          17,0(14)");

extern void test_lhau (void);
ASSEMBLY_FUNC("test_lhau", "lhau          17,0(14)");

extern void test_lhz (void);
ASSEMBLY_FUNC("test_lhz", "lhz          17,0(14)");

extern void test_lhzu (void);
ASSEMBLY_FUNC("test_lhzu", "lhzu         17,0(14)");

extern void test_lwz (void);
ASSEMBLY_FUNC("test_lwz", "lwz          17,0(14)");

extern void test_lwzu (void);
ASSEMBLY_FUNC("test_lwzu", "lwzu          17,0(14)");

#ifdef __powerpc64__
extern void test_ld (void);
ASSEMBLY_FUNC("test_ld", "ld            17,0(14)");

extern void test_ldu (void);
ASSEMBLY_FUNC("test_ldu", "ldu           17,0(14)");

extern void test_lwa (void);
ASSEMBLY_FUNC("test_lwa", "lwa           17,0(14)");
#endif // #ifdef __powerpc64__

static test_t tests_ild_ops_two_i16[] = {
    { &test_lbz             , "         lbz", },
    { &test_lbzu            , "        lbzu", },
    { &test_lha             , "         lha", },
    { &test_lhau            , "        lhau", },
    { &test_lhz             , "         lhz", },
    { &test_lhzu            , "        lhzu", },
    { &test_lwz             , "         lwz", },
    { &test_lwzu            , "        lwzu", },
#ifdef __powerpc64__
    { &test_ld              , "          ld", },
    { &test_ldu             , "         ldu", },
    { &test_lwa             , "         lwa", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_lbzx (void)
{
    __asm__ __volatile__ ("lbzx         17,14,15");
}

static void test_lbzux (void)
{
    __asm__ __volatile__ ("lbzux        17,14,15");
}

static void test_lhax (void)
{
    __asm__ __volatile__ ("lhax         17,14,15");
}

static void test_lhaux (void)
{
    __asm__ __volatile__ ("lhaux        17,14,15");
}

static void test_lhzx (void)
{
    __asm__ __volatile__ ("lhzx         17,14,15");
}

static void test_lhzux (void)
{
    __asm__ __volatile__ ("lhzux        17,14,15");
}

static void test_lwzx (void)
{
    __asm__ __volatile__ ("lwzx         17,14,15");
}

static void test_lwzux (void)
{
    __asm__ __volatile__ ("lwzux        17,14,15");
}

#ifdef __powerpc64__
static void test_ldx (void)
{
    __asm__ __volatile__ ("ldx         17,14,15");
}

static void test_ldux (void)
{
    __asm__ __volatile__ ("ldux        17,14,15");
}

static void test_lwax (void)
{
    __asm__ __volatile__ ("lwax        17,14,15");
}

static void test_lwaux (void)
{
    __asm__ __volatile__ ("lwaux        17,14,15");
}
#endif // #ifdef __powerpc64__

static test_t tests_ild_ops_two[] = {
    { &test_lbzx            , "        lbzx", },
    { &test_lbzux           , "       lbzux", },
    { &test_lhax            , "        lhax", },
    { &test_lhaux           , "       lhaux", },
    { &test_lhzx            , "        lhzx", },
    { &test_lhzux           , "       lhzux", },
    { &test_lwzx            , "        lwzx", },
    { &test_lwzux           , "       lwzux", },
#ifdef __powerpc64__
    { &test_ldx             , "         ldx", },
    { &test_ldux            , "        ldux", },
    { &test_lwax            , "        lwax", },
    { &test_lwaux           , "       lwaux", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

extern void test_stb (void);
ASSEMBLY_FUNC("test_stb", "stb          14,0(15)");

extern void test_stbu (void);
ASSEMBLY_FUNC("test_stbu", "stbu          14,0(15)");

extern void test_sth (void);
ASSEMBLY_FUNC("test_sth", "sth          14,0(15)");

extern void test_sthu (void);
ASSEMBLY_FUNC("test_sthu", "sthu         14,0(15)");

extern void test_stw (void);
ASSEMBLY_FUNC("test_stw", "stw          14,0(15)");

extern void test_stwu (void);
ASSEMBLY_FUNC("test_stwu", "stwu          14,0(15)");

#ifdef __powerpc64__
extern void test_std (void);
ASSEMBLY_FUNC("test_std", "std          14,0(15)");

extern void test_stdu (void);
ASSEMBLY_FUNC("test_stdu", "stdu          14,0(15)");
#endif // #ifdef __powerpc64__

static test_t tests_ist_ops_three_i16[] = {
    { &test_stb             , "         stb", },
    { &test_stbu            , "        stbu", },
    { &test_sth             , "         sth", },
    { &test_sthu            , "        sthu", },
    { &test_stw             , "         stw", },
    { &test_stwu            , "        stwu", },
#ifdef __powerpc64__
    { &test_std             , "         std", },
    { &test_stdu            , "        stdu", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void test_stbx (void)
{
    __asm__ __volatile__ ("stbx         14,15,16");
}

static void test_stbux (void)
{
    __asm__ __volatile__ ("stbux        14,15,16");
}

static void test_sthx (void)
{
    __asm__ __volatile__ ("sthx         14,15,16");
}

static void test_sthux (void)
{
    __asm__ __volatile__ ("sthux        14,15,16");
}

static void test_stwx (void)
{
    __asm__ __volatile__ ("stwx         14,15,16");
}

static void test_stwux (void)
{
    __asm__ __volatile__ ("stwux        14,15,16");
}

#ifdef __powerpc64__
static void test_stdx (void)
{
    __asm__ __volatile__ ("stdx         14,15,16");
}

static void test_stdux (void)
{
    __asm__ __volatile__ ("stdux        14,15,16");
}
#endif // #ifdef __powerpc64__

static test_t tests_ist_ops_three[] = {
    { &test_stbx            , "        stbx", },
    { &test_stbux           , "       stbux", },
    { &test_sthx            , "        sthx", },
    { &test_sthux           , "       sthux", },
    { &test_stwx            , "        stwx", },
    { &test_stwux           , "       stwux", },
#ifdef __powerpc64__
    { &test_stdx            , "        stdx", },
    { &test_stdux           , "       stdux", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};

static void
tests_popcnt_one(void)
{
   __asm__ __volatile__ ("popcntb      17, 14");
}

static test_t tests_popcnt_ops_one[] = {
    { &tests_popcnt_one            , "        popcntb", },
    { NULL,                   NULL,           },
};

#if !defined (NO_FLOAT)
static void test_fsel (void)
{
    __asm__ __volatile__ ("fsel         17, 14, 15, 16");
}

static void test_fmadd (void)
{
    __asm__ __volatile__ ("fmadd        17, 14, 15, 16");
}

static void test_fmadds (void)
{
    __asm__ __volatile__ ("fmadds       17, 14, 15, 16");
}

static void test_fmsub (void)
{
    __asm__ __volatile__ ("fmsub        17, 14, 15, 16");
}

static void test_fmsubs (void)
{
    __asm__ __volatile__ ("fmsubs       17, 14, 15, 16");
}

static void test_fnmadd (void)
{
    __asm__ __volatile__ ("fnmadd       17, 14, 15, 16");
}

static void test_fnmadds (void)
{
    __asm__ __volatile__ ("fnmadds      17, 14, 15, 16");
}

static void test_fnmsub (void)
{
    __asm__ __volatile__ ("fnmsub       17, 14, 15, 16");
}

static void test_fnmsubs (void)
{
    __asm__ __volatile__ ("fnmsubs      17, 14, 15, 16");
}

static test_t tests_fa_ops_three[] = {
    { &test_fsel            , "        fsel", },
    { &test_fmadd           , "       fmadd", },
    { &test_fmadds          , "      fmadds", },
    { &test_fmsub           , "       fmsub", },
    { &test_fmsubs          , "      fmsubs", },
    { &test_fnmadd          , "      fnmadd", },
    { &test_fnmadds         , "     fnmadds", },
    { &test_fnmsub          , "      fnmsub", },
    { &test_fnmsubs         , "     fnmsubs", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_fsel_ (void)
{
    __asm__ __volatile__ ("fsel.        17, 14, 15, 16");
}

static void test_fmadd_ (void)
{
    __asm__ __volatile__ ("fmadd.       17, 14, 15, 16");
}

static void test_fmadds_ (void)
{
    __asm__ __volatile__ ("fmadds.      17, 14, 15, 16");
}

static void test_fmsub_ (void)
{
    __asm__ __volatile__ ("fmsub.       17, 14, 15, 16");
}

static void test_fmsubs_ (void)
{
    __asm__ __volatile__ ("fmsubs.      17, 14, 15, 16");
}

static void test_fnmadd_ (void)
{
    __asm__ __volatile__ ("fnmadd.      17, 14, 15, 16");
}

static void test_fnmadds_ (void)
{
    __asm__ __volatile__ ("fnmadds.     17, 14, 15, 16");
}

static void test_fnmsub_ (void)
{
    __asm__ __volatile__ ("fnmsub.      17, 14, 15, 16");
}

static void test_fnmsubs_ (void)
{
    __asm__ __volatile__ ("fnmsubs.     17, 14, 15, 16");
}

static test_t tests_far_ops_three[] = {
    { &test_fsel_           , "       fsel.", },
    { &test_fmadd_          , "      fmadd.", },
    { &test_fmadds_         , "     fmadds.", },
    { &test_fmsub_          , "      fmsub.", },
    { &test_fmsubs_         , "     fmsubs.", },
    { &test_fnmadd_         , "     fnmadd.", },
    { &test_fnmadds_        , "    fnmadds.", },
    { &test_fnmsub_         , "     fnmsub.", },
    { &test_fnmsubs_        , "    fnmsubs.", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_fadd (void)
{
    __asm__ __volatile__ ("fadd         17, 14, 15");
}

static void test_fadds (void)
{
    __asm__ __volatile__ ("fadds        17, 14, 15");
}

static void test_fsub (void)
{
    __asm__ __volatile__ ("fsub         17, 14, 15");
}

static void test_fsubs (void)
{
    __asm__ __volatile__ ("fsubs        17, 14, 15");
}

static void test_fmul (void)
{
    __asm__ __volatile__ ("fmul         17, 14, 15");
}

static void test_fmuls (void)
{
    __asm__ __volatile__ ("fmuls        17, 14, 15");
}

static void test_fdiv (void)
{
    __asm__ __volatile__ ("fdiv         17, 14, 15");
}

static void test_fdivs (void)
{
    __asm__ __volatile__ ("fdivs        17, 14, 15");
}

static test_t tests_fa_ops_two[] = {
    { &test_fadd            , "        fadd", },
    { &test_fadds           , "       fadds", },
    { &test_fsub            , "        fsub", },
    { &test_fsubs           , "       fsubs", },
    { &test_fmul            , "        fmul", },
    { &test_fmuls           , "       fmuls", },
    { &test_fdiv            , "        fdiv", },
    { &test_fdivs           , "       fdivs", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_fadd_ (void)
{
    __asm__ __volatile__ ("fadd.        17, 14, 15");
}

static void test_fadds_ (void)
{
    __asm__ __volatile__ ("fadds.       17, 14, 15");
}

static void test_fsub_ (void)
{
    __asm__ __volatile__ ("fsub.        17, 14, 15");
}

static void test_fsubs_ (void)
{
    __asm__ __volatile__ ("fsubs.       17, 14, 15");
}

static void test_fmul_ (void)
{
    __asm__ __volatile__ ("fmul.        17, 14, 15");
}

static void test_fmuls_ (void)
{
    __asm__ __volatile__ ("fmuls.       17, 14, 15");
}

static void test_fdiv_ (void)
{
    __asm__ __volatile__ ("fdiv.        17, 14, 15");
}

static void test_fdivs_ (void)
{
    __asm__ __volatile__ ("fdivs.       17, 14, 15");
}

static test_t tests_far_ops_two[] = {
    { &test_fadd_           , "       fadd.", },
    { &test_fadds_          , "      fadds.", },
    { &test_fsub_           , "       fsub.", },
    { &test_fsubs_          , "      fsubs.", },
    { &test_fmul_           , "       fmul.", },
    { &test_fmuls_          , "      fmuls.", },
    { &test_fdiv_           , "       fdiv.", },
    { &test_fdivs_          , "      fdivs.", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_fcmpo (void)
{
    __asm__ __volatile__ ("fcmpo        2, 14, 15");
}

static void test_fcmpu (void)
{
    __asm__ __volatile__ ("fcmpu        2, 14, 15");
}

static test_t tests_fcr_ops_two[] = {
    { &test_fcmpo           , "       fcmpo", },
    { &test_fcmpu           , "       fcmpu", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)

static void test_fres (void)
{
    __asm__ __volatile__ ("fres         17, 14");
}

static void test_frsqrte (void)
{
    __asm__ __volatile__ ("frsqrte      17, 14");
}

static void test_frsp (void)
{
    __asm__ __volatile__ ("frsp         17, 14");
}

static void test_fctiw (void)
{
    __asm__ __volatile__ ("fctiw        17, 14");
}

static void test_fctiwz (void)
{
    __asm__ __volatile__ ("fctiwz       17, 14");
}

static void test_fmr (void)
{
    __asm__ __volatile__ ("fmr          17, 14");
}

static void test_fneg (void)
{
    __asm__ __volatile__ ("fneg         17, 14");
}

static void test_fabs (void)
{
    __asm__ __volatile__ ("fabs         17, 14");
}

static void test_fnabs (void)
{
    __asm__ __volatile__ ("fnabs        17, 14");
}

static void test_fsqrt (void)
{
    __asm__ __volatile__ ("fsqrt        17, 14");
}

#ifdef __powerpc64__
static void test_fcfid (void)
{
    __asm__ __volatile__ ("fcfid        17, 14");
}

static void test_fctid (void)
{
    __asm__ __volatile__ ("fctid        17, 14");
}

static void test_fctidz (void)
{
    __asm__ __volatile__ ("fctidz       17, 14");
}
#endif // #ifdef __powerpc64__

static test_t tests_fa_ops_one[] = {
    { &test_fres            , "        fres", },
    { &test_frsqrte         , "     frsqrte", },
    { &test_frsp            , "        frsp", },
    { &test_fctiw           , "       fctiw", },
    { &test_fctiwz          , "      fctiwz", },
    { &test_fmr             , "         fmr", },
    { &test_fneg            , "        fneg", },
    { &test_fabs            , "        fabs", },
    { &test_fnabs           , "       fnabs", },
    { &test_fsqrt           , "       fsqrt", },
#ifdef __powerpc64__
    { &test_fcfid           , "       fcfid", },
    { &test_fctid           , "       fctid", },
    { &test_fctidz          , "      fctidz", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)

static void test_fres_ (void)
{
    __asm__ __volatile__ ("fres.        17, 14");
}

static void test_frsqrte_ (void)
{
     __asm__ __volatile__ ("frsqrte.     17, 14");
}

static void test_frsp_ (void)
{
    __asm__ __volatile__ ("frsp.        17, 14");
}

static void test_fctiw_ (void)
{
    __asm__ __volatile__ ("fctiw.       17, 14");
}

static void test_fctiwz_ (void)
{
    __asm__ __volatile__ ("fctiwz.      17, 14");
}

static void test_fmr_ (void)
{
    __asm__ __volatile__ ("fmr.         17, 14");
}

static void test_fneg_ (void)
{
    __asm__ __volatile__ ("fneg.        17, 14");
}

static void test_fabs_ (void)
{
    __asm__ __volatile__ ("fabs.        17, 14");
}

static void test_fnabs_ (void)
{
    __asm__ __volatile__ ("fnabs.       17, 14");
}

#ifdef __powerpc64__
static void test_fcfid_ (void)
{
    __asm__ __volatile__ ("fcfid.       17, 14");
}

static void test_fctid_ (void)
{
    __asm__ __volatile__ ("fctid.       17, 14");
}

static void test_fctidz_ (void)
{
    __asm__ __volatile__ ("fctidz.      17, 14");
}
#endif // #ifdef __powerpc64__

static test_t tests_far_ops_one[] = {
    { &test_fres_           , "       fres.", },
    { &test_frsqrte_        , "    frsqrte.", },
    { &test_frsp_           , "       frsp.", },
    { &test_fctiw_          , "      fctiw.", },
    { &test_fctiwz_         , "     fctiwz.", },
    { &test_fmr_            , "        fmr.", },
    { &test_fneg_           , "       fneg.", },
    { &test_fabs_           , "       fabs.", },
    { &test_fnabs_          , "      fnabs.", },
#ifdef __powerpc64__
    { &test_fcfid_          , "      fcfid.", },
    { &test_fctid_          , "      fctid.", },
    { &test_fctidz_         , "     fctidz.", },
#endif // #ifdef __powerpc64__
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static test_t tests_fl_ops_spe[] = {
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static test_t tests_flr_ops_spe[] = {
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */


#if !defined (NO_FLOAT)
extern void test_lfs (void);
ASSEMBLY_FUNC("test_lfs", "lfs          17,0(14)");

extern void test_lfsu (void);
ASSEMBLY_FUNC("test_lfsu", "lfsu          17,0(14)");

extern void test_lfd (void);
ASSEMBLY_FUNC("test_lfd", "lfd          17,0(14)");

extern void test_lfdu (void);
ASSEMBLY_FUNC("test_lfdu", "lfdu          17,0(14)");

static test_t tests_fld_ops_two_i16[] = {
    { &test_lfs             , "         lfs", },
    { &test_lfsu            , "        lfsu", },
    { &test_lfd             , "         lfd", },
    { &test_lfdu            , "        lfdu", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_lfsx (void)
{
    __asm__ __volatile__ ("lfsx         17,14,15");
}

static void test_lfsux (void)
{
    __asm__ __volatile__ ("lfsux        17,14,15");
}

static void test_lfdx (void)
{
    __asm__ __volatile__ ("lfdx         17,14,15");
}

static void test_lfdux (void)
{
    __asm__ __volatile__ ("lfdux        17,14,15");
}

static test_t tests_fld_ops_two[] = {
    { &test_lfsx            , "        lfsx", },
    { &test_lfsux           , "       lfsux", },
    { &test_lfdx            , "        lfdx", },
    { &test_lfdux           , "       lfdux", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
extern void test_stfs (void);
ASSEMBLY_FUNC("test_stfs", "stfs          14,0(15)");

extern void test_stfsu (void);
ASSEMBLY_FUNC("test_stfsu", "stfsu          14,0(15)");

extern void test_stfd (void);
ASSEMBLY_FUNC("test_stfd", "stfd          14,0(15)");

extern void test_stfdu (void);
ASSEMBLY_FUNC("test_stfdu", "stfdu         14,0(15)");

static test_t tests_fst_ops_three_i16[] = {
    { &test_stfs             , "         stfs", },
    { &test_stfsu            , "        stfsu", },
    { &test_stfd             , "         stfd", },
    { &test_stfdu            , "        stfdu", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */

#if !defined (NO_FLOAT)
static void test_stfsx (void)
{
    __asm__ __volatile__ ("stfsx         14,15,16");
}

static void test_stfsux (void)
{
    __asm__ __volatile__ ("stfsux        14,15,16");
}

static void test_stfdx (void)
{
    __asm__ __volatile__ ("stfdx         14,15,16");
}

static void test_stfdux (void)
{
    __asm__ __volatile__ ("stfdux        14,15,16");
}

static test_t tests_fst_ops_three[] = {
    { &test_stfsx            , "        stfsx", },
    { &test_stfsux           , "       stfsux", },
    { &test_stfdx            , "        stfdx", },
    { &test_stfdux           , "       stfdux", },
    { NULL,                   NULL,           },
};
#endif /* !defined (NO_FLOAT) */


#if defined (HAS_ALTIVEC)
static void test_vmhaddshs (void)
{
    __asm__ __volatile__ ("vmhaddshs    17, 14, 15, 16");
}

static void test_vmhraddshs (void)
{
    __asm__ __volatile__ ("vmhraddshs   17, 14, 15, 16");
}

static void test_vmladduhm (void)
{
    __asm__ __volatile__ ("vmladduhm    17, 14, 15, 16");
}

static void test_vmsumubm (void)
{
    __asm__ __volatile__ ("vmsumubm     17, 14, 15, 16");
}

static void test_vmsumuhm (void)
{
    __asm__ __volatile__ ("vmsumuhm     17, 14, 15, 16");
}

static void test_vmsumshs (void)
{
    __asm__ __volatile__ ("vmsumshs     17, 14, 15, 16");
}

static void test_vmsumuhs (void)
{
    __asm__ __volatile__ ("vmsumuhs     17, 14, 15, 16");
}

static void test_vmsummbm (void)
{
    __asm__ __volatile__ ("vmsummbm     17, 14, 15, 16");
}

static void test_vmsumshm (void)
{
    __asm__ __volatile__ ("vmsumshm     17, 14, 15, 16");
}

static test_t tests_aa_ops_three[] = {
    { &test_vmhaddshs       , "   vmhaddshs", },
    { &test_vmhraddshs      , "  vmhraddshs", },
    { &test_vmladduhm       , "   vmladduhm", },
    { &test_vmsumubm        , "    vmsumubm", },
    { &test_vmsumuhm        , "    vmsumuhm", },
    { &test_vmsumshs        , "    vmsumshs", },
    { &test_vmsumuhs        , "    vmsumuhs", },
    { &test_vmsummbm        , "    vmsummbm", },
    { &test_vmsumshm        , "    vmsumshm", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vperm (void)
{
    __asm__ __volatile__ ("vperm        17, 14, 15, 16");
}

static void test_vsel (void)
{
    __asm__ __volatile__ ("vsel         17, 14, 15, 16");
}

static test_t tests_al_ops_three[] = {
    { &test_vperm           , "       vperm", },
    { &test_vsel            , "        vsel", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vaddubm (void)
{
    __asm__ __volatile__ ("vaddubm      17, 14, 15");
}

static void test_vadduhm (void)
{
    __asm__ __volatile__ ("vadduhm      17, 14, 15");
}

static void test_vadduwm (void)
{
    __asm__ __volatile__ ("vadduwm      17, 14, 15");
}

static void test_vaddubs (void)
{
    __asm__ __volatile__ ("vaddubs      17, 14, 15");
}

static void test_vadduhs (void)
{
    __asm__ __volatile__ ("vadduhs      17, 14, 15");
}

static void test_vadduws (void)
{
    __asm__ __volatile__ ("vadduws      17, 14, 15");
}

static void test_vaddsbs (void)
{
    __asm__ __volatile__ ("vaddsbs      17, 14, 15");
}

static void test_vaddshs (void)
{
    __asm__ __volatile__ ("vaddshs      17, 14, 15");
}

static void test_vaddsws (void)
{
    __asm__ __volatile__ ("vaddsws      17, 14, 15");
}

static void test_vaddcuw (void)
{
    __asm__ __volatile__ ("vaddcuw      17, 14, 15");
}

static void test_vsububm (void)
{
    __asm__ __volatile__ ("vsububm      17, 14, 15");
}

static void test_vsubuhm (void)
{
    __asm__ __volatile__ ("vsubuhm      17, 14, 15");
}

static void test_vsubuwm (void)
{
    __asm__ __volatile__ ("vsubuwm      17, 14, 15");
}

static void test_vsububs (void)
{
    __asm__ __volatile__ ("vsububs      17, 14, 15");
}

static void test_vsubuhs (void)
{
    __asm__ __volatile__ ("vsubuhs      17, 14, 15");
}

static void test_vsubuws (void)
{
    __asm__ __volatile__ ("vsubuws      17, 14, 15");
}

static void test_vsubsbs (void)
{
    __asm__ __volatile__ ("vsubsbs      17, 14, 15");
}

static void test_vsubshs (void)
{
    __asm__ __volatile__ ("vsubshs      17, 14, 15");
}

static void test_vsubsws (void)
{
    __asm__ __volatile__ ("vsubsws      17, 14, 15");
}

static void test_vsubcuw (void)
{
    __asm__ __volatile__ ("vsubcuw      17, 14, 15");
}

static void test_vmuloub (void)
{
    __asm__ __volatile__ ("vmuloub      17, 14, 15");
}

static void test_vmulouh (void)
{
    __asm__ __volatile__ ("vmulouh      17, 14, 15");
}

static void test_vmulosb (void)
{
    __asm__ __volatile__ ("vmulosb      17, 14, 15");
}

static void test_vmulosh (void)
{
    __asm__ __volatile__ ("vmulosh      17, 14, 15");
}

static void test_vmuleub (void)
{
    __asm__ __volatile__ ("vmuleub      17, 14, 15");
}

static void test_vmuleuh (void)
{
    __asm__ __volatile__ ("vmuleuh      17, 14, 15");
}

static void test_vmulesb (void)
{
    __asm__ __volatile__ ("vmulesb      17, 14, 15");
}

static void test_vmulesh (void)
{
    __asm__ __volatile__ ("vmulesh      17, 14, 15");
}

static void test_vsumsws (void)
{
    __asm__ __volatile__ ("vsumsws      17, 14, 15");
}

static void test_vsum2sws (void)
{
    __asm__ __volatile__ ("vsum2sws     17, 14, 15");
}

static void test_vsum4ubs (void)
{
    __asm__ __volatile__ ("vsum4ubs     17, 14, 15");
}

static void test_vsum4sbs (void)
{
    __asm__ __volatile__ ("vsum4sbs     17, 14, 15");
}

static void test_vsum4shs (void)
{
    __asm__ __volatile__ ("vsum4shs     17, 14, 15");
}

static void test_vavgub (void)
{
    __asm__ __volatile__ ("vavgub       17, 14, 15");
}

static void test_vavguh (void)
{
    __asm__ __volatile__ ("vavguh       17, 14, 15");
}

static void test_vavguw (void)
{
    __asm__ __volatile__ ("vavguw       17, 14, 15");
}

static void test_vavgsb (void)
{
    __asm__ __volatile__ ("vavgsb       17, 14, 15");
}

static void test_vavgsh (void)
{
    __asm__ __volatile__ ("vavgsh       17, 14, 15");
}

static void test_vavgsw (void)
{
    __asm__ __volatile__ ("vavgsw       17, 14, 15");
}

static void test_vmaxub (void)
{
    __asm__ __volatile__ ("vmaxub       17, 14, 15");
}

static void test_vmaxuh (void)
{
    __asm__ __volatile__ ("vmaxuh       17, 14, 15");
}

static void test_vmaxuw (void)
{
    __asm__ __volatile__ ("vmaxuw       17, 14, 15");
}

static void test_vmaxsb (void)
{
    __asm__ __volatile__ ("vmaxsb       17, 14, 15");
}

static void test_vmaxsh (void)
{
    __asm__ __volatile__ ("vmaxsh       17, 14, 15");
}

static void test_vmaxsw (void)
{
    __asm__ __volatile__ ("vmaxsw       17, 14, 15");
}

static void test_vminub (void)
{
    __asm__ __volatile__ ("vminub       17, 14, 15");
}

static void test_vminuh (void)
{
    __asm__ __volatile__ ("vminuh       17, 14, 15");
}

static void test_vminuw (void)
{
    __asm__ __volatile__ ("vminuw       17, 14, 15");
}

static void test_vminsb (void)
{
    __asm__ __volatile__ ("vminsb       17, 14, 15");
}

static void test_vminsh (void)
{
    __asm__ __volatile__ ("vminsh       17, 14, 15");
}

static void test_vminsw (void)
{
    __asm__ __volatile__ ("vminsw       17, 14, 15");
}

static test_t tests_aa_ops_two[] = {
    { &test_vaddubm         , "     vaddubm", },
    { &test_vadduhm         , "     vadduhm", },
    { &test_vadduwm         , "     vadduwm", },
    { &test_vaddubs         , "     vaddubs", },
    { &test_vadduhs         , "     vadduhs", },
    { &test_vadduws         , "     vadduws", },
    { &test_vaddsbs         , "     vaddsbs", },
    { &test_vaddshs         , "     vaddshs", },
    { &test_vaddsws         , "     vaddsws", },
    { &test_vaddcuw         , "     vaddcuw", },
    { &test_vsububm         , "     vsububm", },
    { &test_vsubuhm         , "     vsubuhm", },
    { &test_vsubuwm         , "     vsubuwm", },
    { &test_vsububs         , "     vsububs", },
    { &test_vsubuhs         , "     vsubuhs", },
    { &test_vsubuws         , "     vsubuws", },
    { &test_vsubsbs         , "     vsubsbs", },
    { &test_vsubshs         , "     vsubshs", },
    { &test_vsubsws         , "     vsubsws", },
    { &test_vsubcuw         , "     vsubcuw", },
    { &test_vmuloub         , "     vmuloub", },
    { &test_vmulouh         , "     vmulouh", },
    { &test_vmulosb         , "     vmulosb", },
    { &test_vmulosh         , "     vmulosh", },
    { &test_vmuleub         , "     vmuleub", },
    { &test_vmuleuh         , "     vmuleuh", },
    { &test_vmulesb         , "     vmulesb", },
    { &test_vmulesh         , "     vmulesh", },
    { &test_vsumsws         , "     vsumsws", },
    { &test_vsum2sws        , "    vsum2sws", },
    { &test_vsum4ubs        , "    vsum4ubs", },
    { &test_vsum4sbs        , "    vsum4sbs", },
    { &test_vsum4shs        , "    vsum4shs", },
    { &test_vavgub          , "      vavgub", },
    { &test_vavguh          , "      vavguh", },
    { &test_vavguw          , "      vavguw", },
    { &test_vavgsb          , "      vavgsb", },
    { &test_vavgsh          , "      vavgsh", },
    { &test_vavgsw          , "      vavgsw", },
    { &test_vmaxub          , "      vmaxub", },
    { &test_vmaxuh          , "      vmaxuh", },
    { &test_vmaxuw          , "      vmaxuw", },
    { &test_vmaxsb          , "      vmaxsb", },
    { &test_vmaxsh          , "      vmaxsh", },
    { &test_vmaxsw          , "      vmaxsw", },
    { &test_vminub          , "      vminub", },
    { &test_vminuh          , "      vminuh", },
    { &test_vminuw          , "      vminuw", },
    { &test_vminsb          , "      vminsb", },
    { &test_vminsh          , "      vminsh", },
    { &test_vminsw          , "      vminsw", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vand (void)
{
    __asm__ __volatile__ ("vand         17, 14, 15");
}

static void test_vor (void)
{
    __asm__ __volatile__ ("vor          17, 14, 15");
}

static void test_vxor (void)
{
    __asm__ __volatile__ ("vxor         17, 14, 15");
}

static void test_vandc (void)
{
    __asm__ __volatile__ ("vandc        17, 14, 15");
}

static void test_vnor (void)
{
    __asm__ __volatile__ ("vnor         17, 14, 15");
}

static void test_vrlb (void)
{
    __asm__ __volatile__ ("vrlb         17, 14, 15");
}

static void test_vrlh (void)
{
    __asm__ __volatile__ ("vrlh         17, 14, 15");
}

static void test_vrlw (void)
{
    __asm__ __volatile__ ("vrlw         17, 14, 15");
}

static void test_vslb (void)
{
    __asm__ __volatile__ ("vslb         17, 14, 15");
}

static void test_vslh (void)
{
    __asm__ __volatile__ ("vslh         17, 14, 15");
}

static void test_vslw (void)
{
    __asm__ __volatile__ ("vslw         17, 14, 15");
}

static void test_vsrb (void)
{
    __asm__ __volatile__ ("vsrb         17, 14, 15");
}

static void test_vsrh (void)
{
    __asm__ __volatile__ ("vsrh         17, 14, 15");
}

static void test_vsrw (void)
{
    __asm__ __volatile__ ("vsrw         17, 14, 15");
}

static void test_vsrab (void)
{
    __asm__ __volatile__ ("vsrab        17, 14, 15");
}

static void test_vsrah (void)
{
    __asm__ __volatile__ ("vsrah        17, 14, 15");
}

static void test_vsraw (void)
{
    __asm__ __volatile__ ("vsraw        17, 14, 15");
}

static void test_vpkuhum (void)
{
    __asm__ __volatile__ ("vpkuhum      17, 14, 15");
}

static void test_vpkuwum (void)
{
    __asm__ __volatile__ ("vpkuwum      17, 14, 15");
}

static void test_vpkuhus (void)
{
    __asm__ __volatile__ ("vpkuhus      17, 14, 15");
}

static void test_vpkuwus (void)
{
    __asm__ __volatile__ ("vpkuwus      17, 14, 15");
}

static void test_vpkshus (void)
{
    __asm__ __volatile__ ("vpkshus      17, 14, 15");
}

static void test_vpkswus (void)
{
    __asm__ __volatile__ ("vpkswus      17, 14, 15");
}

static void test_vpkshss (void)
{
    __asm__ __volatile__ ("vpkshss      17, 14, 15");
}

static void test_vpkswss (void)
{
    __asm__ __volatile__ ("vpkswss      17, 14, 15");
}

static void test_vpkpx (void)
{
    __asm__ __volatile__ ("vpkpx        17, 14, 15");
}

static void test_vmrghb (void)
{
    __asm__ __volatile__ ("vmrghb       17, 14, 15");
}

static void test_vmrghh (void)
{
    __asm__ __volatile__ ("vmrghh       17, 14, 15");
}

static void test_vmrghw (void)
{
    __asm__ __volatile__ ("vmrghw       17, 14, 15");
}

static void test_vmrglb (void)
{
    __asm__ __volatile__ ("vmrglb       17, 14, 15");
}

static void test_vmrglh (void)
{
    __asm__ __volatile__ ("vmrglh       17, 14, 15");
}

static void test_vmrglw (void)
{
    __asm__ __volatile__ ("vmrglw       17, 14, 15");
}

static void test_vslo (void)
{
    __asm__ __volatile__ ("vslo         17, 14, 15");
}

static void test_vsro (void)
{
    __asm__ __volatile__ ("vsro         17, 14, 15");
}

static test_t tests_al_ops_two[] = {
    { &test_vand            , "        vand", },
    { &test_vor             , "         vor", },
    { &test_vxor            , "        vxor", },
    { &test_vandc           , "       vandc", },
    { &test_vnor            , "        vnor", },
    { &test_vrlb            , "        vrlb", },
    { &test_vrlh            , "        vrlh", },
    { &test_vrlw            , "        vrlw", },
    { &test_vslb            , "        vslb", },
    { &test_vslh            , "        vslh", },
    { &test_vslw            , "        vslw", },
    { &test_vsrb            , "        vsrb", },
    { &test_vsrh            , "        vsrh", },
    { &test_vsrw            , "        vsrw", },
    { &test_vsrab           , "       vsrab", },
    { &test_vsrah           , "       vsrah", },
    { &test_vsraw           , "       vsraw", },
    { &test_vpkuhum         , "     vpkuhum", },
    { &test_vpkuwum         , "     vpkuwum", },
    { &test_vpkuhus         , "     vpkuhus", },
    { &test_vpkuwus         , "     vpkuwus", },
    { &test_vpkshus         , "     vpkshus", },
    { &test_vpkswus         , "     vpkswus", },
    { &test_vpkshss         , "     vpkshss", },
    { &test_vpkswss         , "     vpkswss", },
    { &test_vpkpx           , "       vpkpx", },
    { &test_vmrghb          , "      vmrghb", },
    { &test_vmrghh          , "      vmrghh", },
    { &test_vmrghw          , "      vmrghw", },
    { &test_vmrglb          , "      vmrglb", },
    { &test_vmrglh          , "      vmrglh", },
    { &test_vmrglw          , "      vmrglw", },
    { &test_vslo            , "        vslo", },
    { &test_vsro            , "        vsro", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vupkhsb (void)
{
    __asm__ __volatile__ ("vupkhsb      17, 14");
}

static void test_vupkhsh (void)
{
    __asm__ __volatile__ ("vupkhsh      17, 14");
}

static void test_vupkhpx (void)
{
    __asm__ __volatile__ ("vupkhpx      17, 14");
}

static void test_vupklsb (void)
{
    __asm__ __volatile__ ("vupklsb      17, 14");
}

static void test_vupklsh (void)
{
    __asm__ __volatile__ ("vupklsh      17, 14");
}

static void test_vupklpx (void)
{
    __asm__ __volatile__ ("vupklpx      17, 14");
}

static test_t tests_al_ops_one[] = {
    { &test_vupkhsb         , "     vupkhsb", },
    { &test_vupkhsh         , "     vupkhsh", },
    { &test_vupkhpx         , "     vupkhpx", },
    { &test_vupklsb         , "     vupklsb", },
    { &test_vupklsh         , "     vupklsh", },
    { &test_vupklpx         , "     vupklpx", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vcmpgtub (void)
{
    __asm__ __volatile__ ("vcmpgtub     17, 14, 15");
}

static void test_vcmpgtuh (void)
{
    __asm__ __volatile__ ("vcmpgtuh     17, 14, 15");
}

static void test_vcmpgtuw (void)
{
    __asm__ __volatile__ ("vcmpgtuw     17, 14, 15");
}

static void test_vcmpgtsb (void)
{
    __asm__ __volatile__ ("vcmpgtsb     17, 14, 15");
}

static void test_vcmpgtsh (void)
{
    __asm__ __volatile__ ("vcmpgtsh     17, 14, 15");
}

static void test_vcmpgtsw (void)
{
    __asm__ __volatile__ ("vcmpgtsw     17, 14, 15");
}

static void test_vcmpequb (void)
{
    __asm__ __volatile__ ("vcmpequb     17, 14, 15");
}

static void test_vcmpequh (void)
{
    __asm__ __volatile__ ("vcmpequh     17, 14, 15");
}

static void test_vcmpequw (void)
{
    __asm__ __volatile__ ("vcmpequw     17, 14, 15");
}

static test_t tests_ac_ops_two[] = {
    { &test_vcmpgtub        , "    vcmpgtub", },
    { &test_vcmpgtuh        , "    vcmpgtuh", },
    { &test_vcmpgtuw        , "    vcmpgtuw", },
    { &test_vcmpgtsb        , "    vcmpgtsb", },
    { &test_vcmpgtsh        , "    vcmpgtsh", },
    { &test_vcmpgtsw        , "    vcmpgtsw", },
    { &test_vcmpequb        , "    vcmpequb", },
    { &test_vcmpequh        , "    vcmpequh", },
    { &test_vcmpequw        , "    vcmpequw", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vcmpgtub_ (void)
{
    __asm__ __volatile__ ("vcmpgtub.    17, 14, 15");
}

static void test_vcmpgtuh_ (void)
{
    __asm__ __volatile__ ("vcmpgtuh.    17, 14, 15");
}

static void test_vcmpgtuw_ (void)
{
    __asm__ __volatile__ ("vcmpgtuw.    17, 14, 15");
}

static void test_vcmpgtsb_ (void)
{
    __asm__ __volatile__ ("vcmpgtsb.    17, 14, 15");
}

static void test_vcmpgtsh_ (void)
{
    __asm__ __volatile__ ("vcmpgtsh.    17, 14, 15");
}

static void test_vcmpgtsw_ (void)
{
    __asm__ __volatile__ ("vcmpgtsw.    17, 14, 15");
}

static void test_vcmpequb_ (void)
{
    __asm__ __volatile__ ("vcmpequb.    17, 14, 15");
}

static void test_vcmpequh_ (void)
{
    __asm__ __volatile__ ("vcmpequh.    17, 14, 15");
}

static void test_vcmpequw_ (void)
{
    __asm__ __volatile__ ("vcmpequw.    17, 14, 15");
}

static test_t tests_acr_ops_two[] = {
    { &test_vcmpgtub_       , "   vcmpgtub.", },
    { &test_vcmpgtuh_       , "   vcmpgtuh.", },
    { &test_vcmpgtuw_       , "   vcmpgtuw.", },
    { &test_vcmpgtsb_       , "   vcmpgtsb.", },
    { &test_vcmpgtsh_       , "   vcmpgtsh.", },
    { &test_vcmpgtsw_       , "   vcmpgtsw.", },
    { &test_vcmpequb_       , "   vcmpequb.", },
    { &test_vcmpequh_       , "   vcmpequh.", },
    { &test_vcmpequw_       , "   vcmpequw.", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vsl (void)
{
    __asm__ __volatile__ ("vsl          17, 14, 15");
}

static void test_vsr (void)
{
    __asm__ __volatile__ ("vsr          17, 14, 15");
}

extern void test_vspltb (void);
ASSEMBLY_FUNC("test_vspltb", "vspltb       17, 14, 0");

extern void test_vsplth (void);
ASSEMBLY_FUNC("test_vsplth", "vsplth       17, 14, 0");

extern void test_vspltw (void);
ASSEMBLY_FUNC("test_vspltw", "vspltw       17, 14, 0");

extern void test_vspltisb (void);
ASSEMBLY_FUNC("test_vspltisb", "vspltisb       17, 0");

extern void test_vspltish (void);
ASSEMBLY_FUNC("test_vspltish", "vspltish       17, 0");

extern void test_vspltisw (void);
ASSEMBLY_FUNC("test_vspltisw", "vspltisw       17, 0");

extern void test_vsldoi (void);
ASSEMBLY_FUNC("test_vsldoi", "vsldoi       17, 14, 15, 0");

static void test_lvsl (void)
{
    __asm__ __volatile__ ("lvsl         17, 14, 15");
}

static void test_lvsr (void)
{
    __asm__ __volatile__ ("lvsr         17, 14, 15");
}

static test_t tests_av_int_ops_spe[] = {
    { &test_vsl             , "         vsl", },
    { &test_vsr             , "         vsr", },
    { &test_vspltb          , "      vspltb", },
    { &test_vsplth          , "      vsplth", },
    { &test_vspltw          , "      vspltw", },
    { &test_vspltisb        , "    vspltisb", },
    { &test_vspltish        , "    vspltish", },
    { &test_vspltisw        , "    vspltisw", },
    { &test_vsldoi          , "      vsldoi", },
    { &test_lvsl            , "        lvsl", },
    { &test_lvsr            , "        lvsr", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_lvebx (void)
{
    __asm__ __volatile__ ("lvebx        17,14,15");
}

static void test_lvehx (void)
{
    __asm__ __volatile__ ("lvehx        17,14,15");
}

static void test_lvewx (void)
{
    __asm__ __volatile__ ("lvewx        17,14,15");
}

static void test_lvx (void)
{
    __asm__ __volatile__ ("lvx          17,14,15");
}

static void test_lvxl (void)
{
    __asm__ __volatile__ ("lvxl         17,14,15");
}

static test_t tests_ald_ops_two[] = {
    { &test_lvebx           , "       lvebx", },
    { &test_lvehx           , "       lvehx", },
    { &test_lvewx           , "       lvewx", },
    { &test_lvx             , "         lvx", },
    { &test_lvxl            , "        lvxl", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_stvebx (void)
{
    __asm__ __volatile__ ("stvebx       14,15,16");
}

static void test_stvehx (void)
{
    __asm__ __volatile__ ("stvehx       14,15,16");
}

static void test_stvewx (void)
{
    __asm__ __volatile__ ("stvewx       14,15,16");
}

static void test_stvx (void)
{
    __asm__ __volatile__ ("stvx         14,15,16");
}

static void test_stvxl (void)
{
    __asm__ __volatile__ ("stvxl        14,15,16");
}

static test_t tests_ast_ops_three[] = {
    { &test_stvebx          , "      stvebx", },
    { &test_stvehx          , "      stvehx", },
    { &test_stvewx          , "      stvewx", },
    { &test_stvx            , "        stvx", },
    { &test_stvxl           , "       stvxl", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
#if 1
static void test_vmaddfp (void)
{
    __asm__ __volatile__ ("vmaddfp      17, 14, 15, 16");
}

static void test_vnmsubfp (void)
{
    __asm__ __volatile__ ("vnmsubfp     17, 14, 15, 16");
}
#endif

static test_t tests_afa_ops_three[] = {
    { &test_vmaddfp         , "     vmaddfp", },
    { &test_vnmsubfp        , "    vnmsubfp", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vaddfp (void)
{
    __asm__ __volatile__ ("vaddfp       17, 14, 15");
}

static void test_vsubfp (void)
{
    __asm__ __volatile__ ("vsubfp       17, 14, 15");
}

static void test_vmaxfp (void)
{
    __asm__ __volatile__ ("vmaxfp       17, 14, 15");
}

static void test_vminfp (void)
{
    __asm__ __volatile__ ("vminfp       17, 14, 15");
}

static test_t tests_afa_ops_two[] = {
    { &test_vaddfp          , "      vaddfp", },
    { &test_vsubfp          , "      vsubfp", },
    { &test_vmaxfp          , "      vmaxfp", },
    { &test_vminfp          , "      vminfp", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vrfin (void)
{
    __asm__ __volatile__ ("vrfin        17, 14");
}

static void test_vrfiz (void)
{
    __asm__ __volatile__ ("vrfiz        17, 14");
}

static void test_vrfip (void)
{
    __asm__ __volatile__ ("vrfip        17, 14");
}

static void test_vrfim (void)
{
    __asm__ __volatile__ ("vrfim        17, 14");
}

static void test_vrefp (void)
{
    __asm__ __volatile__ ("vrefp        17, 14");
}

static void test_vrsqrtefp (void)
{
    __asm__ __volatile__ ("vrsqrtefp    17, 14");
}

static void test_vlogefp (void)
{
    __asm__ __volatile__ ("vlogefp      17, 14");
}

static void test_vexptefp (void)
{
    __asm__ __volatile__ ("vexptefp     17, 14");
}

static test_t tests_afa_ops_one[] = {
    { &test_vrfin           , "       vrfin", },
    { &test_vrfiz           , "       vrfiz", },
    { &test_vrfip           , "       vrfip", },
    { &test_vrfim           , "       vrfim", },
    { &test_vrefp           , "       vrefp", },
    { &test_vrsqrtefp       , "   vrsqrtefp", },
    { &test_vlogefp         , "     vlogefp", },
    { &test_vexptefp        , "    vexptefp", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vcmpgtfp (void)
{
    __asm__ __volatile__ ("vcmpgtfp     17, 14, 15");
}

static void test_vcmpeqfp (void)
{
    __asm__ __volatile__ ("vcmpeqfp     17, 14, 15");
}

static void test_vcmpgefp (void)
{
    __asm__ __volatile__ ("vcmpgefp     17, 14, 15");
}

static void test_vcmpbfp (void)
{
    __asm__ __volatile__ ("vcmpbfp      17, 14, 15");
}

static test_t tests_afc_ops_two[] = {
    { &test_vcmpgtfp        , "    vcmpgtfp", },
    { &test_vcmpeqfp        , "    vcmpeqfp", },
    { &test_vcmpgefp        , "    vcmpgefp", },
    { &test_vcmpbfp         , "     vcmpbfp", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
static void test_vcmpgtfp_ (void)
{
    __asm__ __volatile__ ("vcmpgtfp.    17, 14, 15");
}

static void test_vcmpeqfp_ (void)
{
    __asm__ __volatile__ ("vcmpeqfp.    17, 14, 15");
}

static void test_vcmpgefp_ (void)
{
    __asm__ __volatile__ ("vcmpgefp.    17, 14, 15");
}

static void test_vcmpbfp_ (void)
{
    __asm__ __volatile__ ("vcmpbfp.     17, 14, 15");
}

static test_t tests_afcr_ops_two[] = {
    { &test_vcmpgtfp_       , "   vcmpgtfp.", },
    { &test_vcmpeqfp_       , "   vcmpeqfp.", },
    { &test_vcmpgefp_       , "   vcmpgefp.", },
    { &test_vcmpbfp_        , "    vcmpbfp.", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

#if defined (HAS_ALTIVEC)
extern void test_vcfux (void);
ASSEMBLY_FUNC("test_vcfux", "vcfux        17, 14, 0");

extern void test_vcfsx (void);
ASSEMBLY_FUNC("test_vcfsx", "vcfsx        17, 14, 0");

extern void test_vctuxs (void);
ASSEMBLY_FUNC("test_vctuxs", "vctuxs        17, 14, 0");

extern void test_vctsxs (void);
ASSEMBLY_FUNC("test_vctsxs", "vctsxs        17, 14, 0");

static test_t tests_av_float_ops_spe[] = {
    { &test_vcfux           , "       vcfux", },
    { &test_vcfsx           , "       vcfsx", },
    { &test_vctuxs          , "      vctuxs", },
    { &test_vctsxs          , "      vctsxs", },
    { NULL,                   NULL,           },
};
#endif /* defined (HAS_ALTIVEC) */

/* Power ISA 2.03 support dcbtct and dcbtstct with valid hint values b00000 - 0b00111.
 * The ISA 2.06 added support for more valid hint values, but rather than tie ourselves
 * in knots trying to test all permutations of ISAs and valid hint values, we'll just
 * verify some of the base hint values from ISA 2.03.
 *
 * In a similar vein, in ISA 2.03, dcbtds had valid values of 0b01000 - 0b01010, whereas
 * ISA 2.06 expanded the range of valid hint values to 0b01000 - 0b01111.  We just test
 * one of the ISA 2.03-supported values for dcbtds.
 */
static void test_dcbtct (void)
{
   /*  dcbt RA, RB, TH */
   ASM_DCBT(17, 14, 1);
   ASM_DCBT(17, 14, 7);
}

static void test_dcbtds (void)
{
   /*  dcbt RA, RB, TH */
   ASM_DCBT(17, 14, 10);
   ASM_DCBT(17, 14, 0);
   ASM_DCBT(17, 14, 16);
}

static void test_dcbtst (void)
{
   /*  dcbtst RA, RB, TH */
   ASM_DCBTST(17, 14, 6);
   ASM_DCBTST(17, 14, 15);
}


static test_t tests_dcbt[] = {
    { &test_dcbtct       , "   dcbtct", },
    { &test_dcbtds       , "   dcbtds", },
    { &test_dcbtst       , "   dcbtst", },
    { NULL,                   NULL,           },
};


#if defined (IS_PPC405)
static void test_macchw (void)
{
    __asm__ __volatile__ ("macchw       17, 14, 15");
}

static void test_macchwo (void)
{
    __asm__ __volatile__ ("macchwo      17, 14, 15");
}

static void test_macchws (void)
{
    __asm__ __volatile__ ("macchws      17, 14, 15");
}

static void test_macchwso (void)
{
    __asm__ __volatile__ ("macchwso     17, 14, 15");
}

static void test_macchwsu (void)
{
    __asm__ __volatile__ ("macchwsu     17, 14, 15");
}

static void test_macchwsuo (void)
{
    __asm__ __volatile__ ("macchwsuo    17, 14, 15");
}

static void test_macchwu (void)
{
    __asm__ __volatile__ ("macchwu      17, 14, 15");
}

static void test_macchwuo (void)
{
    __asm__ __volatile__ ("macchwuo     17, 14, 15");
}

static void test_machhw (void)
{
    __asm__ __volatile__ ("machhw       17, 14, 15");
}

static void test_machhwo (void)
{
    __asm__ __volatile__ ("machhwo      17, 14, 15");
}

static void test_machhws (void)
{
    __asm__ __volatile__ ("machhws      17, 14, 15");
}

static void test_machhwso (void)
{
    __asm__ __volatile__ ("machhwso     17, 14, 15");
}

static void test_machhwsu (void)
{
    __asm__ __volatile__ ("machhwsu     17, 14, 15");
}

static void test_machhwsuo (void)
{
    __asm__ __volatile__ ("machhwsuo    17, 14, 15");
}

static void test_machhwu (void)
{
    __asm__ __volatile__ ("machhwu      17, 14, 15");
}

static void test_machhwuo (void)
{
    __asm__ __volatile__ ("machhwuo     17, 14, 15");
}

static void test_maclhw (void)
{
    __asm__ __volatile__ ("maclhw       17, 14, 15");
}

static void test_maclhwo (void)
{
    __asm__ __volatile__ ("maclhwo      17, 14, 15");
}

static void test_maclhws (void)
{
    __asm__ __volatile__ ("maclhws      17, 14, 15");
}

static void test_maclhwso (void)
{
    __asm__ __volatile__ ("maclhwso     17, 14, 15");
}

static void test_maclhwsu (void)
{
    __asm__ __volatile__ ("maclhwsu     17, 14, 15");
}

static void test_maclhwsuo (void)
{
    __asm__ __volatile__ ("maclhwsuo    17, 14, 15");
}

static void test_maclhwu (void)
{
    __asm__ __volatile__ ("maclhwu      17, 14, 15");
}

static void test_maclhwuo (void)
{
    __asm__ __volatile__ ("maclhwuo     17, 14, 15");
}

static void test_mulchw (void)
{
    __asm__ __volatile__ ("mulchw       17, 14, 15");
}

static void test_mulchwu (void)
{
    __asm__ __volatile__ ("mulchwu      17, 14, 15");
}

static void test_mulhhw (void)
{
    __asm__ __volatile__ ("mulhhw       17, 14, 15");
}

static void test_mulhhwu (void)
{
    __asm__ __volatile__ ("mulhhwu      17, 14, 15");
}

static void test_mullhw (void)
{
    __asm__ __volatile__ ("mullhw       17, 14, 15");
}

static void test_mullhwu (void)
{
    __asm__ __volatile__ ("mullhwu      17, 14, 15");
}

static void test_nmacchw (void)
{
    __asm__ __volatile__ ("nmacchw      17, 14, 15");
}

static void test_nmacchwo (void)
{
    __asm__ __volatile__ ("nmacchwo     17, 14, 15");
}

static void test_nmacchws (void)
{
    __asm__ __volatile__ ("nmacchws     17, 14, 15");
}

static void test_nmacchwso (void)
{
    __asm__ __volatile__ ("nmacchwso    17, 14, 15");
}

static void test_nmachhw (void)
{
    __asm__ __volatile__ ("nmachhw      17, 14, 15");
}

static void test_nmachhwo (void)
{
    __asm__ __volatile__ ("nmachhwo     17, 14, 15");
}

static void test_nmachhws (void)
{
    __asm__ __volatile__ ("nmachhws     17, 14, 15");
}

static void test_nmachhwso (void)
{
    __asm__ __volatile__ ("nmachhwso    17, 14, 15");
}

static void test_nmaclhw (void)
{
    __asm__ __volatile__ ("nmaclhw      17, 14, 15");
}

static void test_nmaclhwo (void)
{
    __asm__ __volatile__ ("nmaclhwo     17, 14, 15");
}

static void test_nmaclhws (void)
{
    __asm__ __volatile__ ("nmaclhws     17, 14, 15");
}

static void test_nmaclhwso (void)
{
    __asm__ __volatile__ ("nmaclhwso    17, 14, 15");
}

static test_t tests_p4m_ops_two[] = {
    { &test_macchw          , "      macchw", },
    { &test_macchwo         , "     macchwo", },
    { &test_macchws         , "     macchws", },
    { &test_macchwso        , "    macchwso", },
    { &test_macchwsu        , "    macchwsu", },
    { &test_macchwsuo       , "   macchwsuo", },
    { &test_macchwu         , "     macchwu", },
    { &test_macchwuo        , "    macchwuo", },
    { &test_machhw          , "      machhw", },
    { &test_machhwo         , "     machhwo", },
    { &test_machhws         , "     machhws", },
    { &test_machhwso        , "    machhwso", },
    { &test_machhwsu        , "    machhwsu", },
    { &test_machhwsuo       , "   machhwsuo", },
    { &test_machhwu         , "     machhwu", },
    { &test_machhwuo        , "    machhwuo", },
    { &test_maclhw          , "      maclhw", },
    { &test_maclhwo         , "     maclhwo", },
    { &test_maclhws         , "     maclhws", },
    { &test_maclhwso        , "    maclhwso", },
    { &test_maclhwsu        , "    maclhwsu", },
    { &test_maclhwsuo       , "   maclhwsuo", },
    { &test_maclhwu         , "     maclhwu", },
    { &test_maclhwuo        , "    maclhwuo", },
    { &test_mulchw          , "      mulchw", },
    { &test_mulchwu         , "     mulchwu", },
    { &test_mulhhw          , "      mulhhw", },
    { &test_mulhhwu         , "     mulhhwu", },
    { &test_mullhw          , "      mullhw", },
    { &test_mullhwu         , "     mullhwu", },
    { &test_nmacchw         , "     nmacchw", },
    { &test_nmacchwo        , "    nmacchwo", },
    { &test_nmacchws        , "    nmacchws", },
    { &test_nmacchwso       , "   nmacchwso", },
    { &test_nmachhw         , "     nmachhw", },
    { &test_nmachhwo        , "    nmachhwo", },
    { &test_nmachhws        , "    nmachhws", },
    { &test_nmachhwso       , "   nmachhwso", },
    { &test_nmaclhw         , "     nmaclhw", },
    { &test_nmaclhwo        , "    nmaclhwo", },
    { &test_nmaclhws        , "    nmaclhws", },
    { &test_nmaclhwso       , "   nmaclhwso", },
    { NULL,                   NULL,           },
};
#endif /* defined (IS_PPC405) */

#if defined (IS_PPC405)
static void test_macchw_ (void)
{
    __asm__ __volatile__ ("macchw.      17, 14, 15");
}

static void test_macchwo_ (void)
{
    __asm__ __volatile__ ("macchwo.     17, 14, 15");
}

static void test_macchws_ (void)
{
    __asm__ __volatile__ ("macchws.     17, 14, 15");
}

static void test_macchwso_ (void)
{
    __asm__ __volatile__ ("macchwso.    17, 14, 15");
}

static void test_macchwsu_ (void)
{
    __asm__ __volatile__ ("macchwsu.    17, 14, 15");
}

static void test_macchwsuo_ (void)
{
    __asm__ __volatile__ ("macchwsuo.   17, 14, 15");
}

static void test_macchwu_ (void)
{
    __asm__ __volatile__ ("macchwu.     17, 14, 15");
}

static void test_macchwuo_ (void)
{
    __asm__ __volatile__ ("macchwuo.    17, 14, 15");
}

static void test_machhw_ (void)
{
    __asm__ __volatile__ ("machhw.      17, 14, 15");
}

static void test_machhwo_ (void)
{
    __asm__ __volatile__ ("machhwo.     17, 14, 15");
}

static void test_machhws_ (void)
{
    __asm__ __volatile__ ("machhws.     17, 14, 15");
}

static void test_machhwso_ (void)
{
    __asm__ __volatile__ ("machhwso.    17, 14, 15");
}

static void test_machhwsu_ (void)
{
    __asm__ __volatile__ ("machhwsu.    17, 14, 15");
}

static void test_machhwsuo_ (void)
{
    __asm__ __volatile__ ("machhwsuo.   17, 14, 15");
}

static void test_machhwu_ (void)
{
    __asm__ __volatile__ ("machhwu.     17, 14, 15");
}

static void test_machhwuo_ (void)
{
    __asm__ __volatile__ ("machhwuo.    17, 14, 15");
}

static void test_maclhw_ (void)
{
    __asm__ __volatile__ ("maclhw.      17, 14, 15");
}

static void test_maclhwo_ (void)
{
    __asm__ __volatile__ ("maclhwo.     17, 14, 15");
}

static void test_maclhws_ (void)
{
    __asm__ __volatile__ ("maclhws.     17, 14, 15");
}

static void test_maclhwso_ (void)
{
    __asm__ __volatile__ ("maclhwso.    17, 14, 15");
}

static void test_maclhwsu_ (void)
{
    __asm__ __volatile__ ("maclhwsu.    17, 14, 15");
}

static void test_maclhwsuo_ (void)
{
    __asm__ __volatile__ ("maclhwsuo.   17, 14, 15");
}

static void test_maclhwu_ (void)
{
    __asm__ __volatile__ ("maclhwu.     17, 14, 15");
}

static void test_maclhwuo_ (void)
{
    __asm__ __volatile__ ("maclhwuo.    17, 14, 15");
}

static void test_mulchw_ (void)
{
    __asm__ __volatile__ ("mulchw.      17, 14, 15");
}

static void test_mulchwu_ (void)
{
    __asm__ __volatile__ ("mulchwu.     17, 14, 15");
}

static void test_mulhhw_ (void)
{
    __asm__ __volatile__ ("mulhhw.      17, 14, 15");
}

static void test_mulhhwu_ (void)
{
    __asm__ __volatile__ ("mulhhwu.     17, 14, 15");
}

static void test_mullhw_ (void)
{
    __asm__ __volatile__ ("mullhw.      17, 14, 15");
}

static void test_mullhwu_ (void)
{
    __asm__ __volatile__ ("mullhwu.     17, 14, 15");
}

static void test_nmacchw_ (void)
{
    __asm__ __volatile__ ("nmacchw.     17, 14, 15");
}

static void test_nmacchwo_ (void)
{
    __asm__ __volatile__ ("nmacchwo.    17, 14, 15");
}

static void test_nmacchws_ (void)
{
    __asm__ __volatile__ ("nmacchws.    17, 14, 15");
}

static void test_nmacchwso_ (void)
{
    __asm__ __volatile__ ("nmacchwso.   17, 14, 15");
}

static void test_nmachhw_ (void)
{
    __asm__ __volatile__ ("nmachhw.     17, 14, 15");
}

static void test_nmachhwo_ (void)
{
    __asm__ __volatile__ ("nmachhwo.    17, 14, 15");
}

static void test_nmachhws_ (void)
{
    __asm__ __volatile__ ("nmachhws.    17, 14, 15");
}

static void test_nmachhwso_ (void)
{
    __asm__ __volatile__ ("nmachhwso.   17, 14, 15");
}

static void test_nmaclhw_ (void)
{
    __asm__ __volatile__ ("nmaclhw.     17, 14, 15");
}

static void test_nmaclhwo_ (void)
{
    __asm__ __volatile__ ("nmaclhwo.    17, 14, 15");
}

static void test_nmaclhws_ (void)
{
    __asm__ __volatile__ ("nmaclhws.    17, 14, 15");
}

static void test_nmaclhwso_ (void)
{
    __asm__ __volatile__ ("nmaclhwso.   17, 14, 15");
}

static test_t tests_p4mc_ops_two[] = {
    { &test_macchw_         , "     macchw.", },
    { &test_macchwo_        , "    macchwo.", },
    { &test_macchws_        , "    macchws.", },
    { &test_macchwso_       , "   macchwso.", },
    { &test_macchwsu_       , "   macchwsu.", },
    { &test_macchwsuo_      , "  macchwsuo.", },
    { &test_macchwu_        , "    macchwu.", },
    { &test_macchwuo_       , "   macchwuo.", },
    { &test_machhw_         , "     machhw.", },
    { &test_machhwo_        , "    machhwo.", },
    { &test_machhws_        , "    machhws.", },
    { &test_machhwso_       , "   machhwso.", },
    { &test_machhwsu_       , "   machhwsu.", },
    { &test_machhwsuo_      , "  machhwsuo.", },
    { &test_machhwu_        , "    machhwu.", },
    { &test_machhwuo_       , "   machhwuo.", },
    { &test_maclhw_         , "     maclhw.", },
    { &test_maclhwo_        , "    maclhwo.", },
    { &test_maclhws_        , "    maclhws.", },
    { &test_maclhwso_       , "   maclhwso.", },
    { &test_maclhwsu_       , "   maclhwsu.", },
    { &test_maclhwsuo_      , "  maclhwsuo.", },
    { &test_maclhwu_        , "    maclhwu.", },
    { &test_maclhwuo_       , "   maclhwuo.", },
    { &test_mulchw_         , "     mulchw.", },
    { &test_mulchwu_        , "    mulchwu.", },
    { &test_mulhhw_         , "     mulhhw.", },
    { &test_mulhhwu_        , "    mulhhwu.", },
    { &test_mullhw_         , "     mullhw.", },
    { &test_mullhwu_        , "    mullhwu.", },
    { &test_nmacchw_        , "    nmacchw.", },
    { &test_nmacchwo_       , "   nmacchwo.", },
    { &test_nmacchws_       , "   nmacchws.", },
    { &test_nmacchwso_      , "  nmacchwso.", },
    { &test_nmachhw_        , "    nmachhw.", },
    { &test_nmachhwo_       , "   nmachhwo.", },
    { &test_nmachhws_       , "   nmachhws.", },
    { &test_nmachhwso_      , "  nmachhwso.", },
    { &test_nmaclhw_        , "    nmaclhw.", },
    { &test_nmaclhwo_       , "   nmaclhwo.", },
    { &test_nmaclhws_       , "   nmaclhws.", },
    { &test_nmaclhwso_      , "  nmaclhwso.", },
    { NULL,                   NULL,           },
};
#endif /* defined (IS_PPC405) */

static test_table_t all_tests[] = {
    {
        tests_ia_ops_two      ,
        "PPC integer arith insns with two args",
        0x00010102,
    },
    {
        tests_iar_ops_two     ,
        "PPC integer arith insns with two args with flags update",
        0x01010102,
    },
    {
        tests_iac_ops_two     ,
        "PPC integer arith insns with two args and carry",
        0x02010102,
    },
    {
        tests_iacr_ops_two    ,
        "PPC integer arith insns with two args and carry with flags update",
        0x03010102,
    },
    {
        tests_il_ops_two      ,
        "PPC integer logical insns with two args",
        0x00010202,
    },
    {
        tests_il_ops_two_sh      ,
        "PPC integer shift algebraic two args",
        0x00070202,
    },
    {
        tests_ilr_ops_two     ,
        "PPC integer logical insns with two args with flags update",
        0x01010202,
    },
    {
        tests_ilr_ops_two_sh     ,
        "PPC integer shift algebraic two args with flags update",
        0x01070202,
    },
    {
        tests_icr_ops_two     ,
        "PPC integer compare insns (two args)",
        0x01010304,
    },
    {
        tests_icr_ops_two_i16 ,
        "PPC integer compare with immediate insns (two args)",
        0x01010305,
    },
    {
        tests_ia_ops_two_i16  ,
        "PPC integer arith insns\n    with one register + one 16 bits immediate args",
        0x00010106,
    },
    {
        tests_iar_ops_two_i16 ,
        "PPC integer arith insns\n    with one register + one 16 bits immediate args with flags update",
        0x01010106,
    },
    {
        tests_il_ops_two_i16  ,
        "PPC integer logical insns\n    with one register + one 16 bits immediate args",
        0x00010206,
    },
    {
        tests_ilr_ops_two_i16 ,
        "PPC integer logical insns\n    with one register + one 16 bits immediate args with flags update",
        0x01010206,
    },
    {
        tests_crl_ops_two     ,
        "PPC condition register logical insns - two operands",
        0x01010202,
    },
    {
        tests_iac_ops_one     ,
        "PPC integer arith insns with one arg and carry",
        0x02010101,
    },
    {
        tests_iacr_ops_one    ,
        "PPC integer arith insns with one arg and carry with flags update",
        0x03010101,
    },
    {
        tests_il_ops_one      ,
        "PPC integer logical insns with one arg",
        0x00010201,
    },
    {
        tests_ilr_ops_one     ,
        "PPC integer logical insns with one arg with flags update",
        0x01010201,
    },
    {
        tests_il_ops_mfspr      ,
        "PPC mfspr instructions",
        0x00080207,
    },
    {
        tests_il_ops_spe      ,
        "PPC logical insns with special forms",
        0x00010207,
    },
    {
        tests_il_ops_spe_sh      ,
        "PPC shift algebraic with special forms",
        0x00070207,
    },
    {
        tests_ilr_ops_spe     ,
        "PPC logical insns with special forms with flags update",
        0x01010207,
    },
    {
        tests_ilr_ops_spe_sh     ,
        "PPC shift algebraic with special forms with flags update",
        0x01070207,
    },
    {
        tests_ild_ops_two_i16 ,
        "PPC integer load insns\n    with one register + one 16 bits immediate args with flags update",
        0x00010508,
    },
    {
        tests_ild_ops_two     ,
        "PPC integer load insns with two register args",
        0x00010509,
    },
    {
        tests_ist_ops_three_i16,
        "PPC integer store insns\n    with one register + one 16 bits immediate args with flags update",
        0x0001050a,
    },
    {
        tests_ist_ops_three   ,
        "PPC integer store insns with three register args",
        0x0001050b,
    },
    {
        tests_popcnt_ops_one   ,
        "PPC integer population count with one register args, no flags",
        0x00010601,
    },
#if !defined (NO_FLOAT)
    {
        tests_fa_ops_three    ,
        "PPC floating point arith insns with three args",
        0x00020103,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_far_ops_three    ,
        "PPC floating point arith insns\n    with three args with flags update",
        0x01020103,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fa_ops_two      ,
        "PPC floating point arith insns with two args",
        0x00020102,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_far_ops_two     ,
        "PPC floating point arith insns\n    with two args with flags update",
        0x01020102,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fcr_ops_two     ,
        "PPC floating point compare insns (two args)",
        0x01020304,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fa_ops_one      ,
        "PPC floating point arith insns with one arg",
        0x00020101,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_far_ops_one     ,
        "PPC floating point arith insns\n    with one arg with flags update",
        0x01020101,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fl_ops_spe      ,
        "PPC floating point status register manipulation insns",
        0x00020207,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_flr_ops_spe     ,
        "PPC floating point status register manipulation insns\n  with flags update",
        0x01020207,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fld_ops_two_i16 ,
        "PPC float load insns\n    with one register + one 16 bits immediate args with flags update",
        0x00020508,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fld_ops_two     ,
        "PPC float load insns with two register args",
        0x00020509,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fst_ops_three_i16,
        "PPC float store insns\n    with one register + one 16 bits immediate args with flags update",
        0x0002050a,
    },
#endif /* !defined (NO_FLOAT) */
#if !defined (NO_FLOAT)
    {
        tests_fst_ops_three   ,
        "PPC float store insns with three register args",
        0x0002050b,
    },
#endif /* !defined (NO_FLOAT) */
#if defined (HAS_ALTIVEC)
    {
        tests_aa_ops_three    ,
        "PPC altivec integer arith insns with three args",
        0x00040103,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_al_ops_three    ,
        "PPC altivec integer logical insns with three args",
        0x00040203,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_aa_ops_two      ,
        "PPC altivec integer arith insns with two args",
        0x00040102,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_al_ops_two      ,
        "PPC altivec integer logical insns with two args",
        0x00040202,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_al_ops_one      ,
        "PPC altivec integer logical insns with one arg",
        0x00040201,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_ac_ops_two      ,
        "Altivec integer compare insns",
        0x00040302,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_acr_ops_two     ,
        "Altivec integer compare insns with flags update",
        0x01040302,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_av_int_ops_spe  ,
        "Altivec integer special insns",
        0x00040207,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_ald_ops_two     ,
        "Altivec load insns with two register args",
        0x00040509,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_ast_ops_three   ,
        "Altivec store insns with three register args",
        0x0004050b,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_afa_ops_two     ,
        "Altivec floating point arith insns with two args",
        0x00050102,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_afa_ops_three   ,
        "Altivec floating point arith insns with three args",
        0x00050103,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_afa_ops_one     ,
        "Altivec floating point arith insns with one arg",
        0x00050101,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_afc_ops_two     ,
        "Altivec floating point compare insns",
        0x00050302,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_afcr_ops_two    ,
        "Altivec floating point compare insns with flags update",
        0x01050302,
    },
#endif /* defined (HAS_ALTIVEC) */
#if defined (HAS_ALTIVEC)
    {
        tests_av_float_ops_spe,
        "Altivec float special insns",
        0x00050207,
    },
#endif /* defined (HAS_ALTIVEC) */
    {
        tests_dcbt,
        "Miscellaneous test: Data cache insns",
        0x0006070C,
    },
#if defined (IS_PPC405)
    {
        tests_p4m_ops_two     ,
        "PPC 405 mac insns with three args",
        0x00030102,
    },
#endif /* defined (IS_PPC405) */
#if defined (IS_PPC405)
    {
        tests_p4mc_ops_two    ,
        "PPC 405 mac insns with three args with flags update",
        0x01030102,
    },
#endif /* defined (IS_PPC405) */
    { NULL,                   NULL,               0x00000000, },
};

/* -------------- END #include "ops-ppc.c" -------------- */

static int verbose = 0;
static int arg_list_size = 0;

static double *fargs = NULL;
static int nb_fargs = 0;
static int nb_normal_fargs = 0;
static HWord_t *iargs = NULL;
static int nb_iargs = 0;
static uint16_t *ii16 = NULL;
static int nb_ii16 = 0;

#if defined (HAS_ALTIVEC)
static vector unsigned int* viargs = NULL;
static int nb_viargs = 0;
static vector float* vfargs = NULL;
static int nb_vfargs = 0;

//#define TEST_VSCR_SAT
#endif

static inline void register_farg (void *farg,
                                  int s, uint16_t _exp, uint64_t mant)
{
   uint64_t tmp;
   
   tmp = ((uint64_t)s << 63) | ((uint64_t)_exp << 52) | mant;
   *(uint64_t *)farg = tmp;
#ifndef __powerpc64__
   AB_DPRINTF("%d %03x %013llx => %016llx %0e\n",
#else
   AB_DPRINTF("%d %03x %013lx => %016lx %0e\n",
#endif
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
    * +QNaN     : 0 0x7FF 0x7FFFFFFFFFFFF => 0x7FF7FFFFFFFFFFFF
    * -QNaN     : 1 0x7FF 0x7FFFFFFFFFFFF => 0xFFF7FFFFFFFFFFFF
    * +SNaN     : 0 0x7FF 0x8000000000000 => 0x7FF8000000000000
    * -SNaN     : 1 0x7FF 0x8000000000000 => 0xFFF8000000000000
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
    * +QNaN     : 0 0xFF 0x3FFFFF => 0x7FBFFFFF
    * -QNaN     : 1 0xFF 0x3FFFFF => 0xFFBFFFFF
    * +SNaN     : 0 0xFF 0x400000 => 0x7FC00000
    * -SNaN     : 1 0xFF 0x400000 => 0xFFC00000
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
//       for (e0=0; e0<2; e0++) {
            for (e1=0x001; ; e1 = ((e1 + 1) << 13) + 7) {  // x2
//          for (e1=0x001; ; e1 = ((e1 + 1) << 5) + 7) {   // x3
               if (e1 >= 0x400)
                  e1 = 0x3fe;
//             _exp = (e0 << 10) | e1;
               _exp = e1;
               for (mant = 0x0000000000001ULL; mant < (1ULL << 52);
                    /* Add 'random' bits */
                    mant = ((mant + 0x4A6) << 29) + 0x359) {  // x2
                  register_farg(&fargs[i++], s, _exp, mant);
               }
               if (e1 == 0x3fe)
                  break;
            }
//       }
      }
   }

   /* To iterate over non-special values only */
   nb_normal_fargs = i;


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

static void build_iargs_table (void)
{
   uint64_t tmp;
   int i=0;
   
#ifndef __powerpc64__
   if (arg_list_size == 1) {                   // Large
      iargs = malloc(400 * sizeof(HWord_t));
      for (tmp=0; ; tmp = tmp + 1 + (tmp >> 1)) {
         if (tmp >= 0x100000000ULL)
            tmp = 0xFFFFFFFF;
         iargs[i++] = (HWord_t)tmp;
         AB_DPRINTF("val %08x\n", (HWord_t)tmp);
         if (tmp == 0xFFFFFFFF)
            break;
      }
   } else {                                    // Default
      iargs = malloc(10 * sizeof(HWord_t));
      // for (tmp = 0; ; tmp = 71*tmp + 1 + (tmp>>1)) {  // gives 8
      // for (tmp = 0; ; tmp = 100000*tmp + 1 + (tmp>>1)) {  // gives 4
      for (tmp=0; ; tmp = 999999*tmp + 999999) {  // gives 3
         if (tmp >= 0x100000000ULL)
            tmp = 0xFFFFFFFF;
         iargs[i++] = (HWord_t)tmp;
         AB_DPRINTF("val %08x\n", (HWord_t)tmp);
         if (tmp == 0xFFFFFFFF)
            break;
      }
   }
#else
   if (arg_list_size == 1) {                   // Large
      iargs = malloc(800 * sizeof(HWord_t));
      for (tmp=0; ; tmp = 2*tmp + 1 + (tmp >> 2)) {
         if ((long)tmp < 0 )
            tmp = 0xFFFFFFFFFFFFFFFFULL;
         iargs[i++] = tmp;
         AB_DPRINTF("val %016lx\n", tmp);
         if (tmp == 0xFFFFFFFFFFFFFFFFULL)
            break;
      }
   } else {                                    // Default
      iargs = malloc(20 * sizeof(HWord_t));
      // for (tmp=0; ; tmp = 9999*tmp + 999999) {  // gives 6
      for (tmp = 0; ; tmp = 123456789*tmp + 123456789999) {  // gives 3
         if ((long)tmp < 0 )
            tmp = 0xFFFFFFFFFFFFFFFFULL;
         iargs[i++] = tmp;
         AB_DPRINTF("val %016lx\n", tmp);
         if (tmp == 0xFFFFFFFFFFFFFFFFULL)
            break;
      }
   }
#endif // #ifndef __powerpc64__

   AB_DPRINTF("Registered %d iargs values\n", i);
   nb_iargs = i;
}

static void build_ii16_table (void)
{
   uint32_t tmp;
   int i=0;
   
   if (arg_list_size == 1) {                   // Large
      ii16 = malloc(200 * sizeof(uint32_t));
      for (tmp=0; ; tmp = tmp + 1 + (tmp >> 2)) {
         if (tmp >= 0x10000)
            tmp = 0xFFFF;
         ii16[i++] = tmp;
         AB_DPRINTF("val %04x\n", tmp);
         if (tmp == 0xFFFF)
            break;
      }
   } else {                                    // Default
      ii16 = malloc(10 * sizeof(uint32_t));
      for (tmp=0; ; tmp = 999*tmp + 999) {  // gives 3
         if (tmp >= 0x10000)
            tmp = 0xFFFF;
         ii16[i++] = tmp;
         AB_DPRINTF("val %04x\n", tmp);
         if (tmp == 0xFFFF)
            break;
      }
   }
   AB_DPRINTF("Registered %d ii16 values\n", i);
   nb_ii16 = i;
}

#if defined (HAS_ALTIVEC)
static void build_viargs_table (void)
{
#if !defined (ALTIVEC_ARGS_LARGE)
   unsigned int i=2;
   viargs = memalign16(i * sizeof(vector unsigned int));
   viargs[0] = (vector unsigned int) { 0x01020304,0x05060708,0x090A0B0C,0x0E0D0E0F };
   AB_DPRINTF_VEC32x4( viargs[0] );
   viargs[1] = (vector unsigned int) { 0xF1F2F3F4,0xF5F6F7F8,0xF9FAFBFC,0xFEFDFEFF };
   AB_DPRINTF_VEC32x4( viargs[1] );
#else
   unsigned int i,j;
   // build from iargs table (large/default already set)
   viargs = malloc(nb_iargs * sizeof(vector unsigned int));
   for (i=0; i<nb_iargs; i++) {
      j = iargs[i];
      viargs[i] = (vector unsigned int){ j, j*2, j*3, j*4 };
      AB_DPRINTF_VEC32x4( viargs[i] );
   }
#endif

   AB_DPRINTF("Registered %d viargs values\n", i);
   nb_viargs = i;
}

static inline void register_vfarg (vector float* vfarg,
                                  int s, uint8_t _exp, uint32_t mant)
{
   uint32_t tmp;
   vector uint32_t* vfargI = (vector uint32_t*)vfarg;

   tmp = ((uint64_t)s << 31) | ((uint64_t)_exp << 23) | mant;
   *vfargI = (vector uint32_t){ tmp,tmp,tmp,tmp };
   AB_DPRINTF("%d %02x %06x => %08x %0e\n",
              s, _exp, mant, *((uint32_t*)&tmp), *(float*)&tmp);
}

static void build_vfargs_table (void)
{
   /* Sign goes from zero to one
    * Exponent goes from 0 to ((1 << 9) - 1)
    * Mantissa goes from 1 to ((1 << 24) - 1)
    * + special values:
    * +0.0      : 0 0x00 0x000000            => 0x00000000
    * -0.0      : 1 0x00 0x000000            => 0x80000000
    * +infinity : 0 0xFF 0x000000            => 0x7F800000
    * -infinity : 1 0xFF 0x000000            => 0xFF800000
    * +SNaN     : 0 0xFF 0x7FFFFF (non-zero) => 0x7FFFFFFF
    * -SNaN     : 1 0xFF 0x7FFFFF (non-zero) => 0xFFFFFFFF
    * +QNaN     : 0 0xFF 0x3FFFFF (non-zero) => 0x7FBFFFFF
    * -QNaN     : 1 0xFF 0x3FFFFF (non-zero) => 0xFFBFFFFF
    * (8 values)
    */
   uint32_t mant;
   uint16_t _exp;
   int s;
   int i=0;
   

#if !defined (ALTIVEC_ARGS_LARGE)
   nb_vfargs = 12;
   vfargs = memalign16(nb_vfargs * sizeof(vector float));

   // 4 values:
   for (s=0; s<2; s++) {
      for (_exp=0x5; ; _exp += 0x9D ) {
         if (_exp > 0xDF)
            break;
         for (mant = 0x3FFFFF; mant < 0x7FFFFF;
              mant = /* random */ ((mant + 0x1A6) << 31) + 0x159) {
            register_vfarg(&vfargs[i++], s, (uint8_t)_exp, mant);
         }
      }
   }
#else
   nb_vfargs = 50;
   vfargs = memalign16(nb_vfargs * sizeof(vector float));

   for (s=0; s<2; s++) {
      for (_exp=0x0; ; _exp += 0x3F ) {
         //      for (_exp=0; ; _exp = ((_exp + 1) << 1) + 3) {
         if (_exp >= 0xFE)
            _exp = 0xFE;
         for (mant = 0x0; mant < 0x7FFFFF;
              mant = /* random */ ((mant + 0x4A6) << 5) + 0x359) {
            register_vfarg(&vfargs[i++], s, (uint8_t)_exp, mant);
         }
         if (_exp >= 0xFE)
            break;
      }
   }
#endif

   /* Special values */
   /* +0.0      : 0 0x00 0x000000 */
   s = 0;
   _exp = 0x00;
   mant = 0x000000;
   register_vfarg(&vfargs[i++], s, _exp, mant);
   /* -0.0      : 1 0x00 0x000000 */
   s = 1;
   _exp = 0x00;
   mant = 0x000000;
   register_vfarg(&vfargs[i++], s, _exp, mant);

   /* +infinity : 0 0xFF 0x000000  */
   s = 0;
   _exp = 0xFF;
   mant = 0x000000;
   register_vfarg(&vfargs[i++], s, _exp, mant);
   /* -infinity : 1 0xFF 0x000000 */
   s = 1;
   _exp = 0xFF;
   mant = 0x000000;
   register_vfarg(&vfargs[i++], s, _exp, mant);

   /* NaN: _exponent all 1s, non-zero fraction */
   /* SNaN is a NaN with the most significant fraction bit clear.*/
   /* +SNaN     : 0 0xFF 0x7FFFFF */
   s = 0;
   _exp = 0xFF;
   mant = 0x7FFFFF;
   register_vfarg(&vfargs[i++], s, _exp, mant);
   /* -SNaN     : 1 0xFF 0x7FFFFF */
   s = 1;
   _exp = 0xFF;
   mant = 0x7FFFFF;
   register_vfarg(&vfargs[i++], s, _exp, mant);

   /* QNaN is a NaN with the most significant fraction bit set */
   /* +QNaN     : 0 0xFF 0x3F0000 */
   s = 0;
   _exp = 0xFF;
   mant = 0x3FFFFF;
   register_vfarg(&vfargs[i++], s, _exp, mant);
   /* -QNaN     : 1 0xFF 0x3F0000 */
   s = 1;
   _exp = 0xFF;
   mant = 0x3FFFFF;
   register_vfarg(&vfargs[i++], s, _exp, mant);
   AB_DPRINTF("Registered %d vfargs values\n", i);

   assert(i <= nb_vfargs);
   nb_vfargs = i;
}
#endif

#if 0
static void dump_iargs (void)
{
   int i;
   for (i = 0; i < nb_iargs; i++) {
      printf("iarg %d: %08x %08x %08x\n", i, iargs[i],
             (unsigned int)&iargs[i], (unsigned int)iargs);
   }
}

static void dump_iargs16 (void)
{
   int i;
   for (i = 0; i < nb_ii16; i++) {
      printf("iarg16 %d: %08x %08x %08x\n", i, ii16[i],
             (unsigned int)&ii16[i], (unsigned int)ii16);
   }
}

static void dump_vfargs (void)
{
   vector float vf;
   float f;
   int i=0;
   for (i=0; i<nb_vfargs; i++) {
      vf = (vector float)vfargs[i];
      f  = ((float*)&vf)[0];
      printf("vfarg %3d: %24f : %08x\n", i, f, ((unsigned int*)&f)[0]);
   }
}
#endif

static void test_int_three_args (const char* name, test_func_t func,
                                 unused uint32_t test_flags)
{
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, k;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_iargs; j++) {
         for (k=0; k<nb_iargs; k++) {
            r14 = iargs[i];
            r15 = iargs[j];
            r16 = iargs[k];

	    SET_CR_XER_ZERO;
            (*func)();
	    GET_CR_XER(flags,xer);
            res = r17;

#ifndef __powerpc64__
            printf("%s %08x, %08x, %08x => %08x (%08x %08x)\n",
#else
            printf("%s %016llx, %016llx, %016llx => %016llx (%08x %08x)\n",
#endif
                   name, iargs[i], iargs[j], iargs[k], res, flags, xer);
         }
         if (verbose) printf("\n");
      }
   }
}

static void test_int_two_args (const char* name, test_func_t func,
                               uint32_t test_flags)
{
   volatile HWord_t res;
   volatile uint32_t flags, xer, xer_orig;
   int i, j, is_div;
#ifdef __powerpc64__
   int zap_hi32;
#endif

   // catches div, divwu, divo, divwu, divwuo, and . variants
   is_div = strstr(name, "divw") != NULL;

#ifdef __powerpc64__
   zap_hi32 = strstr(name, "mulhw") != NULL;
#endif
   
   xer_orig = 0x00000000;
 redo:
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_iargs; j++) {

         /* result of division by zero is implementation dependent.
            don't test it. */
         if (is_div && iargs[j] == 0)
            continue;

         r14 = iargs[i];
         r15 = iargs[j];

         SET_XER(xer_orig);
         SET_CR_ZERO;
         (*func)();
         GET_CR_XER(flags,xer);
         res = r17;

#ifndef __powerpc64__
         printf("%s %08x, %08x => %08x (%08x %08x)\n",
#else
         if (zap_hi32) res &= 0xFFFFFFFFULL;
         printf("%s %016llx, %016llx => %016llx (%08x %08x)\n",
#endif
                name, iargs[i], iargs[j], res, flags, xer);
      }
      if (verbose) printf("\n");
   }
   if ((test_flags & PPC_XER_CA) && xer_orig == 0x00000000) {
      xer_orig = 0x20000000;
      goto redo;
   }
}

static void test_int_one_arg (const char* name, test_func_t func,
                               uint32_t test_flags)
{
   volatile HWord_t res;
   volatile uint32_t flags, xer, xer_orig;
   int i;
   
   xer_orig = 0x00000000;
 redo:
   for (i=0; i<nb_iargs; i++) {
      r14 = iargs[i];
      SET_XER(xer_orig);
      SET_CR_ZERO;
      (*func)();
      res = r17;
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %08x => %08x (%08x %08x)\n",
#else
      printf("%s %016llx => %016llx (%08x %08x)\n",
#endif
             name, iargs[i], res, flags, xer);
   }
   if ((test_flags & PPC_XER_CA) && xer_orig == 0x00000000) {
      xer_orig = 0x20000000;
      goto redo;
   }
}

static inline void invalidate_icache ( void *ptr, int nbytes )
{
   HWord_t startaddr = (HWord_t) ptr;
   HWord_t endaddr   = startaddr + nbytes;
   HWord_t cls       = 32; /*VG_(cache_line_size_ppc32);*/
   HWord_t addr;

   startaddr &= ~(cls - 1);
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("dcbst 0,%0" : : "r" (addr));
   asm volatile("sync");
   for (addr = startaddr; addr < endaddr; addr += cls)
      asm volatile("icbi 0,%0" : : "r" (addr));
   asm volatile("sync; isync");
}

/* for god knows what reason, if this isn't inlined, the
   program segfaults. */
static inline
void _patch_op_imm (uint32_t *p_insn, uint16_t imm, int sh, int len)
{
   uint32_t mask = ((1 << len) - 1) << sh;
   *p_insn = (*p_insn & ~mask) | ((imm<<sh) & mask);
}

static inline
void patch_op_imm (uint32_t* p_insn, uint16_t imm, int sh, int len)
{
   _patch_op_imm(p_insn, imm, sh, len);
   invalidate_icache(p_insn, 4);
}

static inline
void patch_op_imm16 (uint32_t *p_insn, uint16_t imm)
{
   patch_op_imm(p_insn, imm, 0, 16);
}


/* Copy the 2 insn function starting at p_func_F to func_buf[], and
   return a possibly different pointer, which, when called, runs the
   copy in func_buf[]. */
static inline
test_func_t init_function( test_func_t p_func_F, uint32_t func_buf[] )
{
   uint32_t* p_func = (uint32_t*)p_func_F;
#if !defined(__powerpc64__) || _CALL_ELF == 2
   func_buf[0] = p_func[0];
   func_buf[1] = p_func[1];
   return (test_func_t)&func_buf[0];
#else
   /* p_func points to a function descriptor, the first word of which
      points to the real code.  Copy the code itself but not the
      descriptor, and just swizzle the descriptor's entry pointer. */
   uint64_t* descr = (uint64_t*)p_func;
   uint32_t* entry = (uint32_t*)(descr[0]);
   func_buf[0] = entry[0];
   func_buf[1] = entry[1];
   descr[0] = (uint64_t)&func_buf[0];
   return (test_func_t)descr;
#endif // #ifndef __powerpc64__
}


static void test_int_one_reg_imm16 (const char* name,
                                    test_func_t func_IN,
                                    unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j;

   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_ii16; j++) {
         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm16(&func_buf[0], ii16[j]);

         r14 = iargs[i];

         SET_CR_XER_ZERO;
         (*func)();
         GET_CR_XER(flags,xer);
         res = r17;

#ifndef __powerpc64__
         printf("%s %08x, %08x => %08x (%08x %08x)\n",
#else
         printf("%s %016llx, %08x => %016llx (%08x %08x)\n",
#endif
                name, iargs[i], ii16[j], res, flags, xer);
      }
      if (verbose) printf("\n");
   }
}

/* Special test cases for:
 * rlwimi
 * rlwinm
 * rlwnm
 * srawi
 * mcrf
 * mcrfs
 * mcrxr_cb
 * mfcr_cb
 * mfspr_cb
 * mftb_cb
 * mtcrf_cb
 * mtspr_cb

 __powerpc64__ only:
 * rldcl       rA,rS,SH,MB
 * rldcr       rA,rS,SH,ME
 * rldic       rA,rS,SH,MB
 * rldicl      rA,rS,SH,MB
 * rldicr      rA,rS,SH,ME
 * rldimi      rA,rS,SH,MB
 * sradi       rA,rS,SH
 */

static void rlwi_cb (const char* name, test_func_t func_IN,
                     unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, k, l, arg_step;
   
   arg_step = (arg_list_size == 0) ? 31 : 3;
   
   r17 = 0;  // rlwimi takes r17 as input: start with a clean slate.

   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<32; j+=arg_step) {
         for (k=0; k<32; k+=arg_step) {
            for (l=0; l<32; l+=arg_step) {
               /* Patch up the instruction */
               func = init_function( func_IN, func_buf );
               _patch_op_imm(&func_buf[0], j, 11, 5);
               _patch_op_imm(&func_buf[0], k, 6, 5);
               patch_op_imm(&func_buf[0], l, 1, 5);

               r14 = iargs[i];

               SET_CR_XER_ZERO;
               (*func)();
               GET_CR_XER(flags,xer);
               res = r17;

#ifndef __powerpc64__
               printf("%s %08x, %2d, %2d, %2d => %08x (%08x %08x)\n",
#else
               printf("%s %016llx, %2d, %2d, %2d => %016llx (%08x %08x)\n",
#endif
                      name, iargs[i], j, k, l, res, flags, xer);
            }
            if (verbose) printf("\n");
         }
      }
   }
}

static void rlwnm_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, k, l, arg_step;
   
   arg_step = (arg_list_size == 0) ? 31 : 3;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_iargs; j++) {
         for (k=0; k<32; k+=arg_step) {
            for (l=0; l<32; l+=arg_step) {
               /* Patch up the instruction */
               func = init_function( func_IN, func_buf );
               _patch_op_imm(&func_buf[0], k, 6, 5);
               patch_op_imm(&func_buf[0], l, 1, 5);

               r14 = iargs[i];
               r15 = iargs[j];

               SET_CR_XER_ZERO;
               (*func)();
               GET_CR_XER(flags,xer);
               res = r17;

#ifndef __powerpc64__
               printf("%s %08x, %08x, %2d, %2d => %08x (%08x %08x)\n",
#else
               printf("%s %016llx, %016llx, %2d, %2d => %016llx (%08x %08x)\n",
#endif
                      name, iargs[i], iargs[j], k, l, res, flags, xer);
            }
            if (verbose) printf("\n");
         }
      }
   }
}

static void srawi_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, arg_step;
   
   arg_step = (arg_list_size == 0) ? 31 : 1;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<32; j+=arg_step) {
         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm(&func_buf[0], j, 11, 5);

         r14 = iargs[i];

         SET_CR_XER_ZERO;
         (*func)();
         GET_CR_XER(flags,xer);
         res = r17;

#ifndef __powerpc64__
         printf("%s %08x, %2d => %08x (%08x %08x)\n",
#else
         printf("%s %016llx, %2d => %016llx (%08x %08x)\n",
#endif
                name, iargs[i], j, res, flags, xer);
      }
      if (verbose) printf("\n");
   }
}

static void mcrf_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, xer;
   int i, j, k, arg_step;
   
   arg_step = (arg_list_size == 0) ? 7 : 1;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<8; j+=arg_step) {
         for (k=0; k<8; k+=arg_step) {
            /* Patch up the instruction */
            func = init_function( func_IN, func_buf );
            _patch_op_imm(&func_buf[0], j, 23, 3);
            patch_op_imm(&func_buf[0], k, 18, 3);

            r14 = iargs[i];

            SET_CR(r14);
            SET_XER_ZERO;
            (*func)();
            GET_CR_XER(flags,xer);

#ifndef __powerpc64__
            printf("%s %d, %d (%08x) => (%08x %08x)\n",
#else
            printf("%s %d, %d (%016llx) => (%08x %08x)\n",
#endif
                   name, j, k, iargs[i], flags, xer);
         }
         if (verbose) printf("\n");
      }
   }
}

static void mcrxr_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, xer;
   int i, j, k, arg_step;
   
   arg_step = 1; //(arg_list_size == 0) ? 7 : 1;
   
   for (i=0; i<16; i+=arg_step) {
      j = i << 28;
      for (k=0; k<8; k+=arg_step) {
         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm(&func_buf[0], k, 23, 3);

         r14 = j;

	 SET_CR_ZERO;
	 SET_XER(r14);
         (*func)();
         GET_CR_XER(flags,xer);

         printf("%s %d (%08x) => (%08x %08x)\n",
                name, k, j, flags, xer);
      }
      if (verbose) printf("\n");
   }
}

static void mfcr_cb (const char* name, test_func_t func,
                     unused uint32_t test_flags)
{
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i;
   
   for (i=0; i<nb_iargs; i++) {
      r14 = iargs[i];

      /* Set up flags for test */
      SET_CR(r14);
      SET_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = r17;

#ifndef __powerpc64__
      printf("%s (%08x) => %08x (%08x %08x)\n",
#else
      printf("%s (%016llx) => %016llx (%08x %08x)\n",
#endif
             name, iargs[i], res, flags, xer);
   }
}

// NOTE: Not using func: calling function kills lr
static void mfspr_cb (const char* name, test_func_t func,
                      unused uint32_t test_flags)
{
   //volatile uint32_t res, flags, xer, ctr, lr, tmpcr, tmpxer;
   volatile HWord_t res;
   int j, k;
   func = func; // just to stop compiler complaining

   // mtxer followed by mfxer
   for (k=0; k<nb_iargs; k++) {
      j = iargs[k];
      __asm__ __volatile__(
         "mtxer %1\n"
         "\tmfxer %0"
         : /*out*/"=b"(res) : /*in*/"b"(j) : /*trashed*/"xer" 
      );
      res &= 0xE000007F; /* rest of the bits are undefined */

#ifndef __powerpc64__
      printf("%s 1 (%08x) -> mtxer -> mfxer => %08x\n",
#else
      printf("%s 1 (%08x) -> mtxer -> mfxer => %016llx\n",
#endif
             name, j, res);
   }

   // mtlr followed by mflr
   for (k=0; k<nb_iargs; k++) {
      j = iargs[k];
      __asm__ __volatile__(
         "mtlr %1\n"
         "\tmflr %0"
         : /*out*/"=b"(res) : /*in*/"b"(j) : /*trashed*/"lr" 
      );

#ifndef __powerpc64__
      printf("%s 8 (%08x) ->  mtlr ->  mflr => %08x\n",
#else
      printf("%s 8 (%08x) ->  mtlr ->  mflr => %016llx\n",
#endif
             name, j, res);
   }

   // mtctr followed by mfctr
   for (k=0; k<nb_iargs; k++) {
      j = iargs[k];
      __asm__ __volatile__(
         "mtctr %1\n"
         "\tmfctr %0"
         : /*out*/"=b"(res) : /*in*/"b"(j) : /*trashed*/"ctr" 
      );

#ifndef __powerpc64__
      printf("%s 9 (%08x) -> mtctr -> mfctr => %08x\n",
#else
      printf("%s 9 (%08x) -> mtctr -> mfctr => %016llx\n",
#endif
             name, j, res);
   }
}

static void mtcrf_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, xer;
   int i, j, arg_step;
   
   arg_step = (arg_list_size == 0) ? 99 : 1;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<256; j+=arg_step) {
         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm(&func_buf[0], j, 12, 8);

         r14 = iargs[i];

         SET_CR_XER_ZERO;
         (*func)();
         GET_CR_XER(flags,xer);

#ifndef __powerpc64__
         printf("%s %3d, %08x => (%08x %08x)\n",
#else
         printf("%s %3d, %016llx => (%08x %08x)\n",
#endif
                name, j, iargs[i], flags, xer);
      }
      if (verbose) printf("\n");
   }
}

// NOTE: Not using func: calling function kills lr
static void mtspr_cb (const char* name, test_func_t func,
                      unused uint32_t test_flags)
{
}

#ifdef __powerpc64__
static void rldc_cb (const char* name, test_func_t func_IN,
                     unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, k, arg_step;
   
   arg_step = (arg_list_size == 0) ? 7 : 3;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_iargs; j++) {
         for (k=0; k<64; k+=arg_step) {
            /* Patch up the instruction */
            func = init_function( func_IN, func_buf );
            patch_op_imm(&func_buf[0], (((k & 0x1F)<<1) | ((k>>5)&1)), 5, 6);
            
            r14 = iargs[i];
            r15 = iargs[j];

            SET_CR_XER_ZERO;
            (*func)();
            GET_CR_XER(flags,xer);
            res = r17;

            printf("%s %016llx, %016llx, %2d => %016llx (%08x %08x)\n",
                   name, iargs[i], iargs[j], k, res, flags, xer);
         }
         if (verbose) printf("\n");
      }
   }
}

static void rldi_cb (const char* name, test_func_t func_IN,
                     unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, k, arg_step;
   
   arg_step = (arg_list_size == 0) ? 7 : 3;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<64; j+=arg_step) {     // SH
         for (k=0; k<64; k+=arg_step) {  // MB|ME
            /* Patch up the instruction */
            func = init_function( func_IN, func_buf );
            _patch_op_imm(&func_buf[0], (j & 0x1F), 11, 5);
            _patch_op_imm(&func_buf[0], ((j>>5)&1), 1, 1);
            patch_op_imm(&func_buf[0], (((k & 0x1F)<<1) | ((k>>5)&1)), 5, 6);
            
            r14 = iargs[i];

            SET_CR_XER_ZERO;
            (*func)();
            GET_CR_XER(flags,xer);
            res = r17;

            printf("%s %016llx, %2d, %2d => %016llx (%08x %08x)\n",
                   name, iargs[i], j, k, res, flags, xer);
         }
         if (verbose) printf("\n");
      }
   }
}

static void sradi_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res;
   volatile uint32_t flags, xer;
   int i, j, arg_step;
   
   arg_step = (arg_list_size == 0) ? 7 : 3;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<64; j+=arg_step) {     // SH
         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         _patch_op_imm(&func_buf[0], (j & 0x1F), 11, 5);
         patch_op_imm(&func_buf[0], ((j>>5)&1), 1, 1);
            
         r14 = iargs[i];

         SET_CR_XER_ZERO;
         (*func)();
         GET_CR_XER(flags,xer);
         res = r17;

         printf("%s %016llx, %2d => %016llx (%08x %08x)\n",
                name, iargs[i], j, res, flags, xer);
      }
      if (verbose) printf("\n");
   }
}
#endif // #ifdef __powerpc64__


typedef struct special_t special_t;

struct special_t {
   const char *name;
   void (*test_cb)(const char* name, test_func_t func,
                   unused uint32_t test_flags);
};

static void test_special (special_t *table,
                          const char* name, test_func_t func,
                          unused uint32_t test_flags)
{
   const char *tmp;
   int i;
   
   for (tmp = name; isspace(*tmp); tmp++)
      continue;
   for (i=0; table[i].name != NULL; i++) {
#if 0
      fprintf(stderr, "look for handler for '%s' (%s)\n", name,
              table[i].name);
#endif
      if (strcmp(table[i].name, tmp) == 0) {
         (*table[i].test_cb)(name, func, test_flags);
         return;
      }
   }
   fprintf(stderr, "ERROR: no test found for op '%s'\n", name);
}

static special_t special_int_ops[] = {
   {
      "rlwimi", /* One register + 3 5 bits immediate arguments */
      &rlwi_cb,
   },
   {
      "rlwimi.", /* One register + 3 5 bits immediate arguments */
      &rlwi_cb,
   },
   {
      "rlwinm", /* One register + 3 5 bits immediate arguments */
      &rlwi_cb,
   },
   {
      "rlwinm.", /* One register + 3 5 bits immediate arguments */
      &rlwi_cb,
   },
   {
      "rlwnm",  /* Two registers + 2 5 bits immediate arguments */
      &rlwnm_cb,
   },
   {
      "rlwnm.",  /* Two registers + 2 5 bits immediate arguments */
      &rlwnm_cb,
   },
   {
      "srawi",  /* One register + 1 5 bits immediate arguments */
      &srawi_cb,
   },
   {
      "srawi.",  /* One register + 1 5 bits immediate arguments */
      &srawi_cb,
   },
   {
      "mcrf",  /* 2 3 bits immediate arguments */
      &mcrf_cb,
   },
#if 0
   {
      "mcrfs",  /* 2 3 bits immediate arguments */
      &mcrfs_cb,
   },
#endif
   {
      "mcrxr",  /* 1 3 bits immediate argument */
      &mcrxr_cb,
   },
   {
      "mfcr",  /* No arguments */
      &mfcr_cb,
   },
   {
      "mfspr",  /* 1 10 bits immediate argument */
      &mfspr_cb,
   },
#if 0
   {   // Move from time base
      "mftb",  /* 1 10 bits immediate arguments */
      &mftb_cb,
   },
#endif
   {
      "mtcrf",  /* One register + 1 8 bits immediate arguments */
      &mtcrf_cb,
   },
   {
      "mtspr",  /* One register + 1 10 bits immediate arguments */
      &mtspr_cb,
   },
#ifdef __powerpc64__
   {
      "rldcl",   /* Two registers + 1 6 bit immediate argument */
      &rldc_cb,
   },
   {
      "rldcl.",  /* Two registers + 1 6 bit immediate argument */
      &rldc_cb,
   },
   {
      "rldcr",   /* Two registers + 1 6 bit immediate argument */
      &rldc_cb,
   },
   {
      "rldcr.",  /* Two registers + 1 6 bit immediate argument */
      &rldc_cb,
   },
   {
      "rldic",   /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldic.",  /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldicl",  /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldicl.", /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldicr",  /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldicr.", /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldimi",  /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "rldimi.", /* One register + 2 6 bit immediate arguments */
      &rldi_cb,
   },
   {
      "sradi",  /* One register + 1 6 bit immediate argument */
      &sradi_cb,
   },
   {
      "sradi.", /* One register + 1 6 bit immediate argument */
      &sradi_cb,
   },
#endif // #ifdef __powerpc64__
   {
      NULL,
      NULL,
   },
};

static void test_int_special (const char* name, test_func_t func,
                              uint32_t test_flags)
{
   test_special(special_int_ops, name, func, test_flags);
}


static void test_int_ld_one_reg_imm16 (const char* name,
                                       test_func_t func_IN,
                                       unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile HWord_t res, base;
   volatile uint32_t flags, xer;
   int i, offs, shift_offset = 0;

#ifdef __powerpc64__
   if (strstr(name, "lwa") || strstr(name, "ld") || strstr(name, "ldu"))
      shift_offset = 1;
#endif

   // +ve d
   base = (HWord_t)&iargs[0];
   for (i=0; i<nb_iargs; i++) {
      offs = (i == 0) ? i : (i * sizeof(HWord_t)) - 1;

      /* Patch up the instruction */
      func = init_function( func_IN, func_buf );
      if (shift_offset)
         patch_op_imm(&func_buf[0], offs>>2, 2, 14);
      else
         patch_op_imm16(&func_buf[0], offs);

      r14 = base;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = r17;

#ifndef __powerpc64__
      printf("%s %2d, (%08x) => %08x, %2d (%08x %08x)\n",
#else
      printf("%s %3d, (%016llx) => %016llx, %3lld (%08x %08x)\n",
#endif
             name, offs, iargs[i], res, r14-base, flags, xer);
   }
   if (verbose) printf("\n");
   
   // -ve d
   base = (HWord_t)&iargs[nb_iargs-1];
   for (i = 0; i > -nb_iargs; i--) {
      offs = (i * sizeof(HWord_t)) + 1;

      /* Patch up the instruction */
      func = init_function( func, func_buf );
      patch_op_imm16(&func_buf[0], offs);

      r14 = base;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = r17;

#ifndef __powerpc64__
      printf("%s %2d, (%08x) => %08x, %2d (%08x %08x)\n",
#else
      printf("%s %3d, (%016llx) => %016llx, %3lld (%08x %08x)\n",
#endif
             name, offs, iargs[nb_iargs-1 + i], res, r14-base, flags, xer);
   }
}

static void test_int_ld_two_regs (const char* name,
                                  test_func_t func,
                                  unused uint32_t test_flags)
{
   volatile HWord_t res, base;
   volatile uint32_t flags, xer;
   int i, offs;
   
   // +ve d
   base = (HWord_t)&iargs[0];
   for (i=0; i<nb_iargs; i++) {
      offs = i * sizeof(HWord_t);
      r14 = base;
      r15 = offs;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = r17;

#ifndef __powerpc64__
      printf("%s %d (%08x) => %08x, %d (%08x %08x)\n",
#else
      printf("%s %3d, (%016llx) => %016llx, %2lld (%08x %08x)\n",
#endif
             name, offs, iargs[i], res, r14-base, flags, xer);
   }
}

static void test_int_st_two_regs_imm16 (const char* name,
                                        test_func_t func_IN,
                                        unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, xer;
   int i, offs, k;
   HWord_t *iargs_priv, base;

   // private iargs table to store to
   iargs_priv = malloc(nb_iargs * sizeof(HWord_t));
   
   // +ve d
   base = (HWord_t)&iargs_priv[0];
   for (i=0; i<nb_iargs; i++) {
      for (k=0; k<nb_iargs; k++)  // clear array
         iargs_priv[k] = 0;

      offs = i * sizeof(HWord_t);

      /* Patch up the instruction */
      func = init_function( func_IN, func_buf );
      patch_op_imm16(&func_buf[0], offs);

      r14 = iargs[i];             // read from iargs
      r15 = base;                 // store to r15 + offs

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %08x, %2d => %08x, %2d (%08x %08x)\n",
#else
      printf("%s %016llx, %3d => %016llx, %3lld (%08x %08x)\n",
#endif
             name, iargs[i], offs, iargs_priv[i], r15-base, flags, xer);
   }
   if (verbose) printf("\n");
   
   // -ve d
   base = (HWord_t)&iargs_priv[nb_iargs-1];
   for (i = -nb_iargs+1; i<=0; i++) {
      for (k=0; k<nb_iargs; k++)  // clear array
         iargs_priv[k] = 0;

      offs = i * sizeof(HWord_t);

      /* Patch up the instruction */
      func = init_function( func, func_buf );
      patch_op_imm16(&func_buf[0], offs);

      r14 = iargs[nb_iargs-1+i];  // read from iargs
      r15 = base;                 // store to r15 + offs

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %08x, %2d => %08x, %2d (%08x %08x)\n",
#else
      printf("%s %016llx, %3d => %016llx, %3lld (%08x %08x)\n",
#endif
             name, iargs[nb_iargs-1+i], offs, iargs_priv[nb_iargs-1+i],
             r15-base, flags, xer);
   }
   free(iargs_priv);
}

static void test_int_st_three_regs (const char* name,
                                    test_func_t func,
                                    unused uint32_t test_flags)
{
   volatile uint32_t flags, xer;
   int i, offs, k;
   HWord_t *iargs_priv, base;

   // private iargs table to store to
   iargs_priv = malloc(nb_iargs * sizeof(HWord_t));
   
   base = (HWord_t)&iargs_priv[0];
   for (i=0; i<nb_iargs; i++) {
      for (k=0; k<nb_iargs; k++)  // clear array
         iargs_priv[k] = 0;

      offs = i * sizeof(HWord_t);
      r14 = iargs[i];             // read from iargs
      r15 = base;                 // store to r15 + offs
      r16 = offs;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %08x, %d => %08x, %d (%08x %08x)\n",
#else
      printf("%s %016llx, %3d => %016llx, %2lld (%08x %08x)\n",
#endif
             name, iargs[i], offs, iargs_priv[i], r15-base, flags, xer);
   }
   free(iargs_priv);
}


/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/
static test_loop_t int_sh_algebraic[] = {
   &test_int_two_args,
   &test_int_special,
};

static test_loop_t int_mfspr[] = {
   &test_int_special,
};
 
 static test_loop_t int_loops[] = {
   &test_int_one_arg,
   &test_int_two_args,
   &test_int_three_args,
   &test_int_two_args,
   &test_int_one_reg_imm16,
   &test_int_one_reg_imm16,
   &test_int_special,
   &test_int_ld_one_reg_imm16,
   &test_int_ld_two_regs,
   &test_int_st_two_regs_imm16,
   &test_int_st_three_regs,
};

static void test_dcbt_ops (const char* name, test_func_t func,
                           unused uint32_t test_flags)
{
   unsigned long * data = (unsigned long *)malloc(4096 * sizeof(unsigned long));
   HWord_t base;

   base  = (HWord_t)data;
   size_t offs = 100 * sizeof(unsigned long);    // some arbitrary offset)

   r17  = base;
   r14  = offs;

   (*func)();

   printf("%s with various hint values completes with no exceptions\n", name);
   free(data);
}

static test_loop_t misc_loops[] = {
   &test_dcbt_ops,
};

#if !defined (NO_FLOAT)
static void test_float_three_args (const char* name, test_func_t func,
                                   unused uint32_t test_flags)
{
   double res;
   uint64_t u0, u1, u2, ur;
   volatile uint32_t flags;
   int i, j, k;

   /* Note: using nb_normal_fargs:
      - not testing special values for these insns
   */
   for (i=0; i<nb_normal_fargs; i+=3) {
      for (j=0; j<nb_normal_fargs; j+=5) {
         for (k=0; k<nb_normal_fargs; k+=7) {
            u0 = *(uint64_t *)(&fargs[i]);
            u1 = *(uint64_t *)(&fargs[j]);
            u2 = *(uint64_t *)(&fargs[k]);
            f14 = fargs[i];
            f15 = fargs[j];
            f16 = fargs[k];

            SET_FPSCR_ZERO;
            SET_CR_XER_ZERO;
            (*func)();
            GET_CR(flags);
            res = f17;
            ur = *(uint64_t *)(&res);

            /* Note: zapping the bottom byte of the result, 
               as vex's accuracy isn't perfect */
            ur &= 0xFFFFFFFFFFFFFF00ULL;

#ifndef __powerpc64__
            printf("%s %016llx, %016llx, %016llx => %016llx",
#else
            printf("%s %016llx, %016llx, %016llx => %016llx",
#endif
                   name, u0, u1, u2, ur);
#if defined TEST_FLOAT_FLAGS
            printf(" (%08x)", flags);
#endif
            printf("\n");
         }
         if (verbose) printf("\n");
      }
   }
}

static void test_float_two_args (const char* name, test_func_t func,
                                 unused uint32_t test_flags)
{
   double res;
   uint64_t u0, u1, ur;
   volatile uint32_t flags;
   int i, j;
   
   for (i=0; i<nb_fargs; i+=3) {
      for (j=0; j<nb_fargs; j+=5) {
         u0 = *(uint64_t *)(&fargs[i]);
         u1 = *(uint64_t *)(&fargs[j]);
         f14 = fargs[i];
         f15 = fargs[j];

         SET_FPSCR_ZERO;
         SET_CR_XER_ZERO;
         (*func)();
         GET_CR(flags);
         res = f17;
         ur = *(uint64_t *)(&res);

#ifndef __powerpc64__
         printf("%s %016llx, %016llx => %016llx",
#else
         printf("%s %016llx, %016llx => %016llx",
#endif
                name, u0, u1, ur);
#if defined TEST_FLOAT_FLAGS
         printf(" (%08x)", flags);
#endif
         printf("\n");
      }
      if (verbose) printf("\n");
   }
}

static void test_float_one_arg (const char* name, test_func_t func,
                                unused uint32_t test_flags)
{
   double res;
   uint64_t u0, ur;
   volatile uint32_t flags;
   int i;
   unsigned zap_hi_32bits, zap_lo_44bits, zap_lo_47bits;

   /* if we're testing fctiw or fctiwz, zap the hi 32bits,
      as they're undefined */
   zap_hi_32bits = strstr(name, " fctiw")    != NULL  ? 1 : 0;
   zap_lo_44bits = strstr(name, " fres")     != NULL  ? 1 : 0;
   zap_lo_47bits = strstr(name, " frsqrte")  != NULL  ? 1 : 0;

   assert(zap_hi_32bits + zap_lo_44bits + zap_lo_47bits <= 1);

   for (i=0; i<nb_fargs; i++) {
      u0 = *(uint64_t *)(&fargs[i]);
      f14 = fargs[i];

       SET_FPSCR_ZERO;
       SET_CR_XER_ZERO;
       (*func)();
       GET_CR(flags);
       res = f17;
       ur = *(uint64_t *)(&res);

       if (strstr(name, " frsqrte") !=  NULL)
          /* The 32-bit frsqrte instruction is the Floatig Reciprical Square
           * Root Estimate instruction.  The precision of the estimate will
           * vary from Proceesor implementation.  The approximation varies in
           * bottom two bytes of the 32-bit result.
           */
           ur &= 0xFFFF000000000000ULL;

      if (zap_hi_32bits)
         ur &= 0x00000000FFFFFFFFULL;
      if (zap_lo_44bits)
         ur &= 0xFFFFF00000000000ULL;
      if (zap_lo_47bits)
         ur &= 0xFFFF800000000000ULL;

#ifndef __powerpc64__
      printf("%s %016llx => %016llx",
#else
      printf("%s %016llx => %016llx",
#endif
             name, u0, ur);
#if defined TEST_FLOAT_FLAGS
      printf(" (%08x)", flags);
#endif
      printf("\n");
    }
}

/* Special test cases for:
 * mffs
 * mtfsb0
 * mtfsb1
 */
static special_t special_float_ops[] = {
#if 0
   {
      "mffs",   /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mffs.",   /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mtfsb0", /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mtfsb0.", /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mtfsb1", /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mtfsb1.", /* One 5 bits immediate argument */
      &mffs_cb,
   },
   {
      "mtfsf",  /* One register + 1 8 bits immediate argument */
      &mtfsf_cb,
   },
   {
      "mtfsf.",  /* One register + 1 8 bits immediate argument */
      &mtfsf_cb,
   },
   {
      "mtfsfi", /* One 5 bits argument + 1 5 bits argument */
      &mtfsfi_cb,
   },
   {
      "mtfsfi.", /* One 5 bits argument + 1 5 bits argument */
      &mtfsfi_cb,
   },
#endif
   {
      NULL,
      NULL,
   },
};

static void test_float_special (const char* name, test_func_t func,
                                uint32_t test_flags)
{
   test_special(special_float_ops, name, func, test_flags);
}


static void test_float_ld_one_reg_imm16 (const char* name,
                                         test_func_t func_IN,
                                         unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   HWord_t base;
   volatile uint32_t flags, xer;
   volatile double src, res;
   int i;
   uint16_t offs;

   /* offset within [1-nb_fargs:nb_fargs] */
   for (i=1-nb_fargs; i<nb_fargs; i++) {
      offs = i * 8;      // offset = i * sizeof(double)
      if (i < 0) {
         src  = fargs[nb_fargs-1 + i];
         base = (HWord_t)&fargs[nb_fargs-1];
      } else {
         src = fargs[i];
         base = (HWord_t)&fargs[0];
      }

      /* Patch up the instruction */
      func = init_function( func_IN, func_buf );
      patch_op_imm16(&func_buf[0], offs);

      // load from fargs[idx] => r14 + offs
      r14 = base;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = f17;

#ifndef __powerpc64__
      printf("%s %016llx, %4d => %016llx, %4d",
#else
      printf("%s %016llx, %4d => %016llx, %4lld",
#endif
             name, double_to_bits(src), offs,
             double_to_bits(res), r14-base);
#if defined TEST_FLOAT_FLAGS
      printf(" (%08x %08x)", flags, xer);
#endif
      printf("\n");
   }
   if (verbose) printf("\n");
}

static void test_float_ld_two_regs (const char* name,
                                    test_func_t func,
                                    unused uint32_t test_flags)
{
   volatile HWord_t base;
   volatile uint32_t flags, xer;
   volatile double src, res;
   int i, offs;

   /* offset within [1-nb_fargs:nb_fargs] */
   for (i=1-nb_fargs; i<nb_fargs; i++) {
      offs = i * 8;                // offset = i * sizeof(double)
      if (i < 0) {                 // base reg = start of array
         src  = fargs[nb_fargs-1 + i];
         base = (HWord_t)&fargs[nb_fargs-1];
      } else {
         src  = fargs[i];
         base = (HWord_t)&fargs[0];
      }

      r14 = base;
      r15 = offs;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);
      res = f17;

#ifndef __powerpc64__
      printf("%s %016llx, %4d => %016llx, %4d",
#else
      printf("%s %016llx, %4lld => %016llx, %4lld",
#endif
             name, double_to_bits(src), r15/*offs*/,
             double_to_bits(res), r14-base);
#if defined TEST_FLOAT_FLAGS
      printf(" (%08x %08x)", flags, xer);
#endif
      printf("\n");
   }
}

static void test_float_st_two_regs_imm16 (const char* name,
                                          test_func_t func_IN,
                                          unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   HWord_t base;
   volatile uint32_t flags, xer;
   double src, *p_dst;
   int i, offs;
   double *fargs_priv;
   int nb_tmp_fargs = nb_fargs;


   /* if we're storing an fp single-precision, don't want nans
      - the vex implementation doesn't like them (yet)
      Note: This is actually a bigger problem: the vex implementation
      rounds these insns twice.  This leads to many rounding errors.
      For the small fargs set, however, this doesn't show up.
   */
   if (strstr(name, "stfs") != NULL)
      nb_tmp_fargs = nb_normal_fargs;


   // private fargs table to store to
   fargs_priv = malloc(nb_tmp_fargs * sizeof(double));
   
   /* offset within [1-nb_tmp_fargs:nb_tmp_fargs] */
   for (i=1-nb_tmp_fargs; i<nb_tmp_fargs; i++) {
      offs = i * 8;    // offset = i * sizeof(double)
      if (i < 0) {
         src   =  fargs     [nb_tmp_fargs-1 + i];
         p_dst = &fargs_priv[nb_tmp_fargs-1 + i];
         base  = (HWord_t)&fargs_priv[nb_tmp_fargs-1];
      } else {
         src   =  fargs     [i];
         p_dst = &fargs_priv[i];
         base  = (HWord_t)&fargs_priv[0];
      }
      *p_dst = 0;  // clear dst

      /* Patch up the instruction */
      func = init_function( func_IN, func_buf );
      patch_op_imm16(&func_buf[0], offs);

      // read from fargs[idx] => f14
      // store to fargs_priv[idx] => r15 + offs
      f14 = src;
      r15 = base;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %016llx, %4d => %016llx, %4d",
#else
      printf("%s %016llx, %4d => %016llx, %4lld",
#endif
             name, double_to_bits(src), offs,
             double_to_bits(*p_dst), r15-base);
#if defined TEST_FLOAT_FLAGS
      printf(" (%08x %08x)", flags, xer);
#endif
      printf("\n");
   }
   free(fargs_priv);
}

static void test_float_st_three_regs (const char* name,
                                      test_func_t func,
                                      unused uint32_t test_flags)
{
   volatile HWord_t base;
   volatile uint32_t flags, xer;
   double src, *p_dst;
   int i, offs;
   double *fargs_priv;
   int nb_tmp_fargs = nb_fargs;


   /* if we're storing an fp single-precision, don't want nans
      - the vex implementation doesn't like them (yet)
      Note: This is actually a bigger problem: the vex implementation
      rounds these insns twice.  This leads to many rounding errors.
      For the small fargs set, however, this doesn't show up.
   */
   if (strstr(name, "stfs") != NULL)  // stfs(u)(x)
      nb_tmp_fargs = nb_normal_fargs;


   // private fargs table to store to
   fargs_priv = malloc(nb_tmp_fargs * sizeof(double));
   
   //   /* offset within [1-nb_tmp_fargs:nb_tmp_fargs] */
   //   for (i=1-nb_tmp_fargs; i<nb_tmp_fargs; i++) {
   for (i=0; i<nb_tmp_fargs; i++) {
      offs = i * 8;    // offset = i * sizeof(double)
      if (i < 0) {
         src   =  fargs     [nb_tmp_fargs-1 + i];
         p_dst = &fargs_priv[nb_tmp_fargs-1 + i];
         base  = (HWord_t)&fargs_priv[nb_tmp_fargs-1];
      } else {
         src   =  fargs     [i];
         p_dst = &fargs_priv[i];
         base  = (HWord_t)&fargs_priv[0];
      }
      *p_dst = 0;  // clear dst

      f14  = src;    // read from fargs
      r15  = base;   // store to r15 + offs
      r16  = offs;

      SET_CR_XER_ZERO;
      (*func)();
      GET_CR_XER(flags,xer);

#ifndef __powerpc64__
      printf("%s %016llx, %4d => %016llx, %4d",
#else
      printf("%s %016llx, %4lld => %016llx, %4lld",
#endif
             name, double_to_bits(src), r16/*offs*/,
             double_to_bits(*p_dst), r15-base);
#if defined TEST_FLOAT_FLAGS
      printf(" (%08x %08x)", flags, xer);
#endif
      printf("\n");


#if 0
      // print double precision result
#ifndef __powerpc64__
      printf("%s %016llx (%014e), %4d => %016llx (%014e), %08x (%08x %08x)\n",
#else
      printf("%s %016llx (%014e), %4d => %016llx (%014e), %08x (%08x %08x)\n",
#endif
             name, double_to_bits(src), src, offs,
             double_to_bits(*p_dst), *p_dst, r15, flags, xer);

      // print single precision result
#ifndef __powerpc64__
      printf("%s %016llx (%014e), %4d => %08x (%f), %08x (%08x %08x)\n",
#else
      printf("%s %016llx (%014e), %4d => %08x (%f), %08x (%08x %08x)\n",
#endif
             name, double_to_bits(src), src, offs,
             (uint32_t)(double_to_bits(*p_dst) >> 32),
             bits_to_float( (uint32_t)(double_to_bits(*p_dst) >> 32) ),
             r15, flags, xer);
#endif
   }
   free(fargs_priv);
}


/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/
static test_loop_t float_loops[] = {
   &test_float_one_arg,
   &test_float_two_args,
   &test_float_three_args,
   &test_float_two_args,
   NULL,
   NULL,
   &test_float_special,
   &test_float_ld_one_reg_imm16,
   &test_float_ld_two_regs,
   &test_float_st_two_regs_imm16,
   &test_float_st_three_regs,
};
#endif /* !defined (NO_FLOAT) */


#if defined (HAS_ALTIVEC)

/* Ref: vector insns to test setting CR, VSCR:
         volatile vector unsigned int v1 =
            //            (vector unsigned int){ 0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF };
            (vector unsigned int){ 0x80808080,0x80808080,0x80808080,0x80808080 };
         volatile vector unsigned int v2 =
            //            (vector unsigned int){ 0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF };
            (vector unsigned int){ 0x01010101,0x01010101,0x01010101,0x01010101 };
         //__asm__ __volatile__ ("vcmpequw. 31,%0,%1" : : "v" (v1), "v" (v2));   // sets CR[6]
         //__asm__ __volatile__ ("vpkswss 31,%0,%1" : : "v" (v1), "v" (v2));     // sets VSCR[SAT]
         __asm__ __volatile__ ("vsubsbs 31,%0,%1" : : "v" (v1), "v" (v2));       // sets VSCR[SAT]
*/

//#define DEFAULT_VSCR 0x00010000
#define DEFAULT_VSCR 0x0

static void test_av_int_one_arg (const char* name, test_func_t func,
                                 unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in, vec_out, vscr;
   unsigned int *src, *dst;
   int i;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      /* Save flags */
      __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

      vec_in  = (vector unsigned int)viargs[i];
      vec_out = (vector unsigned int){ 0,0,0,0 };
      
      // reset VSCR and CR
      vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
      flags = 0;
      __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
      __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

      // load input -> r14
      __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in));
      
      // do stuff
      (*func)();
      
      // retrieve output <- r17
      __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));

      // get CR,VSCR flags
      __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
      
      /* Restore flags */
      __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
      __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

      src = (unsigned int*)&vec_in;
      dst = (unsigned int*)&vec_out;

      printf("%s: %08x %08x %08x %08x\n", name,
             src[0], src[1], src[2], src[3]);
      printf("%s:  => %08x %08x %08x %08x ", name,
             dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
      p_vscr = (unsigned int*)&vscr;
      printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
      printf("(%08x)\n", flags);
#endif
   }
}

static void test_av_int_two_args (const char* name, test_func_t func,
                                  unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in1, vec_in2, vec_out, vscr;
   unsigned int *src1, *src2, *dst;
   int i,j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      vec_in1 = (vector unsigned int)viargs[i];
      for (j=0; j<nb_viargs; j++) {
         vec_in2 = (vector unsigned int)viargs[j];
         vec_out = (vector unsigned int){ 0,0,0,0 };
         
         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

         // load inputs -> r14,r15
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
         __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_in2));
         
         // do stuff
         (*func)();

         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

         src1 = (unsigned int*)&vec_in1;
         src2 = (unsigned int*)&vec_in2;
         dst  = (unsigned int*)&vec_out;

         printf("%s: ", name);
         printf("%08x%08x%08x%08x, ", src1[0], src1[1], src1[2], src1[3]);
         printf("%08x%08x%08x%08x\n", src2[0], src2[1], src2[2], src2[3]);
         printf("%s:  => %08x %08x %08x %08x ", name,
                dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
         p_vscr = (unsigned int*)&vscr;
         printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
         printf("(%08x)\n", flags);
#endif
      }
      if (verbose) printf("\n");
   }
}

static void test_av_int_three_args (const char* name, test_func_t func,
                                    unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in1, vec_in2, vec_in3, vec_out, vscr;
   unsigned int *src1, *src2, *src3, *dst;
   int i,j,k;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      vec_in1 = (vector unsigned int)viargs[i];
      for (j=0; j<nb_viargs; j++) {
         vec_in2 = (vector unsigned int)viargs[j];
         for (k=0; k<nb_viargs; k++) {
            vec_in3 = (vector unsigned int)viargs[k];
            vec_out = (vector unsigned int){ 0,0,0,0 };
            
            /* Save flags */
            __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

            // reset VSCR and CR
            vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
            flags = 0;
            __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
            __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
            
            // load inputs -> r14,r15,r16
            __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
            __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_in2));
            __asm__ __volatile__ ("vor 16,%0,%0" : : "v" (vec_in3));
            
            // do stuff
            (*func)();
            
            // retrieve output <- r17
            __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
            
            // get CR,VSCR flags
            __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
            
            /* Restore flags */
            __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
            __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

            src1 = (unsigned int*)&vec_in1;
            src2 = (unsigned int*)&vec_in2;
            src3 = (unsigned int*)&vec_in3;
            dst  = (unsigned int*)&vec_out;

            printf("%s: %08x%08x%08x%08x, %08x%08x%08x%08x, %08x%08x%08x%08x\n", name,
                   src1[0], src1[1], src1[2], src1[3],
                   src2[0], src2[1], src2[2], src2[3],
                   src3[0], src3[1], src3[2], src3[3]);

            printf("%s:  => %08x%08x%08x%08x ", name,
                   dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
            p_vscr = (unsigned int*)&vscr;
            printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
            printf("(%08x)\n", flags);
#endif
         }
         if (verbose) printf("\n");
      }
   }
}


static void vs128_cb (const char* name, test_func_t func,
                      unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned char vec_shft;
   volatile vector unsigned int vec_in1, vec_out, vscr;
   unsigned int *src1, *src2, *dst;
   int i,j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      vec_in1 = (vector unsigned int)viargs[i];
      for (j=0; j<8; j++) {
         /* low-order 3bits of every byte must be the same for the shift vector */
         vec_shft = (vector unsigned char) { j,j,j,j, j,j,j,j, j,j,j,j, j,j,j,j };
         vec_out  = (vector unsigned int){ 0,0,0,0 };
         
         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
         
         // load inputs -> r14,r15
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
         __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_shft));
         
         // do stuff
         (*func)();
         
         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

         src1 = (unsigned int*)&vec_in1;
         src2 = (unsigned int*)&vec_shft;
         dst  = (unsigned int*)&vec_out;

         printf("%s: ", name);
         printf("%08x%08x%08x%08x, ", src1[0], src1[1], src1[2], src1[3]);
         printf("%08x%08x%08x%08x\n", src2[0], src2[1], src2[2], src2[3]);

         printf("%s:  => %08x %08x %08x %08x ", name,
                dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
         p_vscr = (unsigned int*)&vscr;
         printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
         printf("(%08x)\n", flags);
#endif
      }
      if (verbose) printf("\n");
   }
}

static void vsplt_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in1, vec_out, vscr;
   unsigned int *src1, *dst;
   int i,j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      vec_in1 = (vector unsigned int)viargs[i];

      for (j=0; j<16; j+=3) {
         vec_out = (vector unsigned int){ 0,0,0,0 };

         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm(&func_buf[0], j, 16, 5);

         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
         
         // load input -> r14
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
         
         // do stuff
         (*func)();
         
         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

         src1 = (unsigned int*)&vec_in1;
         dst  = (unsigned int*)&vec_out;

         printf("%s: ", name);
         printf("%08x %08x %08x %08x, %u\n", src1[0], src1[1], src1[2], src1[3], j);

         printf("%s:  => %08x %08x %08x %08x ", name,
                dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
         p_vscr = (unsigned int*)&vscr;
         printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
         printf("(%08x)\n", flags);
#endif
      }
      if (verbose) printf("\n");
   }
}

static void vspltis_cb (const char* name, test_func_t func_IN,
                      unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_out, vscr;
   unsigned int *dst;
   int i;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<32; i++) {
      vec_out = (vector unsigned int){ 0,0,0,0 };
      
      /* Patch up the instruction */
      func = init_function( func_IN, func_buf );
      patch_op_imm(&func_buf[0], i, 16, 5);
      
      /* Save flags */
      __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));
      
      // reset VSCR and CR
      vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
      flags = 0;
      __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
      __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
      
      // do stuff
      (*func)();
      
      // retrieve output <- r17
      __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
      
      // get CR,VSCR flags
      __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
      
      /* Restore flags */
      __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
      __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));
      
      dst = (unsigned int*)&vec_out;

      printf("%s: %2d => ", name, i);
      printf("%08x %08x %08x %08x ", dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
      p_vscr = (unsigned int*)&vscr;
      printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
      printf("(%08x)\n", flags);
#endif
   }
}

static void vsldoi_cb (const char* name, test_func_t func_IN,
                       unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in1, vec_in2, vec_out, vscr;
   unsigned int *src1, *src2, *dst;
   int i,j,k;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_viargs; i++) {
      vec_in1 = (vector unsigned int)viargs[i];
      for (j=0; j<nb_viargs; j++) {
         vec_in2 = (vector unsigned int)viargs[j];
         for (k=0; k<16; k+=14) {
            vec_out = (vector unsigned int){ 0,0,0,0 };

            /* Patch up the instruction */
            func = init_function( func_IN, func_buf );
            patch_op_imm(&func_buf[0], k, 6, 4);
            
            /* Save flags */
            __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));
            
            // reset VSCR and CR
            vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
            flags = 0;
            __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
            __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
            
            // load inputs -> r14,r15
            __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
            __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_in2));
            
            // do stuff
            (*func)();
         
            // retrieve output <- r17
            __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
            
            // get CR,VSCR flags
            __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
            
            /* Restore flags */
            __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
            __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));
            
            src1   = (unsigned int*)&vec_in1;
            src2   = (unsigned int*)&vec_in2;
            dst    = (unsigned int*)&vec_out;

            printf("%s: ", name);
            printf("%08x%08x%08x%08x, %08x%08x%08x%08x, %u\n",
                   src1[0], src1[1], src1[2], src1[3],
                   src2[0], src2[1], src2[2], src2[3], k);

            printf("%s:  => %08x %08x %08x %08x] ", name,
                   dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
            p_vscr = (unsigned int*)&vscr;
            printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
            printf("(%08x)\n", flags);
#endif
         }
         if (verbose) printf("\n");
      }
   }
}

/* lvsl, lvsr */
static void lvs_cb (const char *name, test_func_t func,
                    unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_out, vscr;
   unsigned shift;
   unsigned char * dst;
   int i, j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif
   
   for (i=-1; i<17; i++) {
      vec_out = (vector unsigned int){ 0,0,0,0 };
      
      // make sure start address is 16 aligned - use viargs[0]
      HWord_t * r15_in_ptr = (HWord_t *)&viargs[0];
      r15 = *r15_in_ptr;
      r14 = i;

      /* Save flags */
      __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));
      
      // reset VSCR and CR
      vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
      flags = 0;
      __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
      __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));         
      
      // do stuff
      (*func)();
      
      // retrieve output <- r17
      __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
      // get CR,VSCR flags
      __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
      
      /* Restore flags */
      __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
      __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));
      
      dst = (unsigned char*)&vec_out;

      shift = ((unsigned int)i + *r15_in_ptr) & 0xf;
      printf("%s %x, %3d", name, shift, 0);
      printf(" => 0x");
      for (j = 0; j < 16; j++) {
         printf("%02x", dst[j]);
         if (j == 7)
            printf(" 0x");
      }

      printf(" (%08x)\n", flags);
   }
   if (verbose) printf("\n");
}

static special_t special_av_int_ops[] = {
   {
      "vsr", /* Two registers arguments */
      &vs128_cb,
   },
   {
      "vsl", /* Two registers arguments */
      &vs128_cb,
   },
   {
      "vspltb", /* One reg, one 5-bit uimm arguments */
      &vsplt_cb,
   },
   {
      "vsplth", /* One reg, one 5-bit uimm arguments */
      &vsplt_cb,
   },
   {
      "vspltw", /* One reg, one 5-bit uimm arguments */
      &vsplt_cb,
   },
   {
      "vspltisb", /* One reg, one 5-bit uimm arguments */
      &vspltis_cb,
   },
   {
      "vspltish", /* One reg, one 5-bit uimm arguments */
      &vspltis_cb,
   },
   {
      "vspltisw", /* One reg, one 5-bit uimm arguments */
      &vspltis_cb,
   },
   {
      "vsldoi", /* Two regs, one 4-bit uimm arguments */
      &vsldoi_cb,
   },
   {
      "lvsl", /* Two regs */
      &lvs_cb,
   },
   {
      "lvsr", /* Two regs */
      &lvs_cb,
   },
   {
      NULL,
      NULL,
   },
};

static void test_av_int_special (const char* name, test_func_t func,
                                 uint32_t test_flags)
{
   test_special(special_av_int_ops, name, func, test_flags);
}

static void test_av_int_ld_two_regs (const char *name,
                                  test_func_t func,
                                  unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in, vec_out, vscr;
   unsigned int *src, *dst;
   int i,j, k, do_mask;

   do_mask = 0;
   if (strstr(name, "lvebx") != NULL) do_mask = 1;
   if (strstr(name, "lvehx") != NULL) do_mask = 2;
   if (strstr(name, "lvewx") != NULL) do_mask = 4;

   for (i=0; i<nb_viargs; i++) {
      for (j=0; j<16; j+=7) {
         vec_out = (vector unsigned int){ 0,0,0,0 };

         // load from viargs array + some dis-alignment
         r15 = (HWord_t)&viargs[0];
         r14 = i*16 + j;
         
         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));
         
         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

         // do stuff
         (*func)();
         
         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));
         
         vec_in = (vector unsigned int)viargs[i];
         src = (unsigned int*)&vec_in;
         dst = (unsigned int*)&vec_out;

         /* For lvebx/lvehx/lvewx, as per the documentation, all of
            the dest reg except the loaded bits are undefined
            afterwards.  And different CPUs really do produce
            different results.  So mask out bits of the result that
            are undefined so as to make the test work reliably. */
         if (do_mask == 1) {
            char* p = (char*)dst;
            for (k = 0; k < 16; k++)
               if (k != j)
                  p[k] = (char)0;
         }
         if (do_mask == 2) {
            short* p = (short*)dst;
            for (k = 0; k < 8; k++)
               if (k != (j>>1))
                  p[k] = (short)0;
         }
         if (do_mask == 4) {
            int* p = (int*)dst;
            for (k = 0; k < 4; k++)
               if (k != (j>>2))
                  p[k] = (int)0;
         }

         printf("%s %3d, %08x %08x %08x %08x", name, j, src[0], src[1], src[2], src[3]);
         printf(" => %08x %08x %08x %08x ", dst[0], dst[1], dst[2], dst[3]);
         printf("(%08x)\n", flags);
      }
      if (verbose) printf("\n");
   }
}


static void test_av_int_st_three_regs (const char *name,
                                       test_func_t func,
                                       unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in, vec_out, vscr;
   unsigned int *src, *dst;
   int i,j;
   vector unsigned int* viargs_priv;

   // private viargs table to store to
   viargs_priv = memalign16(nb_viargs * sizeof(vector unsigned int));
   for (i=0; i<nb_viargs; i++)
      viargs_priv[i] = (vector unsigned int) { 0,0,0,0 };

   for (i=0; i<nb_viargs; i++) {
      for (j=0; j<16; j+=7) {
         // read from viargs
         vec_in = (vector unsigned int)viargs[i];

         // store to viargs_priv[0] + some dis-alignment
         r16 = (HWord_t)&viargs_priv[0];
         r15 = i*16 + j;

         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));
         
         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

         // load inputs -> r14
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in));
         
         // do stuff
         (*func)();

         // Output stored in viargs_priv
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));
         
         vec_out = (vector unsigned int)viargs_priv[i];
         src = (unsigned int*)&vec_in;
         dst = (unsigned int*)&vec_out;

         printf("%s %3d, %08x %08x %08x %08x", name, j, src[0], src[1], src[2], src[3]);
         printf(" => %08x %08x %08x %08x ", dst[0], dst[1], dst[2], dst[3]);
         printf("(%08x)\n", flags);
      }
      if (verbose) printf("\n");
   }
}

/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/
static test_loop_t altivec_int_loops[] = {
   &test_av_int_one_arg,
   &test_av_int_two_args,
   &test_av_int_three_args,
   &test_av_int_two_args,
   NULL,
   NULL,
   &test_av_int_special,
   NULL,
   &test_av_int_ld_two_regs,
   NULL,
   test_av_int_st_three_regs,
};


static void test_av_float_one_arg (const char* name, test_func_t func,
                                   unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector float vec_in, vec_out;
   volatile vector unsigned int vscr;
   unsigned int *src, *dst;
   int i;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   /* if we're doing an estimation operation, arrange to zap the
      bottom 10-bits of the result as it's basically garbage, and differs
      between cpus */
   unsigned int mask
      = (strstr(name,"vrsqrtefp") != NULL ||
         strstr(name,    "vrefp") != NULL)
           ? 0xFFFFC000 : 0xFFFFFFFF;

   for (i=0; i<nb_vfargs; i++) {
      vec_in  = (vector float)vfargs[i];
      vec_out = (vector float){ 0.0, 0.0, 0.0, 0.0 };
      
      /* Save flags */
      __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

      // reset VSCR and CR
      vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
      flags = 0;
      __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
      __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
      
      // load input -> r14
      __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in));
      
      // do stuff
      (*func)();
      
      // retrieve output <- r17
      __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
      
      // get CR,VSCR flags
      __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
      __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
      
      /* Restore flags */
      __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
      __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

      src = (unsigned int*)&vec_in;
      dst = (unsigned int*)&vec_out;

      printf("%s: %08x %08x %08x %08x\n", name,
             src[0], src[1], src[2], src[3]);
      printf("%s:  => %08x %08x %08x %08x ", name,
             dst[0] & mask, dst[1] & mask, dst[2] & mask, dst[3] & mask);
#if defined TEST_VSCR_SAT
      p_vscr = (unsigned int*)&vscr;
      printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
      printf("(%08x)\n", flags);
#endif
   }
}

static void test_av_float_two_args (const char* name, test_func_t func,
                                    unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector float vec_in1, vec_in2, vec_out;
   volatile vector unsigned int vscr;
   unsigned int *src1, *src2, *dst;
   int i,j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_vfargs; i++) {
      for (j=0; j<nb_vfargs; j+=3) {
         vec_in1 = (vector float)vfargs[i];
         vec_in2 = (vector float)vfargs[j];
         vec_out = (vector float){ 0.0, 0.0, 0.0, 0.0 };

         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

         // load inputs -> r14,r15
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
         __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_in2));

         // do stuff
         (*func)();

         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));

         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));

         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

         src1 = (unsigned int*)&vec_in1;
         src2 = (unsigned int*)&vec_in2;
         dst  = (unsigned int*)&vec_out;

         printf("%s: %08x%08x%08x%08x, %08x%08x%08x%08x\n", name,
                src1[0], src1[1], src1[2], src1[3],
                src2[0], src2[1], src2[2], src2[3]);
         printf("%s:  => %08x %08x %08x %08x ", name,
                dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
         p_vscr = (unsigned int*)&vscr;
         printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
         printf("(%08x)\n", flags);
#endif
      }
      if (verbose) printf("\n");
   }
}

static void test_av_float_three_args (const char* name, test_func_t func,
                                      unused uint32_t test_flags)
{
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector float vec_in1, vec_in2, vec_in3, vec_out;
   volatile vector unsigned int vscr;
   unsigned int *src1, *src2, *src3, *dst;
   int i,j,k,n;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_vfargs; i++) {
      for (j=0; j<nb_vfargs; j+=3) {
         for (k=0; k<nb_vfargs; k+=5) {
            vec_in1 = (vector float)vfargs[i];
            vec_in2 = (vector float)vfargs[j];
            vec_in3 = (vector float)vfargs[k];
            vec_out = (vector float){ 0.0, 0.0, 0.0, 0.0 };
            
            /* Save flags */
            __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

            // reset VSCR and CR
            vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
            flags = 0;
            __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
            __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));

            // load inputs -> r14,r15,r16
            __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in1));
            __asm__ __volatile__ ("vor 15,%0,%0" : : "v" (vec_in2));
            __asm__ __volatile__ ("vor 16,%0,%0" : : "v" (vec_in3));

            // do stuff
            (*func)();

            // retrieve output <- r17
            __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));

            // get CR,VSCR flags
            __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
            __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));

            /* Restore flags */
            __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
            __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

            src1 = (unsigned int*)&vec_in1;
            src2 = (unsigned int*)&vec_in2;
            src3 = (unsigned int*)&vec_in3;
            dst  = (unsigned int*)&vec_out;

            /* Valgrind emulation for vmaddfp and vnmsubfp generates negative 
             * NAN.  Technically, NAN is not positive or negative so mask off
             * the sign bit to eliminate false errors.  The lower 22-bits of
             * the 23-bit significand are a don't care for a NAN.  Mask them off.
             * 
             * Valgrind emulation is creating negative zero.  Mask off negative
             * from zero result.
             * 
             * These are only an issue as we are printing the result in hex.
             *
             * The VEX emulation accuracy for the vmaddfp and vnmsubfp 
             * instructions is off by a single bit in the least significant 
             * bit position of the result.  Mask off the LSB.
             */

             for (n=0; n<4; n++) {
                /* NAN result*/
                if (((dst[n] & 0x7F800000) == 0x7F800000) &&
                   ((dst[n] & 0x7FFFFF) != 0))
                   dst[n] &= 0x7FC00000;

                /* Negative zero result */
                else if (dst[n] == 0x80000000)
                    dst[n] = 0x0;

                else
                    /* The actual result and the emulated result for the
                     * vmaddfp and vnmsubfp instructions sometimes differ 
                     * in the least significant bit.  Mask off the bit.
                     */
                    dst[n] &= 0xFFFFFFFE;
                }

            printf("%s: %08x%08x%08x%08x, %08x%08x%08x%08x, %08x%08x%08x%08x\n", name,
                   src1[0], src1[1], src1[2], src1[3],
                   src2[0], src2[1], src2[2], src2[3],
                   src3[0], src3[1], src3[2], src3[3]);
            printf("%s:  => %08x %08x %08x %08x ", name,
                   dst[0], dst[1], dst[2], dst[3]);
#if defined TEST_VSCR_SAT
            p_vscr = (unsigned int*)&vscr;
            printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
            printf("(%08x)\n", flags);
#endif
         }
         if (verbose) printf("\n");
      }
   }
}

static void vcvt_cb (const char* name, test_func_t func_IN,
                     unused uint32_t test_flags)
{
   volatile test_func_t func;
   uint32_t* func_buf = get_rwx_area();
   volatile uint32_t flags, tmpcr;
   volatile vector unsigned int tmpvscr;
   volatile vector unsigned int vec_in, vec_out, vscr;
   unsigned int *src, *dst;
   int i,j;
#if defined TEST_VSCR_SAT
   unsigned int* p_vscr;
#endif

   for (i=0; i<nb_vfargs; i++) {
      vec_in = (vector unsigned int)vfargs[i];

      for (j=0; j<32; j+=9) {
         vec_out = (vector unsigned int){ 0,0,0,0 };

         /* Patch up the instruction */
         func = init_function( func_IN, func_buf );
         patch_op_imm(&func_buf[0], j, 16, 5);
         
         /* Save flags */
         __asm__ __volatile__ ("mfcr   %0" : "=r"  (tmpcr));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (tmpvscr));

         // reset VSCR and CR
         vscr = (vector unsigned int){ 0,0,0,DEFAULT_VSCR };
         flags = 0;
         __asm__ __volatile__ ("mtvscr %0" : : "v" (vscr) );
         __asm__ __volatile__ ("mtcr   %0" : : "r" (flags));
         
         // load input -> r14
         __asm__ __volatile__ ("vor 14,%0,%0" : : "v" (vec_in));
         
         // do stuff
         (*func)();
         
         // retrieve output <- r17
         __asm__ __volatile__ ("vor %0,17,17" : "=v" (vec_out));
         
         // get CR,VSCR flags
         __asm__ __volatile__ ("mfcr   %0" : "=r" (flags));
         __asm__ __volatile__ ("mfvscr %0" : "=v" (vscr));
         
         /* Restore flags */
         __asm__ __volatile__ ("mtcr   %0" : : "r"  (tmpcr));
         __asm__ __volatile__ ("mtvscr %0" : : "v" (tmpvscr));

         src = (unsigned int*)&vec_in;
         dst = (unsigned int*)&vec_out;

         printf("%s: %08x (%13e), %2u", name, src[0], *(float*)(&src[0]), j);
         printf(" => %08x (%13e) ", dst[0], *(float*)(&dst[0]));
//         printf(" => %08x ", dst[0]);
#if defined TEST_VSCR_SAT
            p_vscr = (unsigned int*)&vscr;
            printf("(%08x, %08x)\n", flags, p_vscr[3]);
#else
            printf("(%08x)\n", flags);
#endif
      }
      if (verbose) printf("\n");
   }
}

static special_t special_av_float_ops[] = {
   {
      "vcfux", /* One reg, one 5-bit uimm argument */
      &vcvt_cb,
   },
   {
      "vcfsx", /* One reg, one 5-bit uimm argument */
      &vcvt_cb,
   },
   {
      "vctuxs", /* One reg, one 5-bit uimm argument */
      &vcvt_cb,
   },
   {
      "vcfux", /* One reg, one 5-bit uimm argument */
      &vcvt_cb,
   },
   {
      "vctsxs", /* One reg, one 5-bit uimm argument */
      &vcvt_cb,
   },
   {
      NULL,
      NULL,
   },
};

static void test_av_float_special (const char* name, test_func_t func,
                                   uint32_t test_flags)
{
   test_special(special_av_float_ops, name, func, test_flags);
}

/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/
static test_loop_t altivec_float_loops[] = {
   &test_av_float_one_arg,
   &test_av_float_two_args,
   &test_av_float_three_args,
   &test_av_float_two_args,
   NULL,
   NULL,
   &test_av_float_special,
   NULL,
   NULL,
   NULL,
   NULL,
};

#endif /* defined (HAS_ALTIVEC) */


#if defined (IS_PPC405)
static void test_ppc405 (const char* name, test_func_t func,
                         unused uint32_t test_flags)
{
   volatile uint32_t res, flags, xer, tmpcr, tmpxer;
   int i, j, k;
   
   for (i=0; i<nb_iargs; i++) {
      for (j=0; j<nb_iargs; j++) {
         for (k=0; k<nb_iargs; k++) {
            r14 = iargs[i];
            r15 = iargs[j];
            /* Beware: the third argument and the result
             * are in the same register
             */
            r17 = iargs[k];

            /* Save flags */
            __asm__ __volatile__ ("mfcr 18");
            tmpcr = r18;
            __asm__ __volatile__ ("mfxer 18");
            tmpxer = r18;

            /* Set up flags for test */
            r18 = 0;
            __asm__ __volatile__ ("mtcr 18");
            __asm__ __volatile__ ("mtxer 18");
            (*func)();
            __asm__ __volatile__ ("mfcr 18");
            flags = r18;
            __asm__ __volatile__ ("mfxer 18");
            xer = r18;
            res = r17;

            /* Restore flags */
            r18 = tmpcr;
            __asm__ __volatile__ ("mtcr 18");
            r18 = tmpxer;
            __asm__ __volatile__ ("mtxer 18");

            printf("%s %08x, %08x, %08x => %08x (%08x %08x)\n",
                   name, iargs[i], iargs[j], iargs[k], res, flags, xer);
         }
         if (verbose) printf("\n");
      }
   }
}
#endif /* defined (IS_PPC405) */

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
   int integer, floats, p405, altivec, faltivec, misc, sh_algebraic, mfspr;
   int cr;
} insn_sel_flags_t;

static void do_tests ( insn_sel_flags_t seln_flags,
                       char *filter)
{
#if defined (IS_PPC405)
   test_loop_t tmpl;
#endif
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
          (nb_args == 3 && !seln_flags.three_args))
         continue;
      /* Check instruction type */
      type = all_tests[i].flags & PPC_TYPE;
      if ((type == PPC_ARITH   && !seln_flags.arith) ||
          (type == PPC_LOGICAL && !seln_flags.logical) ||
          (type == PPC_COMPARE && !seln_flags.compare) ||
          (type == PPC_LDST && !seln_flags.ldst) ||
          (type == PPC_POPCNT && !seln_flags.arith))
         continue;
      /* Check instruction family */
      family = all_tests[i].flags & PPC_FAMILY;
      if ((family == PPC_INTEGER  && !seln_flags.integer) ||
          (family == PPC_FLOAT    && !seln_flags.floats) ||
          (family == PPC_405      && !seln_flags.p405) ||
          (family == PPC_ALTIVEC  && !seln_flags.altivec) ||
          (family == PPC_MISC  && !seln_flags.misc) ||
          (family == PPC_SH_ALGEBRAIC && !seln_flags.sh_algebraic) ||
          (family == PPC_MFSPR && !seln_flags.mfspr) ||
          (family == PPC_FALTIVEC && !seln_flags.faltivec))
         continue;
      /* Check flags update */
      if (((all_tests[i].flags & PPC_CR)  && seln_flags.cr == 0) ||
          (!(all_tests[i].flags & PPC_CR) && seln_flags.cr == 1))
         continue;
      /* All passed, do the tests */
      tests = all_tests[i].tests;
      /* Select the test loop */
      switch (family) {
      case PPC_INTEGER:
         loop = &int_loops[nb_args - 1];
         break;
      case PPC_SH_ALGEBRAIC:
         loop = &int_sh_algebraic[0];
         break;
      case PPC_MFSPR:
         loop = &int_mfspr[0];
         break;
      case PPC_MISC:
         loop = &misc_loops[0];
         break;
      case PPC_FLOAT:
#if !defined (NO_FLOAT)
         loop = &float_loops[nb_args - 1];
         break;
#else
         fprintf(stderr, "Sorry. "
                 "PPC floating point instructions tests "
                 "are disabled on your host\n");
#endif /* !defined (NO_FLOAT) */
         
      case PPC_405:
#if defined (IS_PPC405)
         tmpl = &test_ppc405;
         loop = &tmpl;
         break;
#else
         fprintf(stderr, "Sorry. "
                 "PPC405 instructions tests are disabled on your host\n");
         continue;
#endif /* defined (IS_PPC405) */
      case PPC_ALTIVEC:
#if defined (HAS_ALTIVEC)
         loop = &altivec_int_loops[nb_args - 1];
         break;
#else
         fprintf(stderr, "Sorry. "
                 "Altivec instructions tests are disabled on your host\n");
         continue;
#endif
      case PPC_FALTIVEC:
#if defined (HAS_ALTIVEC)
         loop = &altivec_float_loops[nb_args - 1];
         break;
#else
         fprintf(stderr, "Sorry. "
                 "Altivec float instructions tests "
                 "are disabled on your host\n");
#endif
         continue;
      default:
         printf("ERROR: unknown insn family %08x\n", family);
         continue;
      }
      if (1 || verbose > 0)
         printf("%s:\n", all_tests[i].name);
      for (j=0; tests[j].name != NULL; j++) {
         if (check_name(tests[j].name, filter, exact)) {
            if (verbose > 1)
               printf("Test instruction %s\n", tests[j].name);
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
#if !defined (USAGE_SIMPLE)
   fprintf(stderr,
           "jm-insns [-1] [-2] [-3] [-*] [-t <type>] [-f <family>] [-u] "
           "[-n <filter>] [-r <test_rigour>] [-h]\n"
           "\t-1: test opcodes with one argument\n"
           "\t-2: test opcodes with two arguments\n"
           "\t-3: test opcodes with three arguments\n"
           "\t-*: launch test without checking the number of arguments\n"
           "\t-t: launch test for instructions of type <type>\n"
           "\t    recognized types:\n"
           "\t\tarith (or a)\n"
           "\t\tlogical (or l)\n"
           "\t\tcompare (or c)\n"
           "\t\tstoreload (or s)\n"
           "\t-f: launch test for instructions of family <family>\n"
           "\t    recognized families:\n"
           "\t\tinteger (or i)\n"
           "\t\tfloat (or f)\n"
           "\t\tppc405 (or mac)\n"
           "\t\taltivec (or a)\n"
           "\t-u: test instructions that update flags\n"
           "\t-n: filter instructions with <filter>\n"
           "\t    <filter> can be in two forms:\n"
           "\t\tname  : filter functions that exactly match <name>\n"
           "\t\tname* : filter functions that start with <name>\n"
           "\t-r: set size of arg tables to use to define <test_rigour>\n"
           "\t    recognized types:\n"
           "\t\tlarge (or l)\n"
           "\t\tsmall (or s) - default\n"
           "\t-v: verbose (-v -v for more)\n"
           "\t-h: print this help\n"
           );
#else // #if !defined (USAGE_SIMPLE)
   fprintf(stderr,
           "Usage: jm-insns [OPTION]\n"
           "\t-i: test integer arithmetic instructions (default)\n"
           "\t-l: test integer logical instructions (default)\n"
           "\t-c: test integer compare instructions (default)\n"
           "\t-L: test integer load/store instructions (default)\n"
           "\t-f: test floating point instructions\n"
           "\t-a: test altivec instructions\n"
           "\t-m: test miscellaneous instructions\n"
           "\t-s: test shift algebraic (sraw, srawi, srad, sradi) instructions\n"
           "\t-M: test mfspr instructions\n"
           "\t-A: test all (int, fp, altivec) instructions\n"
           "\t-v: be verbose\n"
           "\t-h: display this help and exit\n"
           );
#endif // #if !defined (USAGE_SIMPLE)
}



int main (int argc, char **argv)
{
#if !defined (USAGE_SIMPLE)
////////////////////////////////////////////////////////////////////////
   unsigned char *tmp, *filter = NULL;
   insn_sel_flags_t flags;
   int c;

   // check HWord_t really is a host word
   assert(sizeof(void*) == sizeof(HWord_t));

   flags.one_arg    = 0;
   flags.two_args   = 0;
   flags.three_args = 0;
   flags.arith      = 0;
   flags.logical    = 0;
   flags.compare    = 0;
   flags.ldst       = 0;
   flags.integer    = 0;
   flags.floats     = 0;
   flags.p405       = 0;
   flags.altivec    = 0;
   flags.faltivec   = 0;
   flags.cr         = -1;
   flags.sh_algebraic = 0;
   flags.mfspr      = 0;
   
   while ((c = getopt(argc, argv, "123t:f:n:r:uvh")) != -1) {
      switch (c) {
      case '1':
         flags.one_arg = 1;
         break;
      case '2':
         flags.two_args = 1;
         break;
      case '3':
         flags.three_args = 1;
         break;
      case 't':
         tmp = optarg;
         if (strcmp(tmp, "arith") == 0 || strcmp(tmp, "a") == 0) {
            flags.arith = 1;
         } else if (strcmp(tmp, "logical") == 0 || strcmp(tmp, "l") == 0) {
            flags.logical = 1;
         } else if (strcmp(tmp, "compare") == 0 || strcmp(tmp, "c") == 0) {
            flags.compare = 1;
         } else if (strcmp(tmp, "storeload") == 0 || strcmp(tmp, "s") == 0) {
            flags.ldst = 1;
         } else {
            goto bad_arg;
         }
         break;
      case 'f':
         tmp = optarg;
         if (strcmp(tmp, "integer") == 0 || strcmp(tmp, "i") == 0) {
            flags.integer = 1;
         } else if (strcmp(tmp, "float") == 0 || strcmp(tmp, "f") == 0) {
            flags.floats = 1;
         } else if (strcmp(tmp, "ppc405") == 0 || strcmp(tmp, "mac") == 0) {
            flags.p405 = 1;
         } else if (strcmp(tmp, "altivec") == 0 || strcmp(tmp, "a") == 0) {
            flags.altivec = 1;
            flags.faltivec = 1;
         } else {
            goto bad_arg;
         }
         break;
      case 'n':
         filter = optarg;
         break;
      case 'r':
         tmp = optarg;
         if (strcmp(tmp, "large") == 0 || strcmp(tmp, "l") == 0) {
            arg_list_size = 1;
         } else if (strcmp(tmp, "small") == 0 || strcmp(tmp, "s") == 0) {
            arg_list_size = 0;
         } else {
            goto bad_arg;
         }
         break;
         
      case 'u':
         flags.cr = 1;
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
      bad_arg:
         usage();
         fprintf(stderr, "Bad argument for '%c': '%s'\n", c, tmp);
         return 1;
      }
   }
   if (argc != optind) {
      usage();
      fprintf(stderr, "Bad number of arguments\n");
      return 1;
   }
   
   // Default n_args
   if (flags.one_arg == 0 && flags.two_args == 0 && flags.three_args == 0) {
      flags.one_arg = 1;
      flags.two_args = 1;
      flags.three_args = 1;
   }
   // Default type
   if (flags.arith == 0 && flags.logical == 0 &&
       flags.compare == 0 && flags.ldst == 0) {
      flags.arith   = 1;
      flags.logical = 1;
      flags.compare = 1;
      flags.ldst    = 1;
   }
   // Default family
   if (flags.integer == 0 && flags.floats == 0 &&
       flags.p405 == 0 && flags.altivec == 0 && flags.faltivec == 0) {
      flags.integer  = 1;
      flags.floats   = 1;
      flags.p405     = 1;
      flags.altivec  = 1;
      flags.faltivec = 1;
      flags.algebraic = 1;
      flags.mfspr = 1;
   }
   // Default cr update
   if (flags.cr == -1)
      flags.cr = 2;       // both

#else // #if !defined (USAGE_SIMPLE)
////////////////////////////////////////////////////////////////////////
   /* Simple usage:
      ./jm-insns -i   => int arithmetic insns
      ./jm-insns -l   => int logical insns
      ./jm-insns -f   => fp  insns
      ./jm-insns -a   => av  insns
      ./jm-insns -m   => miscellaneous insns
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
   flags.arith      = 0;
   flags.logical    = 0;
   flags.compare    = 0;
   flags.ldst       = 0;
   // Family
   flags.integer    = 0;
   flags.floats     = 0;
   flags.misc       = 0;
   flags.p405       = 0;
   flags.altivec    = 0;
   flags.faltivec   = 0;
   flags.sh_algebraic = 0;
   flags.mfspr      = 0;
   // Flags
   flags.cr         = 2;

   while ((c = getopt(argc, argv, "ilcLfmMsahvA")) != -1) {
      switch (c) {
      case 'i':
         flags.arith    = 1;
         flags.integer  = 1;
         break;
      case 's':
         flags.logical  = 1;
         flags.sh_algebraic = 1;
         break;
      case 'M':
         flags.logical  = 1;
         flags.mfspr = 1;
         break;
      case 'l':
         flags.logical  = 1;
         flags.integer  = 1;
         break;
      case 'c':
         flags.compare  = 1;
         flags.integer  = 1;
         break;
      case 'L':
         flags.ldst     = 1;
         flags.integer  = 1;
         break;
      case 'f':
         flags.arith  = 1;
         flags.logical  = 1;
         flags.compare  = 1;
         flags.ldst     = 1;
         flags.floats   = 1;
         break;
      case 'a':
         flags.arith    = 1;
         flags.logical  = 1;
         flags.compare  = 1;
         flags.ldst     = 1;
         flags.altivec  = 1;
         flags.faltivec = 1;
         break;
      case 'm':
         flags.arith    = 1;
         flags.logical  = 1;
         flags.compare  = 1;
         flags.ldst     = 1;
         flags.misc     = 1;
         break;
      case 'A':
         flags.arith    = 1;
         flags.logical  = 1;
         flags.compare  = 1;
         flags.ldst     = 1;
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
#endif // #if !defined (USAGE_SIMPLE)
   

   build_iargs_table();
   build_fargs_table();
   build_ii16_table();
#if defined (HAS_ALTIVEC)
   if (flags.altivec || flags.faltivec) {
      build_viargs_table();
      build_vfargs_table();
   }
#endif
   // dump_iargs();
   // dump_iargs16();
   // dump_vfargs();

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
      printf("    p405       = %d\n", flags.p405);
      printf("    altivec    = %d\n", flags.altivec);
      printf("    faltivec   = %d\n", flags.faltivec);
      printf("    sh_algebraic = %d\n", flags.sh_algebraic);
      printf("    mfspr      = %d\n", flags.mfspr);
      printf("  cr update: \n");
      printf("    cr         = %d\n", flags.cr);
      printf("\n");
      printf("  num args: \n");
      printf("    iargs      - %d\n", nb_iargs);
      printf("    fargs      - %d\n", nb_fargs);
#if defined (HAS_ALTIVEC)
      printf("    viargs     - %d\n", nb_viargs);
      printf("    vfargs     - %d\n", nb_vfargs);
#endif
      printf("\n");
   }
 
   do_tests( flags, filter );
   
   return 0;
}
