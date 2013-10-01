
/* A Memcheck test program for conditional loads and stores,
   as shown in do_conditional_{load,store}32.

   Program is run twice, once for loads and once for stores, only
   because each run generates 80 errors, and we want to see them all.
   Doing both loads and stores in each run runs into the problem that
   errors are more aggressively commoned up after the 100th, and so
   some that we want to see aren't shown.  Splitting the run into two
   pieces avoids this.

   On ARM we hardwire genuine conditional loads and stores to be
   tested -- which is the real point of this test, since we are sure
   they will turn into IRLoadG/IRStoreG.  On other platforms we make
   do with whatever gcc gives us for the equivalent C fragment.  In
   both cases Memcheck's results should be identical -- at least in
   error counts; line numbers unfortunately will differ.  Hence there
   are -arm and -non-arm expected output files. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "../memcheck.h"

typedef  unsigned int  UInt;

typedef  unsigned char  Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

static void make_undef ( void* addr, size_t len )
{
  (void) VALGRIND_MAKE_MEM_UNDEFINED(addr, len);
}

static void make_def ( void* addr, size_t len )
{
  (void) VALGRIND_MAKE_MEM_DEFINED(addr, len);
}

// Returns either |*src| or |alt|.
__attribute__((noinline))
UInt do_conditional_load32 ( UInt* src, UInt alt, Bool b )
{
   UInt res;
#  if defined(__linux__) && defined(__arm__)
   __asm__ __volatile__(
     "mov r5, %2"     "\n\t"  // alt
     "tst %3, #0xFF"  "\n\t"  // b
     "it ne"          "\n\t" 
     "ldrne r5, [%1]" "\n\t"  // src
     "mov %0, r5"     "\n\t"  // res
     : /*OUT*/"=r"(res)
     : /*IN*/"r"(src), "r"(alt), "r"(b)
     : /*TRASH*/ "r5","cc","memory"
   );
#  else
   __asm__ __volatile__("" ::: "cc","memory");
   res = b ? *src : alt;
#  endif
   // res might be undefined.  Paint it as defined so the
   // caller can look at it without invoking further errors.
   make_def(&res, sizeof(res));
   return res;
}

// Possibly writes |alt| to |*dst|, and returns the resulting
// value of |*dst|.
__attribute__((noinline))
UInt do_conditional_store32 ( UInt* dst, UInt alt, Bool b )
{
#  if defined(__linux__) && defined(__arm__)
   __asm__ __volatile__(
     "mov r5, %1"     "\n\t"  // alt
     "tst %2, #0xFF"  "\n\t"  // b
     "it ne"          "\n\t" 
     "strne r5, [%0]" "\n\t"  // dst
     : /*OUT*/
     : /*IN*/"r"(dst), "r"(alt), "r"(b)
     : /*TRASH*/ "r5","cc","memory"
   );
#  else
   __asm__ __volatile__("" ::: "cc","memory");
   if (b) *dst = alt;
#  endif
   /* Now we need to get hold of the value at *dst.  But it might be
      unaddressible and/or undefined.  Hence turn off error reporting
      when getting it. */
   UInt res;
   VALGRIND_DISABLE_ERROR_REPORTING;
   res = *dst;
   VALGRIND_ENABLE_ERROR_REPORTING;
   make_def(&res, sizeof(res));
   return res;
}


/* --- LOAD ----------------------------------------- LOAD --- */
/* --- LOAD ----------------------------------------- LOAD --- */
/* --- LOAD ----------------------------------------- LOAD --- */

/* For conditional loads, there are 64 combinations to test.

   cond: { defined-true, defined-false,
           undefined-true, undefined-false }     D1 D0 U1 U0
   x
   addr: { defined-valid, defined-invalid,
           undefined-valid, undefined-invalid }  DV DI UV UI
   x
   alt:  { defined, undefined }                  Da Ub
   x
   data: { defined, undefined }                  Dc Ud

   // a, b, c, d refer to actual values

   The general form of the test is:
   1.  Place data at *addr
   2.  return "cond ? *addr : alt"
*/
typedef  enum { Cond_D1=10, Cond_D0, Cond_U1, Cond_U0 }  Inp_Cond;
typedef  enum { Addr_DV=20, Addr_DI, Addr_UV, Addr_UI }  Inp_Addr;
typedef  enum { Alt_Da=30,  Alt_Ub }                     Inp_Alt;
typedef  enum { Data_Dc=40, Data_Ud }                    Inp_Data;

typedef
   struct { Inp_Cond inp_Cond;  Inp_Addr inp_Addr;
            Inp_Alt  inp_Alt;   Inp_Data inp_Data;
            char res; char defErr_Cond; char defErr_Addr; char addrErr; }
   TestCase;

const TestCase loadCases[64] = {

   // ADDR       ALT         COND       DATA        Res 
   //                                                    defErr-COND
   //                                                         defErr-ADDR
   //                                                              addrErr

   // In all of the next 16 cases, the load definitely happens
   // and |alt| is therefore irrelevant
   { Cond_D1,    Addr_DV,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'N' }, // 0
   { Cond_D1,    Addr_DV,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DV,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DV,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DI,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'Y' },

   { Cond_D1,    Addr_UV,    Alt_Da,    Data_Dc,    'C', 'N', 'Y', 'N' }, // 8
   { Cond_D1,    Addr_UV,    Alt_Da,    Data_Ud,    'D', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UV,    Alt_Ub,    Data_Dc,    'C', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UV,    Alt_Ub,    Data_Ud,    'D', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UI,    Alt_Da,    Data_Dc,    'C', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Da,    Data_Ud,    'D', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Ub,    Data_Dc,    'C', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Ub,    Data_Ud,    'D', 'N', 'Y', 'Y' },

   // In the next 16 cases, the load definitely does not happen,
   // so we just return |alt|.
   { Cond_D0,    Addr_DV,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'N' }, // 16
   { Cond_D0,    Addr_DV,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DV,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DV,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'N' },

   { Cond_D0,    Addr_UV,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'N' }, // 24
   { Cond_D0,    Addr_UV,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UV,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UV,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'N' },

   // ADDR       ALT         COND       DATA        Res 
   //                                                    defErr-COND
   //                                                         defErr-ADDR
   //                                                              addrErr

   // In the next 16 cases, the load happens, but the condition
   // is undefined.  This means that it should behave like the
   // first group of 16 cases, except that we should also get a
   // complaint about the definedness of the condition.
   { Cond_U1,    Addr_DV,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'N' }, // 32
   { Cond_U1,    Addr_DV,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DV,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DV,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DI,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'Y' },

   { Cond_U1,    Addr_UV,    Alt_Da,    Data_Dc,    'C', 'Y', 'Y', 'N' }, // 40
   { Cond_U1,    Addr_UV,    Alt_Da,    Data_Ud,    'D', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UV,    Alt_Ub,    Data_Dc,    'C', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UV,    Alt_Ub,    Data_Ud,    'D', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UI,    Alt_Da,    Data_Dc,    'C', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Da,    Data_Ud,    'D', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Ub,    Data_Dc,    'C', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Ub,    Data_Ud,    'D', 'Y', 'Y', 'Y' },

   // In this last group of 16 cases, the load does not happen,
   // but the condition is undefined.  So we just return |alt|,
   // and also complain about the condition.  Hence it's like the
   // second group of 16 cases except that we also get a complaint
   // about the condition.
   { Cond_U0,    Addr_DV,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'N' }, // 48
   { Cond_U0,    Addr_DV,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DV,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DV,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'N' },

   { Cond_U0,    Addr_UV,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'N' }, // 56
   { Cond_U0,    Addr_UV,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UV,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UV,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'N' }  // 63
};

// Constant, corresponding to the test enums
static Bool c_Cond_D1, c_Cond_D0, c_Cond_U1, c_Cond_U0;
static UInt *c_Addr_DV, *c_Addr_DI, *c_Addr_UV, *c_Addr_UI;
static UInt c_Alt_Da, c_Alt_Ub;

static void setup_test_data ( Inp_Data inp_Data )
{
   c_Cond_D1 = c_Cond_U1 = True;
   c_Cond_D0 = c_Cond_U0 = False;
   make_undef(&c_Cond_U1, sizeof(c_Cond_U1));
   make_undef(&c_Cond_U0, sizeof(c_Cond_U0));

   c_Addr_DV = c_Addr_UV = malloc(4);
   c_Addr_DI = c_Addr_UI = malloc(4);
   // install test data at the given address
   UInt testd = inp_Data == Data_Dc ? 0xCCCCCCCC : 0xDDDDDDDD;
   *c_Addr_DV = *c_Addr_DI = testd;
   if (inp_Data == Data_Dc) {
     // it's already defined
   } else {
     make_undef(c_Addr_DV, 4);
     make_undef(c_Addr_DI, 4);
   }

   // make the invalid address invalid.  This unfortunately loses
   // the definedness state of the data that is stored there.
   free(c_Addr_DI);

   // and set the definedness of the pointers themselves.
   make_undef(&c_Addr_UV, sizeof(c_Addr_UV));
   make_undef(&c_Addr_UI, sizeof(c_Addr_UI));

   // and set up alt
   c_Alt_Da = 0xAAAAAAAA;
   c_Alt_Ub = 0xBBBBBBBB;
   make_undef(&c_Alt_Ub, sizeof(c_Alt_Ub));
}

static void do_test_case ( int caseNo, Bool isLoad, const TestCase* lc )
{
   fprintf(stderr,
           "\n-----------------------------------------------------------\n");
   fprintf(stderr, "%s CASE %d\n", isLoad ? "LOAD" : "STORE", caseNo);
   // validate ..
   assert(Cond_D1 <= lc->inp_Cond && lc->inp_Cond <= Cond_U0);
   assert(Addr_DV <= lc->inp_Addr && lc->inp_Addr <= Addr_UI);
   assert(lc->inp_Alt == Alt_Da || lc->inp_Alt == Alt_Ub);
   assert(lc->inp_Data == Data_Dc || lc->inp_Data == Data_Ud);
   assert('A' <= lc->res && lc->res <= 'D');
   assert(lc->defErr_Cond == 'Y' || lc->defErr_Cond == 'N');
   assert(lc->defErr_Addr == 'Y' || lc->defErr_Addr == 'N');
   assert(lc->addrErr     == 'Y' || lc->addrErr     == 'N');
   // set up test data constants
   setup_test_data(lc->inp_Data);

   // and select constants for the test, depending on |lc|
   // Except, skip i_Data since setup_test_data takes care of it.
   Bool i_Cond;
   UInt* i_Addr;
   UInt i_Alt;
   switch (lc->inp_Cond) {
     case Cond_D1: i_Cond = c_Cond_D1; break;
     case Cond_D0: i_Cond = c_Cond_D0; break;
     case Cond_U1: i_Cond = c_Cond_U1; break;
     case Cond_U0: i_Cond = c_Cond_U0; break;
     default: assert(0);
   }
   switch (lc->inp_Addr) {
     case Addr_DV: i_Addr = c_Addr_DV; break;
     case Addr_DI: i_Addr = c_Addr_DI; break;
     case Addr_UV: i_Addr = c_Addr_UV; break;
     case Addr_UI: i_Addr = c_Addr_UI; break;
     default: assert(0);
   }
   switch (lc->inp_Alt) {
     case Alt_Da: i_Alt = c_Alt_Da; break;
     case Alt_Ub: i_Alt = c_Alt_Ub; break;
     default: assert(0);
   }

   // How many errors do we expect from this?
   UInt n_errs_exp
     = (lc->defErr_Cond == 'Y' ? 1 : 0) + (lc->defErr_Addr == 'Y' ? 1 : 0)
       + (lc->addrErr == 'Y' ? 1 : 0);

   UInt n_errs_act = VALGRIND_COUNT_ERRORS;
   UInt res_act;
   if (isLoad) {
      res_act = do_conditional_load32(i_Addr, i_Alt, i_Cond);
   } else {
      res_act = do_conditional_store32(i_Addr, i_Alt, i_Cond);
   }
   n_errs_act = VALGRIND_COUNT_ERRORS - n_errs_act;

   if (n_errs_act == n_errs_exp) {
      fprintf(stderr, "PASS: %u errors\n", n_errs_act);
   } else {
      fprintf(stderr, "FAIL: %u errors expected, %u actual\n",
              n_errs_exp, n_errs_act);
   }

   // What's the expected result value (actual loaded data?)
   UInt res_exp = 0;
   switch (lc->res) {
      case 'A': res_exp = 0xAAAAAAAA; break;
      case 'B': res_exp = 0xBBBBBBBB; break;
      case 'C': res_exp = 0xCCCCCCCC; break;
      case 'D': res_exp = 0xDDDDDDDD; break;
      default: assert(0);
   }

   if (res_act == res_exp) {
      fprintf(stderr, "PASS: correct result\n");
   } else {
      fprintf(stderr, "FAIL: result: %08x expected, %08x actual\n",
              res_exp, res_act);
   }

   free(c_Addr_DV);
}


void do_test_case_steer ( void (*fn)(int,Bool,const TestCase*),
                          int i, Bool isLd, const TestCase* tc )
{
   __asm__ __volatile__("");
   if (i == 0) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 1) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 2) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 3) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 4) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 5) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 6) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 7) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 8) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 9) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 10) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 11) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 12) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 13) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 14) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 15) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 16) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 17) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 18) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 19) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 20) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 21) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 22) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 23) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 24) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 25) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 26) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 27) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 28) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 29) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 30) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 31) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 32) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 33) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 34) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 35) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 36) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 37) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 38) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 39) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 40) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 41) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 42) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 43) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 44) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 45) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 46) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 47) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 48) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 49) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 50) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 51) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 52) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 53) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 54) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 55) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 56) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 57) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 58) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 59) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 60) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 61) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 62) { fn(i,isLd,tc); return; };
   __asm__ __volatile__("");
   if (i == 63) { fn(i,isLd,tc); return; };
   assert(0);
}


/* --- STORE --------------------------------------- STORE --- */
/* --- STORE --------------------------------------- STORE --- */
/* --- STORE --------------------------------------- STORE --- */

/* For conditional stores, there are 64 combinations to test.

   cond: { defined-true, defined-false,
           undefined-true, undefined-false }     D1 D0 U1 U0
   x
   addr: { defined-valid, defined-invalid,
           undefined-valid, undefined-invalid }  DV DI UV UI
   x
   alt:  { defined, undefined }                  Da Ub
   x
   data: { defined, undefined }                  Dc Ud

   // a, b, c, d refer to actual values

   The general form of the test is:
   1.  Place data at *addr
   2.  do "if (cond) *addr = alt"
   3   return *addr

   Hence identical setup to the load cases, although the roles of
   data and alt are somewhat confusingly swapped.  |data| here is
   the "didn't happen" result, and |alt| is the "did happen" result.
*/

const TestCase storeCases[64] = {

   // ADDR       ALT         COND       DATA        Res 
   //                                                    defErr-COND
   //                                                         defErr-ADDR
   //                                                              addrErr

   // In all of the next 16 cases, the store definitely happens
   // and |data| is therefore irrelevant
   { Cond_D1,    Addr_DV,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'N' }, // 0
   { Cond_D1,    Addr_DV,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DV,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DV,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'N' },
   { Cond_D1,    Addr_DI,    Alt_Da,    Data_Dc,    'A', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Da,    Data_Ud,    'A', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Ub,    Data_Dc,    'B', 'N', 'N', 'Y' },
   { Cond_D1,    Addr_DI,    Alt_Ub,    Data_Ud,    'B', 'N', 'N', 'Y' },

   { Cond_D1,    Addr_UV,    Alt_Da,    Data_Dc,    'A', 'N', 'Y', 'N' }, // 8
   { Cond_D1,    Addr_UV,    Alt_Da,    Data_Ud,    'A', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UV,    Alt_Ub,    Data_Dc,    'B', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UV,    Alt_Ub,    Data_Ud,    'B', 'N', 'Y', 'N' },
   { Cond_D1,    Addr_UI,    Alt_Da,    Data_Dc,    'A', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Da,    Data_Ud,    'A', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Ub,    Data_Dc,    'B', 'N', 'Y', 'Y' },
   { Cond_D1,    Addr_UI,    Alt_Ub,    Data_Ud,    'B', 'N', 'Y', 'Y' },

   // In the next 16 cases, the store definitely does not happen,
   // so we just return |data|.
   { Cond_D0,    Addr_DV,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'N' }, // 16
   { Cond_D0,    Addr_DV,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DV,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DV,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_DI,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'N' },

   { Cond_D0,    Addr_UV,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'N' }, // 24
   { Cond_D0,    Addr_UV,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UV,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UV,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Da,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Da,    Data_Ud,    'D', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Ub,    Data_Dc,    'C', 'N', 'N', 'N' },
   { Cond_D0,    Addr_UI,    Alt_Ub,    Data_Ud,    'D', 'N', 'N', 'N' },

   // ADDR       ALT         COND       DATA        Res 
   //                                                    defErr-COND
   //                                                         defErr-ADDR
   //                                                              addrErr

   // In the next 16 cases, the store happens, but the condition
   // is undefined.  This means that it should behave like the
   // first group of 16 cases, except that we should also get a
   // complaint about the definedness of the condition.
   { Cond_U1,    Addr_DV,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'N' }, // 32
   { Cond_U1,    Addr_DV,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DV,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DV,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'N' },
   { Cond_U1,    Addr_DI,    Alt_Da,    Data_Dc,    'A', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Da,    Data_Ud,    'A', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Ub,    Data_Dc,    'B', 'Y', 'N', 'Y' },
   { Cond_U1,    Addr_DI,    Alt_Ub,    Data_Ud,    'B', 'Y', 'N', 'Y' },

   { Cond_U1,    Addr_UV,    Alt_Da,    Data_Dc,    'A', 'Y', 'Y', 'N' }, // 40
   { Cond_U1,    Addr_UV,    Alt_Da,    Data_Ud,    'A', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UV,    Alt_Ub,    Data_Dc,    'B', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UV,    Alt_Ub,    Data_Ud,    'B', 'Y', 'Y', 'N' },
   { Cond_U1,    Addr_UI,    Alt_Da,    Data_Dc,    'A', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Da,    Data_Ud,    'A', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Ub,    Data_Dc,    'B', 'Y', 'Y', 'Y' },
   { Cond_U1,    Addr_UI,    Alt_Ub,    Data_Ud,    'B', 'Y', 'Y', 'Y' },

   // In this last group of 16 cases, the store does not happen,
   // but the condition is undefined.  So we just return |data|,
   // and also complain about the condition.  Hence it's like the
   // second group of 16 cases except that we also get a complaint
   // about the condition.
   { Cond_U0,    Addr_DV,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'N' }, // 48
   { Cond_U0,    Addr_DV,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DV,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DV,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_DI,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'N' },

   { Cond_U0,    Addr_UV,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'N' }, // 56
   { Cond_U0,    Addr_UV,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UV,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UV,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Da,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Da,    Data_Ud,    'D', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Ub,    Data_Dc,    'C', 'Y', 'N', 'N' },
   { Cond_U0,    Addr_UI,    Alt_Ub,    Data_Ud,    'D', 'Y', 'N', 'N' }  // 63
};

void usage ( char* pname )
{
  fprintf(stderr, "usage: %s [loads|stores]\n", pname);
  exit(1);
}

int main ( int argc, char** argv )
{
   UInt i, nCases;

   if (argc != 2) usage(argv[0]);

   Bool doLoad = False;
   if (0 == strcmp(argv[1], "loads")) {
     doLoad = True;
   }
   else if (0 == strcmp(argv[1], "stores")) {
     doLoad = False;
   }
   else usage(argv[0]);

   if (doLoad) {
     nCases = sizeof(loadCases) / sizeof(loadCases[0]);
     assert(nCases == 64);
     for (i = 0; i < nCases; i++)
       do_test_case_steer( do_test_case, i, True/*isLoad*/, &loadCases[i] );
   } else {
     nCases = sizeof(storeCases) / sizeof(storeCases[0]);
     assert(nCases == 64);
     for (i = 0; i < nCases; i++)
       do_test_case_steer( do_test_case, i, False/*!isLoad*/, &storeCases[i] );
   }

   return 0;
}
