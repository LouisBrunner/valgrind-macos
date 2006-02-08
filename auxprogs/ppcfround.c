
#include <assert.h>
#include <stdio.h>
#include <malloc.h>

typedef unsigned int UInt;
typedef unsigned long long int ULong;

typedef
   struct {
      double fres;
      UInt cr;
      UInt fpscr;  
   }
   Result;


static void set_NEAREST ( void ) {
   __asm__ __volatile__("mtfsb0 30 ; mtfsb0 31");
}
static void set_ZERO ( void ) {
   __asm__ __volatile__("mtfsb0 30 ; mtfsb1 31");
}
static void set_PosINF ( void ) {
   __asm__ __volatile__("mtfsb1 30 ; mtfsb0 31");
}
static void set_NegINF ( void ) {
   __asm__ __volatile__("mtfsb1 30 ; mtfsb1 31");
}


static ULong double_as_ULong ( double d )
{
   union { double dd; ULong ll; } u;
   assert(sizeof(u) == 8);
   u.dd = d;
   return u.ll;
}

static ULong round_with_mask ( ULong x, ULong mask )
{
  if (mask == 1) {
     switch (x & 1) {
        case 0:  return x;
        case 1:  return x+1;
     }
     assert(0);
  }

  if (mask == 3) {
     switch (x & 3) {
        case 0:  return x;
        case 1:  return x-1;
        case 2:  return x+2;
        case 3:  return x+1;
     }
     assert(0);
  }

  if (mask == 7) {
     switch (x & 7) {
        case 0:  return x;
        case 1:  return x-1;
        case 2:  return x-2;
        case 3:  return x-3;
        case 4:  return x+4;
        case 5:  return x+3;
        case 6:  return x+2;
        case 7:  return x+1;
     }
     assert(0);
  }

  if (mask == 15) {
     switch (x & 15) {
        case 0:  return x;
        case 1:  return x-1;
        case 2:  return x-2;
        case 3:  return x-3;
        case 4:  return x-4;
        case 5:  return x-5;
        case 6:  return x-6;
        case 7:  return x-7;
        case 8:   return x+8;
        case 9:   return x+7;
        case 10:  return x+6;
        case 11:  return x+5;
        case 12:  return x+4;
        case 13:  return x+3;
        case 14:  return x+2;
        case 15:  return x+1;
     }
     assert(0);
  }

  assert(0);
}

static void showResult ( Result r, ULong hideMask )
{
  /* hidemask should have 1 for every result bit we **don't** 
     want to show.  viz should be all zeroes normally. */
#if 0
  printf("(%016llx cr1 0x%01x fprf 0x%02x)",
         double_as_ULong(r.fres) & ~hidemask, 
         (r.cr >> 24) & 0xF, (r.fpscr >> 12) & 0x1F);
#else
  printf("(%016llx cr1 ... fprf ...)",
	 (hideMask == 0x1 || hideMask == 0x3 || hideMask == 0x7)
	 ? round_with_mask( double_as_ULong(r.fres), hideMask )
         : double_as_ULong(r.fres) & ~hideMask
        );
#endif
}


/* Give an insn string such as "fmadd %%f4, %%f1,%%f2,%%f3".  Args are
   in f1, f2, f3, and result should go in f4. */
#define INSN(name,insn)                                                 \
                                                                        \
  static Result insn_##name ( double arg1, double arg2, double arg3 )   \
  {                                                                     \
     struct {                                                           \
        /* 0  */ double a1;                                             \
        /* 8  */ double a2;                                             \
        /* 16 */ double a3;                                             \
        /* 24 */ double res;                                            \
        /* 32 */ UInt fpscr_after;                                      \
        /* 36 */ UInt cr_after;                                         \
     } foo;                                                             \
     assert(sizeof(foo) == 40);                                         \
     foo.a1 = foo.a2 = foo.a3 = foo.res = 0;                            \
     foo.fpscr_after = foo.cr_after = 0;                                \
     foo.a1 = arg1;                                                     \
     foo.a2 = arg2;                                                     \
     foo.a3 = arg3;                                                     \
     __asm__ __volatile__(                                              \
       "lfd  %%f1, 0(%0)\n\t"  /* a1 */                                 \
       "lfd  %%f2, 8(%0)\n\t"  /* a2 */                                 \
       "lfd  %%f3, 16(%0)\n\t" /* a3 */                                 \
       insn "\n\t"                                                      \
       "stfd %%f4, 24(%0)\n\t" /* res */                                \
       "mffs %%f4\n\t"                                                  \
       "addi %0,%0,32\n\t"                                              \
       "stfiwx %%f4, %%r0,%0\n\t"  /* fpscr_after.  r0 reads as zero */ \
       "addi %0,%0,-32\n\t"                                             \
       "mfcr %%r31\n\t"                                                 \
       "stw %%r31, 36(%0)"  /* cr_after */                              \
       : /*out*/                                                        \
       : /*in*/ "b" (&foo.a1)                                           \
       : /*trash*/ "memory","cc", "fr1","fr2","fr3","fr4", "r31"        \
     );                                                                 \
     { Result result;                                                   \
       result.fres  = foo.res;                                          \
       result.cr    = foo.cr_after;                                     \
       result.fpscr = foo.fpscr_after;                                  \
       return result;                                                   \
     }                                                                  \
  }

INSN(fabs,     "fabs     %%f4, %%f1");
INSN(fabs_,    "fabs.    %%f4, %%f1");

INSN(fnabs,    "fnabs    %%f4, %%f1");
INSN(fnabs_,   "fnabs.   %%f4, %%f1");

INSN(fadd,     "fadd     %%f4, %%f1,%%f2");
INSN(fadd_,    "fadd.    %%f4, %%f1,%%f2");

INSN(fadds,    "fadds    %%f4, %%f1,%%f2");
INSN(fadds_,   "fadds.   %%f4, %%f1,%%f2");

INSN(fcfid,    "fcfid    %%f4, %%f1");
INSN(fcfid_,   "fcfid.   %%f4, %%f1");

INSN(fctid,    "fctid    %%f4, %%f1");
INSN(fctid_,   "fctid.   %%f4, %%f1");

INSN(fctidz,   "fctidz   %%f4, %%f1");
INSN(fctidz_,  "fctidz.  %%f4, %%f1");

INSN(fctiw,    "fctiw    %%f4, %%f1");
INSN(fctiw_,   "fctiw.   %%f4, %%f1");

INSN(fctiwz,   "fctiwz   %%f4, %%f1");
INSN(fctiwz_,  "fctiwz.  %%f4, %%f1");

INSN(fdiv,     "fdiv     %%f4, %%f1,%%f2");
INSN(fdiv_,    "fdiv.    %%f4, %%f1,%%f2");

INSN(fdivs,    "fdivs    %%f4, %%f1,%%f2");
INSN(fdivs_,   "fdivs.   %%f4, %%f1,%%f2");

INSN(fmadd,    "fmadd    %%f4, %%f1,%%f2,%%f3");
INSN(fmadd_,   "fmadd.   %%f4, %%f1,%%f2,%%f3");

INSN(fmadds,   "fmadds   %%f4, %%f1,%%f2,%%f3");
INSN(fmadds_,  "fmadds.  %%f4, %%f1,%%f2,%%f3");

INSN(fmr,      "fmr      %%f4, %%f1");
INSN(fmr_,     "fmr.     %%f4, %%f1");

INSN(fmsub,    "fmsub    %%f4, %%f1,%%f2,%%f3");
INSN(fmsub_,   "fmsub.   %%f4, %%f1,%%f2,%%f3");

INSN(fmsubs,   "fmsubs   %%f4, %%f1,%%f2,%%f3");
INSN(fmsubs_,  "fmsubs.  %%f4, %%f1,%%f2,%%f3");

INSN(fmul,     "fmul     %%f4, %%f1,%%f2");
INSN(fmul_,    "fmul.    %%f4, %%f1,%%f2");

INSN(fmuls,    "fmuls    %%f4, %%f1,%%f2");
INSN(fmuls_,   "fmuls.   %%f4, %%f1,%%f2");

INSN(fneg,     "fneg     %%f4, %%f1");
INSN(fneg_,    "fneg.    %%f4, %%f1");

INSN(fnmadd,   "fnmadd   %%f4, %%f1,%%f2,%%f3");
INSN(fnmadd_,  "fnmadd.  %%f4, %%f1,%%f2,%%f3");

INSN(fnmadds,  "fnmadds  %%f4, %%f1,%%f2,%%f3");
INSN(fnmadds_, "fnmadds. %%f4, %%f1,%%f2,%%f3");

INSN(fnmsub,   "fnmsub   %%f4, %%f1,%%f2,%%f3");
INSN(fnmsub_,  "fnmsub.  %%f4, %%f1,%%f2,%%f3");

INSN(fnmsubs,  "fnmsubs  %%f4, %%f1,%%f2,%%f3");
INSN(fnmsubs_, "fnmsubs. %%f4, %%f1,%%f2,%%f3");

INSN(fre,      "fre      %%f4, %%f1");
INSN(fre_,     "fre.     %%f4, %%f1");

INSN(fres,     "fres     %%f4, %%f1");
INSN(fres_,    "fres.    %%f4, %%f1");

INSN(frsqrte,  "frsqrte  %%f4, %%f1");
INSN(frsqrte_, "frsqrte. %%f4, %%f1");

//INSN(frsqrtes, "frsqrtes %%f4, %%f1");
//INSN(frsqrtes_, "frsqrtes. %%f4, %%f1");

INSN(frsp,     "frsp     %%f4, %%f1");
INSN(frsp_,    "frsp.    %%f4, %%f1");

INSN(fsel,     "fsel     %%f4, %%f1,%%f2,%%f3");
INSN(fsel_,    "fsel.    %%f4, %%f1,%%f2,%%f3");

INSN(fsqrt,    "fsqrt    %%f4, %%f1");
INSN(fsqrt_,   "fsqrt.   %%f4, %%f1");

INSN(fsqrts,   "fsqrts   %%f4, %%f1");
INSN(fsqrts_,  "fsqrts.  %%f4, %%f1");

INSN(fsub,     "fsub     %%f4, %%f1,%%f2");
INSN(fsub_,    "fsub.    %%f4, %%f1,%%f2");

INSN(fsubs,    "fsubs    %%f4, %%f1,%%f2");
INSN(fsubs_,   "fsubs.   %%f4, %%f1,%%f2");



void do_1_unary ( char* name, 
                  Result(*f)(double,double,double),
                  double a1,
                  ULong hideMask )
{
   Result r;
   printf("%8s: %016llx (%e)\n", name, double_as_ULong(a1), a1);
   set_NEAREST();
   r = f(a1, 0.0,0.0);
   printf("        near "); showResult(r,hideMask); printf("\n");
   set_ZERO();
   r = f(a1, 0.0,0.0);
   printf("        zero "); showResult(r,hideMask); printf("\n");
   set_PosINF();
   r = f(a1, 0.0,0.0);
   printf("        +inf "); showResult(r,hideMask); printf("\n");
   set_NegINF();
   r = f(a1, 0.0,0.0);
   printf("        -inf "); showResult(r,hideMask); printf("\n"); 
}

void do_1_binary ( char* name, 
                   Result(*f)(double,double,double),
                   double a1, double a2, 
                   ULong hideMask )
{
   Result r;
   printf("%8s: %016llx %016llx\n", name, double_as_ULong(a1),
	  double_as_ULong(a2));
   set_NEAREST();
   r = f(a1,a2, 0.0);
   printf("        near "); showResult(r,hideMask); printf("\n");
   set_ZERO();
   r = f(a1,a2, 0.0);
   printf("        zero "); showResult(r,hideMask); printf("\n");
   set_PosINF();
   r = f(a1,a2, 0.0);
   printf("        +inf "); showResult(r,hideMask); printf("\n");
   set_NegINF();
   r = f(a1,a2, 0.0);
   printf("        -inf "); showResult(r,hideMask); printf("\n"); 
}

void do_1_ternary ( char* name, 
                    Result(*f)(double,double,double),
                    double a1, double a2, double a3,
                    ULong hideMask )
{
   Result r;
   printf("%8s: %016llx %016llx %016llx\n", 
          name, double_as_ULong(a1),
	  double_as_ULong(a2), double_as_ULong(a3));
   set_NEAREST();
   r = f(a1,a2,a3);
   printf("        near "); showResult(r,hideMask); printf("\n");
   set_ZERO();
   r = f(a1,a2,a3);
   printf("        zero "); showResult(r,hideMask); printf("\n");
   set_PosINF();
   r = f(a1,a2,a3);
   printf("        +inf "); showResult(r,hideMask); printf("\n");
   set_NegINF();
   r = f(a1,a2,a3);
   printf("        -inf "); showResult(r,hideMask); printf("\n"); 
}

void do_N_unary ( char* name,
                  Result(*f)(double,double,double),
                  double* args,
		  int nargs,
                  ULong hideMask )
{
   int i;
   for (i = 0; i < nargs; i++) {
      do_1_unary( name, f, args[i], hideMask );
   }
}

void do_N_binary ( char* name,
                   Result(*f)(double,double,double),
                   double* args,
                   int nargs,
                   ULong hideMask )
{
   int i, j;
   for (i = 0; i < nargs; i++) {
      for (j = 0; j < nargs; j++) {
         do_1_binary( name, f, args[i], args[j], hideMask );
      }
   }
}

void do_N_ternary ( char* name,
                    Result(*f)(double,double,double),
                    double* args,
                    int nargs,
                    ULong hideMask )
{
   int i, j, k;
   for (i = 0; i < nargs; i++) {
      for (j = 0; j < nargs; j++) {
         for (k = 0; k < nargs; k++) {
            do_1_ternary( name, f, args[i], args[j], args[k], hideMask );
         }
      }
   }
}

int main ( void )
{
  const ULong SHOW_ALL = 0;

  int     nargs    = 21;
  int     nMacArgs = 11;

  double* args    = malloc(nargs * sizeof(double));
  double* macArgs = malloc(nMacArgs * sizeof(double));

  args[0]  =  0.0;
  args[1]  =  1.0 / 0.0; // inf
  args[2]  = -args[1]; //  -inf
  args[3]  = args[2]/args[2]; // nan
  args[4]  = -args[3]; // -nan
  args[5]  = -5e100;
  args[6]  = -5e20;
  args[7]  = -501.0;
  args[8]  = -6.0;
  args[9]  = -1.0;
  args[10] = -2e-20;
  args[11] = -2e-200;
  args[12] =  2e-200;
  args[13] =  2e-20;
  args[14] =  1.0;
  args[15] =  6.0;
  args[16] =  501.0;
  args[17] =  5e20;
  args[18] =  5e100;
  args[19] =  1.23e+5;
  args[20] =  1.23e+14;

#if 0
  macArgs[0]  =  0.0;
  macArgs[1]  = -5e100;
  macArgs[2]  = -5e20;
  macArgs[3]  = -501.0;
  macArgs[4]  = -6.0;
  macArgs[5]  = -1.0;
  macArgs[6]  = -2e-20;
  macArgs[7]  = -2e-200;
  macArgs[8]  =  2e-200;
  macArgs[9]  =  2e-20;
  macArgs[10] =  1.0;
  macArgs[11] =  6.0;
  macArgs[12] =  501.0;
  macArgs[13] =  5e20;
  macArgs[14] =  5e100;
  macArgs[15] =  1.23e+5;
  macArgs[16] =  1.23e+14;

  //macArgs[17]  = args[3]; // nan
  //macArgs[18]  = -args[3]; // -nan
#endif

  macArgs[0]  = 0.0;
  macArgs[1]  = 1.0;
  macArgs[2]  = 1.0 + (1.0/7.0);
  macArgs[3]  = 6.01;
  macArgs[4]  = 501.0;
  macArgs[5]  = 31415927.0;
  macArgs[6]  = - 1.0;
  macArgs[7]  = - (1.0 + (1.0/7.0));
  macArgs[8]  = - 6.01;
  macArgs[9]  = - 501.0;
  macArgs[10] = - 31415927.0;


  do_N_unary("fmr",     insn_fmr,    args, nargs, SHOW_ALL);
  do_N_unary("fmr_",    insn_fmr_,   args, nargs, SHOW_ALL);

  do_N_unary("fneg",    insn_fneg,   args, nargs, SHOW_ALL);
  do_N_unary("fneg_",   insn_fneg_,  args, nargs, SHOW_ALL);

  do_N_unary("fabs",    insn_fabs,   args, nargs, SHOW_ALL);
  do_N_unary("fabs_",   insn_fabs_,  args, nargs, SHOW_ALL);

  do_N_unary("fnabs",   insn_fnabs,  args, nargs, SHOW_ALL);
  do_N_unary("fnabs_",  insn_fnabs_, args, nargs, SHOW_ALL);


  do_N_binary("fadd",   insn_fadd,   args, nargs, SHOW_ALL);
  do_N_binary("fadd_",  insn_fadd_,  args, nargs, SHOW_ALL);

  do_N_binary("fadds",  insn_fadds,  args, nargs, SHOW_ALL);
  do_N_binary("fadds_", insn_fadds_, args, nargs, SHOW_ALL);

  do_N_binary("fdiv",   insn_fdiv,   args, nargs, SHOW_ALL);
  do_N_binary("fdiv_",  insn_fdiv_,  args, nargs, SHOW_ALL);

  do_N_binary("fdivs",  insn_fdivs,  args, nargs, SHOW_ALL);
  do_N_binary("fdivs_", insn_fdivs_, args, nargs, SHOW_ALL);

  do_N_binary("fmul",   insn_fmul,   args, nargs, SHOW_ALL);
  do_N_binary("fmul_",  insn_fmul_,  args, nargs, SHOW_ALL);

  do_N_binary("fmuls",  insn_fmuls,  args, nargs, SHOW_ALL);
  do_N_binary("fmuls_", insn_fmuls_, args, nargs, SHOW_ALL);

  do_N_binary("fsub",   insn_fsub,   args, nargs, SHOW_ALL);
  do_N_binary("fsub_",  insn_fsub_,  args, nargs, SHOW_ALL);

  do_N_binary("fsubs",  insn_fsubs,  args, nargs, SHOW_ALL);
  do_N_binary("fsubs_", insn_fsubs_, args, nargs, SHOW_ALL);

  //do_N_unary(fcfid, SHOW_ALL);
  //do_N_unary(fcfid_, SHOW_ALL);

  //do_N_unary(fctid, SHOW_ALL);
  //do_N_unary(fctid_, SHOW_ALL);

  //do_N_unary(fctidz, SHOW_ALL);
  //do_N_unary(fctidz_, SHOW_ALL);

  do_N_unary("fctiw",  insn_fctiw,  args, nargs, 0xFFFFFFFF00000000ULL);
  do_N_unary("fctiw_", insn_fctiw_, args, nargs, 0xFFFFFFFF00000000ULL);

  do_N_unary("fctiwz",  insn_fctiwz,  args, nargs, 0xFFFFFFFF00000000ULL);
  do_N_unary("fctiwz_", insn_fctiwz_, args, nargs, 0xFFFFFFFF00000000ULL);

  do_N_ternary("fmadd",    insn_fmadd,    macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fmadd_",   insn_fmadd_,   macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fmadds",   insn_fmadds,   macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fmadds_",  insn_fmadds_,  macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fmsub",    insn_fmsub,    macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fmsub_",   insn_fmsub_,   macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fmsubs",   insn_fmsubs,   macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fmsubs_",  insn_fmsubs_,  macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fnmadd",   insn_fnmadd,   macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fnmadd_",  insn_fnmadd_,  macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fnmadds",  insn_fnmadds,  macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fnmadds_", insn_fnmadds_, macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fnmsub",   insn_fnmsub,   macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fnmsub_",  insn_fnmsub_,  macArgs, nMacArgs, SHOW_ALL);

  do_N_ternary("fnmsubs",  insn_fnmsubs,  macArgs, nMacArgs, SHOW_ALL);
  do_N_ternary("fnmsubs_", insn_fnmsubs_, macArgs, nMacArgs, SHOW_ALL);

  //do_N_unary(fre, SHOW_ALL);
  //do_N_unary(fre_, SHOW_ALL);

  do_N_unary("fres",  insn_fres,  args, nargs, 0x000001FFFFFFFFFFULL);
  do_N_unary("fres_", insn_fres_, args, nargs, 0x000001FFFFFFFFFFULL);

  do_N_unary("frsqrte",  insn_frsqrte,  args, nargs, SHOW_ALL);
  do_N_unary("frsqrte_", insn_frsqrte_, args, nargs, SHOW_ALL);

  // do_N_unary("frsqrtes",  insn_frsqrtes,  args, nargs, SHOW_ALL);
  // do_N_unary("frsqrtes_", insn_frsqrtes_, args, nargs, SHOW_ALL);

  do_N_unary("frsp",  insn_frsp,  args, nargs, SHOW_ALL);
  do_N_unary("frsp_", insn_frsp_, args, nargs, SHOW_ALL);

  do_N_ternary("fsel",  insn_fsel,  args, nargs, SHOW_ALL);
  do_N_ternary("fsel_", insn_fsel_, args, nargs, SHOW_ALL);

  //do_N_unary(fsqrt, SHOW_ALL);
  //do_N_unary(fsqrt_, SHOW_ALL);

  //do_N_unary(fsqrts, SHOW_ALL);
  //do_N_unary(fsqrts_, SHOW_ALL);

  return 0;
}
