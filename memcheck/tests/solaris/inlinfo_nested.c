/* Taken from memcheck/tests/inlinfo.c almost verbatim;
 * only main() renamed to main_nested().
 */

#include "../../memcheck.h"
#define INLINE static inline __attribute__((always_inline))

INLINE int fun_d(int argd) {
   static int locd = 0;
   if (argd > 0)
      locd += argd;
   return locd;
}

INLINE int fun_c(int argc) {
   static int locc = 0;
   locc += argc;
   return fun_d(locc);
}

INLINE int fun_b(int argb) {
   static int locb = 0;
   locb += argb;
   return fun_c(locb);
}

INLINE int fun_a(int arga) {
   static int loca = 0;
   loca += arga;
   return fun_b(loca);
}

__attribute__((noinline))
static int fun_noninline_m(int argm)
{
   return fun_d(argm);
}

__attribute__((noinline))
static int fun_noninline_o(int argo)
{
   static int loco = 0;
   if (argo > 0)
      loco += argo;
   return loco;
}

INLINE int fun_f(int argf) {
   static int locf = 0;
   locf += argf;
   return fun_noninline_o(locf);
}

INLINE int fun_e(int arge) {
   static int loce = 0;
   loce += arge;
   return fun_f(loce);
}

__attribute__((noinline))
static int fun_noninline_n(int argn)
{
   return fun_e(argn);
}


int main_nested(void) {
   int result;
   result = fun_a(result);
   VALGRIND_MAKE_MEM_UNDEFINED(&result, sizeof(result));
   result += fun_noninline_m(result);
   VALGRIND_MAKE_MEM_UNDEFINED(&result, sizeof(result));
   result += fun_d(result);
   VALGRIND_MAKE_MEM_UNDEFINED(&result, sizeof(result));
   result += fun_noninline_n(result);
   return 0;
}

