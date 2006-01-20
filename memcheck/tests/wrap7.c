
#include <stdio.h>
#include "valgrind.h"

/* The simplest possible wrapping test: just call a wrapped function
   and check we run the wrapper instead.  Except: the wrapped
   function is in a different shared object.  This causes some
   additional complications on ppc64-linux, hence another test. */

extern void actual ( void );

/* The wrapper.  The function being wrapped is in a .so with soname
   "wrap7so.so". */
void I_WRAP_SONAME_FNNAME_ZU(wrap7soZdso,actual) ( void )
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("wrapper-pre\n");
   CALL_FN_v_v(fn);
   printf("wrapper-post\n");
}

/* --------------- */

int main ( void )
{
   printf("starting\n");
   actual();
   return 0;
}
