
#include <stdio.h>
#include <stdlib.h>
#include "basictypes.h"

__attribute__ ((noreturn))
void vex_assert_fail ( const Char* expr,
                       const Char* file, Int line, const Char* fn )
{
   fprintf(stderr, "\nvex: %s:%d (%s): Assertion `%s' failed.\n",
                   file, line, fn, expr );
   exit(1);
}

__attribute__ ((noreturn))
void panic ( Char* str )
{
   fprintf(stderr, "\nvex: the `impossible' happened:\n   %s\n", str);
   exit(1);
}
