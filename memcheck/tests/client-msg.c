#include "valgrind.h"

void baz()
{
   VALGRIND_PRINTF_BACKTRACE("hello <> %s%%s world\n","<&>");
}

void bar()
{
   baz();
}

void foo()
{
     bar();
}

int main()
{
   foo();

   return 0;
}

