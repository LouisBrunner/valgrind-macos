#include <assert.h>
#include <stdlib.h>
#include <string.h>

// This tests that the suppression for the leak in setenv() works.  See bug
// 188572.

int main(void)
{
   char* val1 = "x";
   char* val2 = "xx";
   char* val3 = "xxx";

   setenv("MYVAR", val1, /*overwrite*/0); // makes a copy which is later leaked
   assert( 0 == strcmp(getenv("MYVAR"), val1) );

   setenv("MYVAR", val2, /*overwrite*/1); // makes a copy which is later leaked
   assert( 0 == strcmp(getenv("MYVAR"), val2) );

   setenv("MYVAR", val3, /*overwrite*/0); // doesn't overwrite MYVAR=val2
   assert( 0 == strcmp(getenv("MYVAR"), val2) );

   putenv("MYVAR=xxxx");                  // no leak for putenv()
   assert( 0 == strcmp(getenv("MYVAR"), "xxxx") );

   unsetenv("MYVAR");
   assert( NULL == getenv("MYVAR") );

   return 0;
}

