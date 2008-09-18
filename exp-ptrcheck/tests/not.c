#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"

int main(void)
{
   #include "arith_include2.c"
   
   // NOT =========================================================
   ui(~, p,  n);      // det, det

   ui(~, up, n);      // det, det

   ui(~, un, n);      // det, det

   ui(~, n,  n);      // det, det

   ui(~, nn, n);      // det, det

   return 0;
}
