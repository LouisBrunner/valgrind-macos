/* This test case is for a FPU bug to do with lazy eflags updating that was
   fixed by commit 1.42 in coregrind/vg_from_ucode.c in the HEAD.  Thanks to
   Dominic Mazzoni <dmazzoni@aig.jpl.nasa.gov for the test case and the
   following information:

   Anyway, the error only occurs if you compile it with the options:

     gcc -O2 -mcpu=pentiumpro -march=pentiumpro

   However, the exact same error occurs whether I compile the program with
   gcc 2.96 (RedHat 7.3's version) or gcc 3.2.

   The correct output of the program is "0.000000".  When run under valgrind
   1.9.4, it outputs "1.000000".
*/

#include <stdio.h>

int main(int argc, char **argv)
{
   union {
      float a[2];
      int b[2];
   } u;

   u.a[0] = 0.0 / 0.0;
   u.a[1] = ((*u.b & 0x7FC00000) != 0x7FC00000);
   printf("%f\n", u.a[1]);

   return 0;
}

