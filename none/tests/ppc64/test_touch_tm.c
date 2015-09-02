#include <stdio.h>

int main (void) {
#ifdef SUPPORTS_HTM
   /* Just get the compiler to generate each of the TM instructions
    * so we can verify that valgrind recognizes them.
    * For now, only the tbegin instruction does anything in valgrind.
    * The rest are just treated as NOPS.
    */
   __builtin_tabort (0);
#ifdef __PPC64__
   __builtin_tabortdc (0,0,0);
   __builtin_tabortdci (0,0,0);
#endif
   __builtin_tabortwc (0,0,0);
   __builtin_tabortwci (0,0,0);
   __builtin_tbegin (0);
   __builtin_tend (0);
   //   __builtin_tcheck (0);  tcheck not recognized by compiler
   __builtin_trechkpt ();  // not recognized by early HW
   __builtin_treclaim (0); // not recognized by early HW
   __builtin_tsr (0);
#else
   printf ("No HTM support.");
#endif
   return 0;
}
