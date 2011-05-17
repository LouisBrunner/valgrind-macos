#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// This file determines s390x features a processor supports.
//
// We return:
// - 0 if the machine matches the asked-for feature.
// - 1 if the machine does not.
// - 2 if the asked-for feature isn't recognised (this will be the case for
//     any feature if run on a non-s390x machine).
// - 3 if there was a usage error (it also prints an error message).

jmp_buf env;

#if defined(VGA_s390x)

void handle_sigill(int signum)
{
   longjmp(env, 1);
}

unsigned long long stfle(void)
{

   unsigned long long ret;

   signal(SIGILL, handle_sigill);
   if (setjmp(env)) {
      /* stfle not available: assume no facilities */
      return 0;
   } else {
      asm volatile("lghi 0, 0\n"
                   ".insn s,0xb2b00000,%0\n" /* stfle */
      : "=Q" (ret)::"0", "cc");
      return ret;
   }
}

static int go(char* cpu)
{
   unsigned long long facilities;
   unsigned long long match;

   facilities = stfle();

   if        (strcmp(cpu, "s390x-zarch") == 0 ) {
     match = (facilities & (1ULL << 62) && (facilities & (1ULL << 61)));
   } else if (strcmp(cpu, "s390x-n3") == 0 ) {
     match = (facilities & (1ULL << 63));
   } else if (strcmp(cpu, "s390x-stfle") == 0 ) {
     match = (facilities & (1ULL << 56));
   } else if (strcmp(cpu, "s390x-ldisp") == 0 ) {
     match = (facilities & (1ULL << 45) && (facilities & (1ULL << 44)));
   } else if (strcmp(cpu, "s390x-eimm") == 0 ) {
     match = (facilities & (1ULL << 42));
   } else if (strcmp(cpu, "s390x-stckf") == 0 ) {
     match = (facilities & (1ULL << 38));
   } else if (strcmp(cpu, "s390x-genins") == 0 ) {
     match = (facilities & (1ULL << 29));
   } else if (strcmp(cpu, "s390x-exrl") == 0 ) {
     match = (facilities & (1ULL << 28));
   } else {
     return 2;          // Unrecognised feature.
   }

   return match == 0;
}

#else

static int go(char* cpu)
{
   return 2;      // Feature not recognised (non-s390x machine!)
}

#endif


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if ( argc != 2 ) {
      fprintf( stderr, "usage: s390x_features <feature>\n" );
      exit(3);                // Usage error.
   }
   return go(argv[1]);
}
