
/* FIXME: this is basically a bad test as it is scheduling-
   sensitive.  Sometimes the output is:

   child: new value 6
   child: new value 10
   done, x = 10

   and sometimes

   child: new value 10
   done, x = 10
*/

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Simple test program, no race.  Parent writes atomically to a counter
   whilst child reads it.  When counter reaches a prearranged value, 
   child joins back to parent.  Parent (writer) uses hardware bus lock;
   child is only reading and so does not need to use a bus lock. */

#undef PLAT_ppc64_aix5
#undef PLAT_ppc32_aix5
#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux

#if defined(_AIX) && defined(__64BIT__)
#  define PLAT_ppc64_aix5 1
#elif defined(_AIX) && !defined(__64BIT__)
#  define PLAT_ppc32_aix5 1
#elif defined(__APPLE__) && defined(__i386__)
#  define PLAT_x86_darwin 1
#elif defined(__APPLE__) && defined(__x86_64__)
#  define PLAT_amd64_darwin 1
#elif defined(__linux__) && defined(__i386__)
#  define PLAT_x86_linux 1
#elif defined(__linux__) && defined(__x86_64__)
#  define PLAT_amd64_linux 1
#elif defined(__linux__) && defined(__powerpc__) && !defined(__powerpc64__)
#  define PLAT_ppc32_linux 1
#elif defined(__linux__) && defined(__powerpc__) && defined(__powerpc64__)
#  define PLAT_ppc64_linux 1
#elif defined(__linux__) && defined(__arm__)
#  define PLAT_arm_linux 1
#endif


#if defined(PLAT_amd64_linux) || defined(PLAT_x86_linux) \
    || defined(PLAT_amd64_darwin) || defined(PLAT_x86_darwin)
#  define INC(_lval,_lqual)	     \
      __asm__ __volatile__ ( \
      "lock ; incl (%0)" : /*out*/ : /*in*/"r"(&(_lval)) : "memory", "cc" )
#elif defined(PLAT_ppc32_linux) || defined(PLAT_ppc64_linux) \
      || defined(PLAT_ppc32_aix5) || defined(PLAT_ppc64_aix5)
#  define INC(_lval,_lqual)		  \
   __asm__ __volatile__(                  \
      "L1xyzzy1" _lqual ":\n"             \
      "        lwarx 15,0,%0\n"           \
      "        addi 15,15,1\n"            \
      "        stwcx. 15,0,%0\n"          \
      "        bne- L1xyzzy1" _lqual      \
      : /*out*/ : /*in*/ "b"(&(_lval))    \
      : /*trash*/ "r15", "cr0", "memory"  \
   )
#elif defined(PLAT_arm_linux)
#  define INC(_lval,_lqual) \
  __asm__ __volatile__( \
      "L1xyzzy1" _lqual ":\n"                \
      "        ldrex r8, [%0, #0]\n"         \
      "        add   r8, r8, #1\n"           \
      "        strex r9, r8, [%0, #0]\n"     \
      "        cmp   r9, #0\n"               \
      "        bne L1xyzzy1" _lqual          \
      : /*out*/ : /*in*/ "r"(&(_lval))       \
      : /*trash*/ "r8", "r9", "cc", "memory" \
  );
#else
#  error "Fix Me for this platform"
#endif



#define LIMIT 10

volatile int x = 0;

void* child_fn ( void* arg )
{
   int q = 0;
   int oldx = 0;
   int ctr = 0;
   while (1) {
      q = (x >= LIMIT);
      if (x != oldx) {
         oldx = x;
         printf("child: new value %d\n", oldx);
         fflush(stdout);
      }
      if (q) break;
      /* Make sure the parent doesn't starve.  Seems to be a problem
	 on very slow machines. */
      ctr++;
      if (ctr == 2000000) sleep(1);
   }
   return NULL;
}

int main ( void )
{
   pthread_t child;
   int i;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   for (i = 0; i < LIMIT; i++) {
      INC(x, "main");
      if (i == 5) sleep(1); /* make sure child doesn't starve */
   }

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   printf("done, x = %d\n", x);

   return 0;
}
