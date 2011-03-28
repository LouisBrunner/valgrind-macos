
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, no race.  Parent and child both modify x and
   use the hardware bus lock. */

#undef PLAT_ppc64_aix5
#undef PLAT_ppc32_aix5
#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux
#undef PLAT_s390x_linux

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
#elif defined(__linux__) && defined(__s390x__)
#  define PLAT_s390x_linux 1
#endif

#if defined(PLAT_amd64_linux) || defined(PLAT_x86_linux) \
    || defined(PLAT_amd64_darwin) || defined(PLAT_x86_darwin)
#  define INC(_lval,_lqual) \
      __asm__ __volatile__ ( \
      "lock ; incl (%0)" : /*out*/ : /*in*/"r"(&(_lval)) : "memory", "cc" )
#elif defined(PLAT_ppc32_linux) || defined(PLAT_ppc64_linux) \
      || defined(PLAT_ppc32_aix5) || defined(PLAT_ppc64_aix5)
#  define INC(_lval,_lqual)               \
   __asm__ __volatile__(                  \
      "1:\n"                              \
      "        lwarx 15,0,%0\n"           \
      "        addi 15,15,1\n"            \
      "        stwcx. 15,0,%0\n"          \
      "        bne- 1b\n"                 \
      : /*out*/ : /*in*/ "b"(&(_lval))    \
      : /*trash*/ "r15", "cr0", "memory"  \
   )
#elif defined(PLAT_arm_linux)
#  define INC(_lval,_lqual) \
  __asm__ __volatile__( \
      "1:\n"                                 \
      "        ldrex r8, [%0, #0]\n"         \
      "        add   r8, r8, #1\n"           \
      "        strex r9, r8, [%0, #0]\n"     \
      "        cmp   r9, #0\n"               \
      "        bne   1b\n"                   \
      : /*out*/ : /*in*/ "r"(&(_lval))       \
      : /*trash*/ "r8", "r9", "cc", "memory" \
  );
#elif defined(PLAT_s390x_linux)
#  define INC(_lval,_lqual) \
   __asm__ __volatile__( \
      "1: l     0,%0\n"                            \
      "   lr    1,0\n"                             \
      "   ahi   1,1\n"                             \
      "   cs    0,1,%0\n"                          \
      "   jl    1b\n"                              \
      : "+m" (_lval) :: "cc", "1","2" \
   )
#else
#  error "Fix Me for this platform"
#endif


int x = 0;

void* child_fn ( void* arg )
{
   INC(x, "childfn");
   return NULL;
}

int main ( void )
{
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   INC(x, "main");

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   printf("x = %d\n", x);
   return 0;
}
