
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, no race.  Parent and child both modify x and
   use the hardware bus lock. */

#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux
#undef PLAT_arm64_linux
#undef PLAT_s390x_linux
#undef PLAT_mips32_linux

#if defined(__APPLE__) && defined(__i386__)
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
#elif defined(__linux__) && defined(__arm__) && !defined(__aarch64__)
#  define PLAT_arm_linux 1
#elif defined(__linux__) && defined(__aarch64__) && !defined(__arm__)
#  define PLAT_arm64_linux 1
#elif defined(__linux__) && defined(__s390x__)
#  define PLAT_s390x_linux 1
#elif defined(__linux__) && defined(__mips__)
#  define PLAT_mips32_linux 1
#endif

#if defined(PLAT_amd64_linux) || defined(PLAT_x86_linux) \
    || defined(PLAT_amd64_darwin) || defined(PLAT_x86_darwin)
#  define INC(_lval,_lqual) \
      __asm__ __volatile__ ( \
      "lock ; incl (%0)" : /*out*/ : /*in*/"r"(&(_lval)) : "memory", "cc" )
#elif defined(PLAT_ppc32_linux) || defined(PLAT_ppc64_linux)
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
#elif defined(PLAT_arm64_linux)
#  define INC(_lval,_lqual) \
  __asm__ __volatile__( \
      "1:\n"                                 \
      "        ldxr  w8, [%0, #0]\n"         \
      "        add   w8, w8, #1\n"           \
      "        stxr  w9, w8, [%0, #0]\n"     \
      "        cmp   w9, #0\n"               \
      "        bne   1b\n"                   \
      : /*out*/ : /*in*/ "r"(&(_lval))       \
      : /*trash*/ "x8", "x9", "cc", "memory" \
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
#elif defined(PLAT_mips32_linux)
#  define INC(_lval,_lqual)                         \
     __asm__ __volatile__ (                         \
      "1:\n"                                        \
      "        move $8, %0\n"                       \
      "        ll $9, 0($8)\n"                      \
      "        addiu $9, $9, 1\n"                   \
      "        sc $9, 0($8)\n"                      \
      "        li $10, 1\n"                         \
      "        bne $9, $10, 1b\n"                   \
      "        nop\n"                               \
      : /*out*/ : /*in*/ "r"(&(_lval))              \
      : /*trash*/ "$8", "$9", "$10", "cc", "memory" \
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
