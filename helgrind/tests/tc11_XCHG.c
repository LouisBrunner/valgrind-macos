
#include "config.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Simple test program, no race.  Parent and child both modify x and
   use the hardware bus lock (implicitly, since XCHG r,m on x86/amd64
   does not require an explicit LOCK prefix.). */

#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux
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
#  define XCHG_M_R(_addr,_lval) \
     __asm__ __volatile__( \
        "xchgl %0, %1" \
        : /*out*/ "+r"(_lval) \
        : /*in*/  "m"(_addr) \
        : "memory", "cc" \
     )
#  define XCHG_M_R_with_redundant_LOCK(_addr,_lval) \
     __asm__ __volatile__( \
        "lock xchgl %0, %1" \
        : /*out*/ "+r"(_lval) \
        : /*in*/  "m"(_addr) \
        : "memory", "cc" \
     )

#elif defined(PLAT_s390x_linux)
#  define XCHG_M_R(_addr,_lval)                              \
     do {                                                    \
        __asm__ __volatile__(                                \
           "0: l   0,%[global]\n\t"                          \
           "   cs  0,%[local],%[global]\n\t"                 \
           "   bne 0b\n\t"                                   \
           "   lr  %[local],0\n\t"                           \
           : /*out*/ [global]"+m"(_addr), [local]"+d"(_lval) \
           : /*in*/                                          \
           : "0", "memory", "cc"                             \
        );                                                   \
     } while (0)

#  define XCHG_M_R_with_redundant_LOCK(_addr,_lval) \
      XCHG_M_R(_addr,_lval)

#elif defined(PLAT_mips32_linux) || defined(PLAT_mips64_linux)
#  define XCHG_M_R(_addr,_lval)                              \
     __asm__ __volatile__(                                   \
        "move $12, %2\n"                                     \
        "move $13, %1\n"                                     \
        "ll $14, 0($13)\n"                                   \
        "sc $12, 0($13)\n"                                   \
        "move %0, $14\n"                                     \
        : /*out*/ "=r"(_lval)                                \
        : /*in*/  "r"(&_addr), "r"(_lval)                    \
        : "$12", "$13", "$14", "memory", "cc"                \
     )

#  define XCHG_M_R_with_redundant_LOCK(_addr,_lval) \
      XCHG_M_R(_addr,_lval)

#elif defined(PLAT_ppc32_linux) || defined(PLAT_ppc64_linux) \
      || defined(PLAT_arm_linux) || defined(PLAT_arm64_linux)
#  if defined(HAVE_BUILTIN_ATOMIC)
#    define XCHG_M_R(_addr,_lval)                                           \
        do {                                                                \
          int tmp;                                                          \
          while ((tmp = *(int*)(& _addr)),                                  \
                 ! __sync_bool_compare_and_swap((int*)&_addr, tmp, _lval))  \
            ;                                                               \
          _lval = tmp;                                                      \
        } while (0)
#  else
#    warning "XCHG_M_R() implementation is missing. Either" \
             "provide one or use a newer gcc version."
#    define XCHG_M_R(_addr,_lval) \
        do { int tmp = *(int*)(& _addr); \
             *(int*)(& _addr) = (_lval); \
             _lval = tmp; \
        } while (0)
#  endif
#  define XCHG_M_R_with_redundant_LOCK(_addr,_lval) \
      XCHG_M_R(_addr,_lval)

#else
#  error "Unsupported architecture"

#endif

int x = 0;

void* child_fn ( void* arg )
{
   int v = 12345;
   XCHG_M_R_with_redundant_LOCK(x, v);
   assert(v == 0 || v == 6789);
   return NULL;
}

int main ( void )
{
   int v = 6789;
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   XCHG_M_R(x, v);
   assert(v == 0 || v == 12345);

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   if (v == 0 || v == 12345)
      printf("success\n");
   else
      printf("failure\n");

   return v;
}
