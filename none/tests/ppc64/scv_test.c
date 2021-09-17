#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef HAS_ISA_3_00
#include <asm/unistd.h>
#include <sys/auxv.h>

#define ASM_INPUT_0 "0" (r0)

#define INTERNAL_SYSCALL_SCV(name, nr) \
  ({                                                                    \
    register long int r0  __asm__ ("r0");                               \
    register long int r3  __asm__ ("r3");                               \
    register long int r4  __asm__ ("r4");                               \
    register long int r5  __asm__ ("r5");                               \
    register long int r6  __asm__ ("r6");                               \
    register long int r7  __asm__ ("r7");                               \
    register long int r8  __asm__ ("r8");                               \
    r0=name;                                                            \
    __asm__ __volatile__                                                \
      (".machine power9\n"                                              \
       "scv 0\n\t"                                                      \
       "0:"                                                             \
       : "=&r" (r0),                                                    \
         "=&r" (r3), "=&r" (r4), "=&r" (r5),                            \
         "=&r" (r6), "=&r" (r7), "=&r" (r8)                             \
       : ASM_INPUT_##nr                                                 \
       : "r9", "r10", "r11", "r12",                                     \
         "cr0", "cr1", "cr5", "cr6", "cr7", "xer",                      \
         "lr", "ctr", "memory");                                        \
    r3;						                        \
  })

#define INTERNAL_SYSCALL_SC(name, nr) \
  ({                                                                    \
    register long int r0  __asm__ ("r0");                               \
    register long int r3  __asm__ ("r3");                               \
    register long int r4  __asm__ ("r4");                               \
    register long int r5  __asm__ ("r5");                               \
    register long int r6  __asm__ ("r6");                               \
    register long int r7  __asm__ ("r7");                               \
    register long int r8  __asm__ ("r8");                               \
    r0=name;                                                            \
    __asm__ __volatile__                                                \
      ("sc\n\t"                                                         \
       "mfcr %0\n\t"                                                    \
       "0:"                                                             \
       : "=&r" (r0),                                                    \
         "=&r" (r3), "=&r" (r4), "=&r" (r5),                            \
         "=&r" (r6), "=&r" (r7), "=&r" (r8)                             \
       : ASM_INPUT_##nr                                                 \
       : "r9", "r10", "r11", "r12",                                     \
         "xer", "cr0", "ctr", "memory");                                \
    r0 & (1 << 28) ? -r3 : r3; \
  })

#define PPC_FEATURE2_SCV           0x00100000 /* scv syscall enabled */

#endif

int
main(void)
{

#ifdef HAS_ISA_3_00
  int result;
  unsigned long hwcaps2_val;

  result = INTERNAL_SYSCALL_SC(__NR_gettid, 0);

  if (result < 0) {
    printf("The sc instruction test unexpectedly failed\n");
    exit (-1);
  }

  hwcaps2_val = getauxval(AT_HWCAP2);

  if ((hwcaps2_val & PPC_FEATURE2_SCV) == PPC_FEATURE2_SCV) {
    /* system supports the scv instruction */
    result = INTERNAL_SYSCALL_SCV(__NR_gettid, 0);

    if (result < 0) {
      printf("The scv instruction test unexpectedly failed\n");
      exit (-1);
    }
  }

  printf("Success\n");
#else
  printf("HAS_ISA_3_00 not detected.\n");
#endif
  return 0;
}
