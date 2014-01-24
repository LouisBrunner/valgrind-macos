#include <stdio.h>
#include <stdint.h>

#ifndef __powerpc64__
typedef uint32_t HWord_t;
#else
typedef uint64_t HWord_t;
#endif

typedef void (*test_func_t)();

typedef struct test_table {
   test_func_t func;
   char *name;
} test_table_t;

static uint32_t values[] = {
   0x6efbcfdf,
   0xd16c2fd4,
   0xf9dc1743,
   0xa5aa0bd4,
   0x6c8f0c14,
   0x69a24188,
   0x53b57f1b,
};

register HWord_t r27 asm("r27");
register HWord_t r28 asm("r28");
register HWord_t r29 asm("r29");
register HWord_t r30 asm("r30");
register HWord_t r31 asm("r31");

register HWord_t r14 asm("r14");

HWord_t temp[5];

#ifdef __powerpc64__

#define SAVE_REGS(addr)                       \
   asm volatile(                              \
   "	std   27, 0(%0)   \n"                   \
   "	std   28, 8(%0)   \n"                   \
   "	std   29, 16(%0)  \n"                   \
   "	std   30, 24(%0)  \n"                   \
   "	std   31, 32(%0)  \n"                   \
   ::"b"(addr))

#define RESTORE_REGS(addr)                    \
   asm volatile(                              \
   "	ld    27, 0(%0)   \n"                   \
   "	ld    28, 8(%0)   \n"                   \
   "	ld    29, 16(%0)  \n"                   \
   "	ld    30, 24(%0)  \n"                   \
   "	ld    31, 32(%0)  \n"                   \
   ::"b"(addr))

#else /* !__powerpc64__ */

#define SAVE_REGS(addr)                       \
   asm volatile(                              \
   "	stw   27, 0(%0)   \n"                   \
   "	stw   28, 4(%0)   \n"                   \
   "	stw   29, 8(%0)   \n"                   \
   "	stw   30, 12(%0)  \n"                   \
   "	stw   31, 16(%0)  \n"                   \
   ::"b"(addr))

#define RESTORE_REGS(addr)                    \
   asm volatile(                              \
   "	lwz   27, 0(%0)   \n"                   \
   "	lwz   28, 4(%0)   \n"                   \
   "	lwz   29, 8(%0)   \n"                   \
   "	lwz   30, 12(%0)  \n"                   \
   "	lwz   31, 16(%0)  \n"                   \
   ::"b"(addr))

#endif /* __powerpc64__ */

/*
 * gcc is not happy if we modify r31 (the frame pointer) behind its back
 * so we omit it
 */
static void __attribute__((optimize("-fomit-frame-pointer"))) test_lmw(void)
{
   r14 = (HWord_t)values;

   /* save r27 - r31 */
   SAVE_REGS(temp);

   /* load r27 - r31 */
   asm volatile(
      "	lmw	%r27, 0(%r14)	\n");

#ifdef __powerpc64__
   printf("lmw => %016lx %016lx %016lx %016lx %016lx\n",
#else
   printf("lmw => %08x %08x %08x %08x %08x\n",
#endif
          r27, r28, r29, r30, r31);

   /*
    * test load multiple with nonzero immediate offset
    * load the last two values into r30 - r31.
    * r27 - r29 should remain the same
    */
   asm volatile(
      "	lmw	%r30, 20(%r14)	\n");

#ifdef __powerpc64__
   printf("lmw => %016lx %016lx %016lx %016lx %016lx\n",
#else
   printf("lmw => %08x %08x %08x %08x %08x\n",
#endif
          r27, r28, r29, r30, r31);

   /* restore r27 - r31 */
   RESTORE_REGS(temp);
}

/*
 * gcc is not happy if we modify r31 (the frame pointer) behind its back
 * so we omit it
 */
static void __attribute__((optimize("-fomit-frame-pointer"))) test_stmw(void)
{
   uint32_t result[7] = { 0 };
   int i;

   SAVE_REGS(temp);

#ifdef __powerpc64__
   asm volatile(
   "	lwz   27, 0(%0)   \n"                   \
   "	lwz   28, 4(%0)   \n"                   \
   "	lwz   29, 8(%0)   \n"                   \
   "	lwz   30, 12(%0)  \n"                   \
   "	lwz   31, 16(%0)  \n"                   \
   ::"b"(values));
#else
   RESTORE_REGS(values);
#endif

   r14 = (HWord_t)&result;

   /* store r27 - r31 */
   asm volatile(
      "	stmw	%r27, 0(%r14)	\n");

   printf("stmw => ");
   for (i = 0; i < sizeof(result) / sizeof(result[0]); i++)
      printf("%08x ", result[i]);

   printf("\n");

   /*
    * test store multiple with nonzero immediate offset
    * store r30 - r31 into the last two places in the array
    * the rest of the array should remain the same
    */
   asm volatile(
      "	stmw	%r30, 20(%r14)	\n");

   printf("stmw => ");
   for (i = 0; i < sizeof(result) / sizeof(result[0]); i++)
      printf("%08x ", result[i]);

   printf("\n");

   RESTORE_REGS(temp);
}

static test_table_t all_tests[] = {
   { &test_lmw,
     "Test Load Multiple instruction" },
   { &test_stmw,
     "Test Store Multiple instruction" },
   { NULL, NULL },
};

int main(void)
{
   test_func_t func;
   int i = 0;

   while ((func = all_tests[i].func)) {
      (*func)();
      i++;
   }

   return 0;
}
