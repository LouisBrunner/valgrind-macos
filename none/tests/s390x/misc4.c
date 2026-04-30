#include <stdio.h>

/* -- Count leading/trailing zeros -- */

#define INSNS                                                                  \
   XTEST(b968, clzg);                                                          \
   XTEST(b969, ctzg);

#define XTEST(opcode, insn)                                                    \
   static void test_##insn(unsigned long val)                                  \
   {                                                                           \
      unsigned long result;                                                    \
                                                                               \
      __asm__(".insn rre,0x" #opcode "0000,%[out], %[in]"                      \
              : [out] "=d"(result)                                             \
              : [in] "d"(val)                                                  \
              :);                                                              \
                                                                               \
      printf("\t%016lx -> %lu\n", val, result);                                \
   }

INSNS;

#undef XTEST
#define XTEST(opcode, insn)                                                    \
   do {                                                                        \
      puts(#insn);                                                             \
      test_##insn(-1);                                                         \
      test_##insn(0);                                                          \
      test_##insn(1);                                                          \
      test_##insn(1UL << 47);                                                  \
      test_##insn(1UL << 63);                                                  \
      test_##insn(0x0a00000000000070);                                         \
   } while (0)

static void test_all_count(void) { INSNS; }

#undef XTEST
#undef INSNS

/* -- Bit deposit/extract -- */

#define INSNS                                                                  \
   XTEST(b96c, bextg);                                                         \
   XTEST(b96d, bdepg);

#define XTEST(opcode, insn)                                                    \
   static void test_##insn(unsigned long a, unsigned long b)                   \
   {                                                                           \
      unsigned long result;                                                    \
      __asm__(".insn rrf,0x" #opcode "0000,%[out],%[a],%[b],0"                 \
              : [out] "=d"(result)                                             \
              : [a] "d"(a), [b] "d"(b)                                         \
              :);                                                              \
                                                                               \
      printf("\t%016lx %016lx -> %016lx\n", a, b, result);                     \
   }

INSNS;

#undef XTEST
#undef INSNS

static void test_all_bitx(void)
{
   puts("bextg");
   test_bextg(-1, -1);
   test_bextg(42, 0);
   test_bextg(0, 0xaffecafebabefea0);
   test_bextg(0xbedd49b39c58558a, 0xcef75cddae16e548);
   test_bextg(1, 1);

   puts("bdepg");
   test_bdepg(-1, -1);
   test_bdepg(42, 0);
   test_bextg(0, 0xaffecafebabefea0);
   test_bdepg(0xbedd49b39c58558a, 0xcef75cddae16e548);
   test_bdepg(1UL << 63, 1);
}

/* -- Load indexed address -- */

enum {
   DISPMAX = 0x7ffff,
   DISPMIN = -0x80000,
};

#define INSNS                                                                  \
   XTEST(e3, 60, lxab);                                                        \
   XTEST(e3, 62, lxah);                                                        \
   XTEST(e3, 64, lxaf);                                                        \
   XTEST(e3, 66, lxag);                                                        \
   XTEST(e3, 68, lxaq);                                                        \
   XTEST(e3, 61, llxab);                                                       \
   XTEST(e3, 63, llxah);                                                       \
   XTEST(e3, 65, llxaf);                                                       \
   XTEST(e3, 67, llxag);                                                       \
   XTEST(e3, 69, llxaq);

#define XXTEST(op1, op2, insn, disp)                                           \
   static void test_##insn##_##disp(unsigned int x, unsigned long b)           \
   {                                                                           \
      unsigned long result;                                                    \
      __asm__(".insn rxy,0x" #op1 "00000000" #op2 ",%[out],%[d](%[x],%[b])"    \
              : [out] "=d"(result)                                             \
              : [d] "i"(disp), [x] "a"(x), [b] "a"(b)                          \
              :);                                                              \
      printf("\t%7s(%08x,%016lx) -> %016lx\n", #disp, x, b, result);           \
                                                                               \
      __asm__(".insn rxy,0x" #op1 "00000000" #op2 ",%[out],%[d](0,%[b])"       \
              : [out] "=d"(result)                                             \
              : [d] "i"(disp), [b] "a"(b)                                      \
              :);                                                              \
      printf("\t%7s(%08x,%016lx) -> %016lx\n", #disp, 0, b, result);           \
                                                                               \
      __asm__(".insn rxy,0x" #op1 "00000000" #op2 ",%[out],%[d](%[x],0)"       \
              : [out] "=d"(result)                                             \
              : [d] "i"(disp), [x] "a"(x)                                      \
              :);                                                              \
      printf("\t%7s(%08x,%016lx) -> %016lx\n", #disp, x, 0UL, result);         \
   }

#define XTEST(op1, op2, insn)                                                  \
   XXTEST(op1, op2, insn, DISPMAX);                                            \
   XXTEST(op1, op2, insn, 0);                                                  \
   XXTEST(op1, op2, insn, DISPMIN);

INSNS;

#undef XXTEST
#undef XTEST

#define XTEST(op1, op2, insn)                                                  \
   do {                                                                        \
      puts(#insn);                                                             \
      test_##insn##_DISPMAX(0x7fffffff, -1ULL);                                \
      test_##insn##_0(311, 1ULL < 60);                                         \
      test_##insn##_DISPMIN(0x80000000, 42);                                   \
   } while (0)

static void test_all_load_indexed(void) { INSNS; }

#undef XTEST
#undef INSNS

int main(void)
{
   test_all_count();
   test_all_bitx();
   test_all_load_indexed();
   return 0;
}
