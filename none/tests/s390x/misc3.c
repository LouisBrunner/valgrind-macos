#include <stdio.h>

/* -- Logical instructions -- */

#define TEST_GENERATE(opcode,insn)                              \
   static void test_##insn(unsigned long a, unsigned long b)    \
   {                                                            \
      unsigned long out = 0xdecaffee42424242;                   \
      int cc;                                                   \
                                                                \
      __asm__(                                                  \
         "cr    0,0\n\t"               /* Clear CC */           \
         ".insn rrf,0x" #opcode "0000,%[out],%[a],%[b],0\n\t"   \
         "ipm   %[cc]\n\t"                                      \
         "srl   %[cc],28\n"                                     \
         : [out] "+d" (out),                                    \
           [cc] "=d" (cc)                                       \
         : [a] "d" (a),                                         \
           [b] "d" (b)                                          \
         : "cc");                                               \
                                                                \
      printf("\t%016lx %016lx -> %016lx cc=%d\n",               \
             a, b, out, cc);                                    \
   }

#define TEST_EXEC(opcode,insn)                             \
   do {                                                    \
      puts(#insn);                                         \
      test_##insn(0, 0);                                   \
      test_##insn(0, -1);                                  \
      test_##insn(-1, 0);                                  \
      test_##insn(-1, -1);                                 \
      test_##insn(0x012345678abcdef, 0);                   \
      test_##insn(0x012345678abcdef, -1);                  \
      test_##insn(0x55555555aaaaaaaa, 0xaaaaaaaa55555555); \
   } while (0)

#define INSNS                                    \
   XTEST(b9f5,ncrk);                             \
   XTEST(b9e5,ncgrk);                            \
   XTEST(b974,nnrk);                             \
   XTEST(b964,nngrk);                            \
   XTEST(b976,nork);                             \
   XTEST(b966,nogrk);                            \
   XTEST(b977,nxrk);                             \
   XTEST(b967,nxgrk);                            \
   XTEST(b975,ocrk);                             \
   XTEST(b965,ocgrk);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_logical_insns()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}
#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC


/* -- Full population count -- */

static void test_popcnt(unsigned long op2)
{
   unsigned long result;
   int cc;

   __asm__(".insn   rrf,0xb9e10000,%[result],%[op2],8,0\n\t"
           "ipm     %[cc]\n\t"
           "srl     %[cc],28\n"
           : [result]"=d" (result),
             [cc]"=d" (cc)
           : [op2]"d" (op2)
           : "cc");
   printf("\t%016lx -> %2lu cc=%d\n", op2, result, cc);
}

static int test_all_popcnt()
{
   puts("popcnt");
   test_popcnt(0);
   test_popcnt(1);
   test_popcnt(0x8000000000000000);
   test_popcnt(-1UL);
   test_popcnt(0xff427e3800556bcd);
   return 0;
}

/* -- Select -- */

#define TEST_GENERATE(opcode,insn)                              \
   static void test_##insn(unsigned long a, unsigned long b)    \
   {                                                            \
      unsigned long out0 = 0x0cafebad0badcafe;                  \
      unsigned long out1 = 0x0badcafe0cafebad;                  \
                                                                \
      __asm__(                                                  \
         "cr    0,0\n\t"               /* Clear CC */           \
         ".insn rrf,0x" #opcode "0000,%[out0],%[a],%[b],8\n\t"  \
         ".insn rrf,0x" #opcode "0000,%[out1],%[a],%[b],7\n\t"  \
         : [out0] "+d" (out0),                                  \
           [out1] "+d" (out1)                                   \
         : [a] "d" (a),                                         \
           [b] "d" (b)                                          \
         : "cc");                                               \
                                                                \
      printf("\t%016lx %016lx -> %016lx %016lx\n",              \
             a, b, out0, out1);                                 \
   }

#define TEST_EXEC(opcode,insn)                             \
   do {                                                    \
      puts(#insn);                                         \
      test_##insn(-1, 0);                                  \
      test_##insn(0, -1);                                  \
      test_##insn(0x1234567890abcdef, 0xfedcba9876543210); \
   } while (0)

#define INSNS                                    \
   XTEST(b9f0,selr);                             \
   XTEST(b9e3,selgr);                            \
   XTEST(b9c0,selfhr);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_select()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}
#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC


/* -- Move right to left -- */

static void test_mvcrl(char *to, char *from, size_t len)
{
   len -= 1;
   __asm__("lgr    0,%[len]\n\t"
           ".insn  sse,0xe50a00000000,%[to],%[from]\n\t"
           : [to] "+Q" (*(char (*)[len]) to)
           : [from] "Q" (*(char (*)[len]) from),
             [len] "d" (len)
           : );
}

static void test_all_mvcrl()
{
   static const char pattern[] =
      "abcdefghijklmnopqrstuvwxyz-0123456789.ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   char buf[4 * sizeof(pattern) - 2];

   test_mvcrl(buf, (char *) pattern, sizeof(pattern));
   test_mvcrl(buf + sizeof(pattern) - 1, buf, sizeof(pattern));
   test_mvcrl(buf + 2 * sizeof(pattern) - 2, buf, 2 * sizeof(pattern) - 1);
   test_mvcrl(buf + 32, buf + 10, 63);
   test_mvcrl(buf + 2, buf + 1, 256);
   test_mvcrl(buf + 254, buf + 256, 2);
   puts("mvcrl");
   for (int i = 0; i < 256; i += 64) {
      printf("\t%.64s\n", buf + i);
   }
}


int main()
{
   test_all_logical_insns();
   test_all_popcnt();
   test_all_select();
   test_all_mvcrl();
   return 0;
}
