#include <stdio.h>

static const char *const cmp_result_str[] = {
   "==", "<", ">", "??"
};

#define TEST_CxB(insn, fmt, mode, v1, v2)        \
   do {                                          \
      int cc;                                    \
                                                 \
      __asm__ volatile(insn " %[r1],%[r2]\n\t"       \
                       "ipm   %[psw]\n\t"            \
                       "srl   %[psw],28\n\t"         \
                       : [psw]"=d"(cc)               \
                       : [r1]"f"(v1), [r2]mode(v2)   \
                       : "cc");                      \
      printf("%-6s" fmt " %s " fmt "\n",             \
             insn ":", v1, cmp_result_str[cc], v2);  \
   } while (0)

/* Test BFP comparison for 32/64/128-bit. */

static void cebr(float a, float b)
{
   TEST_CxB("cebr", "%g", "f", a, b);
}

static void ceb(float a, float b)
{
   TEST_CxB("ceb", "%g", "R", a, b);
}

static void cdbr(double a, double b)
{
   TEST_CxB("cdbr", "%g", "f", a, b);
}

static void cdb(double a, double b)
{
   TEST_CxB("cdb", "%g", "R", a, b);
}

static void cxbr(long double a, long double b)
{
   TEST_CxB("cxbr", "%Lg", "f", a, b);
}

static void kebr(float a, float b)
{
   TEST_CxB("kebr", "%g", "f", a, b);
}

static void keb(float a, float b)
{
   TEST_CxB("keb", "%g", "R", a, b);
}

static void kdbr(double a, double b)
{
   TEST_CxB("kdbr", "%g", "f", a, b);
}

static void kdb(double a, double b)
{
   TEST_CxB("kdb", "%g", "R", a, b);
}

static void kxbr(long double a, long double b)
{
   TEST_CxB("kxbr", "%Lg", "f", a, b);
}

static void do_compare(float a, float b)
{
   cebr(a, b);
   ceb(a, b);
   kebr(a, b);
   keb(a, b);
   cdbr((double) a, (double) b);
   cdb((double) a, (double) b);
   kdbr((double) a, (double) b);
   kdb((double) a, (double) b);
   cxbr((long double) a, (long double) b);
   kxbr((long double) a, (long double) b);
}

int main(void)
{
   float inf = 1.f / 0.;
   float neg_inf = -1.f / 0.;

   do_compare(3.14f, 3.14f);
   do_compare(-2.78f, 2.78f);
   do_compare(inf, inf);
   do_compare(inf, neg_inf);
   do_compare(neg_inf, neg_inf);
   do_compare(inf, 1.f);
   do_compare(neg_inf, -1.f);
   do_compare(1.f / inf, -1.f / inf);
   return 0;
}
