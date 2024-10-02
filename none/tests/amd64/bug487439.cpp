// This is more or less a copy/paste from the generated insn_sse2.c
// I didn't want to mess with the perl generator because
// GCC and clang don't agree on the asm syntax
// Using the rex prefix looks like a bug or misfeature in OpenJDK
// so I'm assuming that this is a one-off and not a general issue

#include <iostream>
#include <csetjmp>
#include <csignal>

union reg128_t {
  char sb[16];
  unsigned char ub[16];
  short sw[8];
  unsigned short uw[8];
  int sd[4];
  unsigned int ud[4];
  long long int sq[2];
  unsigned long long int uq[2];
  float ps[4];
  double pd[2];
} __attribute__ ((aligned (16)));

static sigjmp_buf catchpoint;

static void handle_sigill(int signum)
{
   siglongjmp(catchpoint, 1);
}

/* with redundant rex.W */
static void psllq_4(void)
{
   reg128_t arg1 = { .uq = { 0x0123456789abcdefULL, 0x0123456789abcdefULL } };
   reg128_t result0;
   char state[108];

   if (sigsetjmp(catchpoint, 1) == 0)
   {
      asm(
         "ffree %%st(7)\n"
         "ffree %%st(6)\n"
         "ffree %%st(5)\n"
         "ffree %%st(4)\n"
         "movlps %2, %%xmm1\n"
         "movhps %3, %%xmm1\n"
         // only GCC
         //".rex.W psllq $12, %%xmm1\n"
         // only clang
         //"data16 rex64 psllq $12, %mm1\n"
         ".byte 0x66,0x48,0x0f,0x73,0xf1,0x0c\n"
         "movlps %%xmm1, %0\n"
         "movhps %%xmm1, %1\n"
         "cld\n"
         : "=m" (result0.uq[0]), "=m" (result0.uq[1])
         : "m" (arg1.uq[0]), "m" (arg1.uq[1]), "m" (state[0])
         : "xmm1"
      );

      if (result0.uq[0] == 0x3456789abcdef000ULL && result0.uq[1] == 0x3456789abcdef000ULL )
      {
         std::cout << "psllq_4 ... ok\n";
      }
      else
      {
         std::cout << "psllq_4 ... not ok\n";
         std::cout << "  result0.uq[0] = " << result0.uq[0] << " (expected " << 0x3456789abcdef000ULL << ")\n";
         std::cout << "  result0.uq[1] = " << result0.uq[1] << " (expected " << 0x3456789abcdef000ULL << ")\n";
      }
   }
   else
   {
      std::cout << "psllq_4 ... failed\n";
   }

   return;
}

int main()
{
   signal(SIGILL, handle_sigill);
   psllq_4();
}
