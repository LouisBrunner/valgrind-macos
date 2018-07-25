#include <features.h>
#include <fpu_control.h>
#include <signal.h>
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>
#include <unistd.h>

#define MSK_NEVER 0x0
#define MSK_GT 0x2
#define MSK_LT 0x4
#define MSK_NE 0x6
#define MSK_EQ 0x8
#define MSK_GE 0xa
#define MSK_LE 0xc
#define MSK_ALWAYS 14

#define str(a) #a

static const char *msk_op[] = {
   "N", ">", "<", "!=", "==", ">=", "<=", "Y"
};

static void handle_sigfpe(int sig, siginfo_t *info, void *uc)
{
   printf(" [FPE %d]", info->si_code);
   fflush(stdout);
}

/* Load and trap. */

#define test_LxAT(type,mnem,opc)                                        \
   {                                                                    \
      static const type vals[] = {                                      \
         0x12345678, -0x12345678, 0x80000000, 0                         \
      };                                                                \
      int i;                                                            \
      printf("%-6s", mnem); fflush(stdout);                             \
      for (i = 0; i < sizeof(vals) / sizeof(vals[0]); i++) {            \
         unsigned long result;                                          \
         asm volatile("lghi %[out], -1\n"                               \
                      ".insn rxy, 0x" opc ",%[out],%[in]\n"             \
                      : [out] "=&d" (result) : [in] "R" (vals[i]));     \
         printf(" %016lx", result);                                     \
      }                                                                 \
      putchar('\n');                                                    \
   }

/* Compare and trap, register/register. */

#define insn_CxRT(type,fmt,opc,mask,a_val,b_val) {                      \
      type a = a_val;                                                   \
      type b = b_val;                                                   \
      printf("  " fmt " %s " fmt, a, msk_op[mask / 2], b);              \
      fflush(stdout);                                                   \
      asm volatile(".insn rrf, 0x" opc "0000,%[a],%[b]," str(mask) ",0\n" \
                   : : [a] "d" (a), [b] "d" (b));                       \
      putchar('\n');                                                    \
   }

#define test_CxRT(type,fmt,mnem,opc) {                                  \
      printf("%s:\n", mnem);                                            \
      insn_CxRT(type, fmt, opc, MSK_ALWAYS, 0, 0);                      \
      insn_CxRT(type, fmt, opc, MSK_LT, 1, -1);                         \
      insn_CxRT(type, fmt, opc, MSK_GT, 0x7fffffff, -0x80000000);       \
   }

/* Compare and trap, register/immediate. */

#define insn_CxIT(sign,type,fmt,opc1,opc2,mask,val,imm) {           \
      sign type a = val;                                            \
      printf("  " fmt " %s " fmt, a, msk_op[mask / 2],              \
             (sign type)(sign short) imm);                          \
      fflush(stdout);                                               \
      asm volatile(".insn ri, 0x" opc1 "000000,%[a]," str(imm) "\n" \
                   ".byte " str(mask) "*16, 0x" opc2 "\n"           \
                   : : [a] "d" (a));                                \
      putchar('\n');                                                \
   }

#define test_CxIT(sign,type,fmt,mnem,opc1,opc2) {                       \
      printf("%s:\n", mnem);                                            \
      insn_CxIT(sign, type, fmt, opc1, opc2, MSK_NEVER, 0, 0);          \
      insn_CxIT(sign, type, fmt, opc1, opc2, MSK_NE, -1, -1);           \
      insn_CxIT(sign, type, fmt, opc1, opc2, MSK_LE, -0x80000000, 41);  \
   }

/* Compare and trap, register/memory. */

#define insn_CLxT(type,fmt,opc1,opc2,mask,a_val,b_val) {                \
      type a = a_val;                                                   \
      type b = b_val;                                                   \
      printf("  " fmt " %s " fmt, a, msk_op[mask / 2], b);              \
      fflush(stdout);                                                   \
      asm volatile(".insn rsy, 0x" opc1 "00000000" opc2 ",%[a],"        \
                   str(mask) ",%[b]\n" : : [a] "d" (a), [b] "R" (b));   \
      putchar('\n');                                                    \
   }

#define test_CLxT(type,fmt,mnem,opc1,opc2) {                            \
      printf("%s:\n", mnem);                                            \
      insn_CLxT(type, fmt, opc1, opc2, MSK_GE, 1, -1);                  \
      insn_CLxT(type, fmt, opc1, opc2, MSK_EQ, 0xffffffff, -1);         \
   }

int main(void)
{
   struct sigaction sa;

   sa.sa_sigaction = handle_sigfpe;
   sa.sa_flags = SA_SIGINFO;
   sigemptyset(&sa.sa_mask);
   sigaction(SIGFPE, &sa, NULL);

   test_LxAT(unsigned, "lat", "e3000000009f");
   test_LxAT(unsigned long, "lgat", "e30000000085");
   test_LxAT(unsigned, "lfhat", "e300000000c8");
   test_LxAT(unsigned, "llgfat", "e3000000009d");
   test_LxAT(unsigned, "llgtat", "e3000000009c");

   putchar('\n');
   test_CxRT(int, "%d", "crt", "b972");
   test_CxRT(unsigned int, "%u", "clrt", "b973");
   test_CxRT(long int, "%ld", "cgrt", "b960");
   test_CxRT(unsigned long, "%lu", "clgrt", "b961");

   putchar('\n');
   test_CxIT(signed, int, "%d", "cit", "ec", "72");
   test_CxIT(unsigned, int, "%u", "clfit", "ec", "73");
   test_CxIT(signed, long, "%ld", "cgit", "ec", "70");
   test_CxIT(unsigned, long, "%lu", "clgit", "ec", "71");

   putchar('\n');
   test_CLxT(unsigned int, "%u", "clt", "eb", "23");
   test_CLxT(unsigned long, "%lu", "clgt", "eb", "2b");
   return 0;
}
