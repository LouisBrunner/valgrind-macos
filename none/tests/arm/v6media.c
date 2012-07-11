
/* How to compile:
   gcc -g -Wall -mcpu=cortex-a8 -o v6mediaA -marm none/tests/arm/v6media.c
   or
   gcc -g -Wall -mcpu=cortex-a8 -o v6mediaT -mthumb none/tests/arm/v6media.c
*/

#include <stdio.h>

static int gen_cin(cin)
{
  int r = ((cin & 1) ? (1<<29) : 0);
  //r |= (1 << 31) | (1 << 30);
  return r;
}

/* test macros to generate and output the result of a single instruction */
#define TESTINST2(instruction, RMval, RD, RM, carryin) \
{ \
  unsigned int out;   \
  unsigned int cpsr;  \
\
  __asm__ volatile(   \
    "msr  cpsr_fs, %3;"  \
    "mov " #RM ",%2;"         \
                /* set #RD to 0x55555555 so we can see which parts get overwritten */ \
                "mov " #RD ", #0x55" "\n\t" \
                "orr " #RD "," #RD "," #RD ", LSL #8" "\n\t"  \
                "orr " #RD "," #RD "," #RD ", LSL #16" "\n\t" \
    instruction ";"    \
    "mov %0," #RD ";"  \
    "mrs %1,cpsr;"     \
    : "=&r" (out), "=&r" (cpsr)  \
    : "r" (RMval), "r" (gen_cin(carryin))       \
    : #RD, #RM, "cc", "memory"   \
  ); \
  printf("%s :: rd 0x%08x rm 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c%c ge[3:0]=%d%d%d%d\n", \
    instruction, out, RMval,      \
    carryin ? 1 : 0,     \
    cpsr & 0xffff0000,            \
    ((1<<31) & cpsr) ? 'N' : ' ', \
    ((1<<30) & cpsr) ? 'Z' : ' ', \
    ((1<<29) & cpsr) ? 'C' : ' ', \
    ((1<<28) & cpsr) ? 'V' : ' ', \
    ((1<<27) & cpsr) ? 'Q' : ' ', \
    (cpsr >> 19) & 1, (cpsr >> 18) & 1, (cpsr >> 17) & 1, (cpsr >> 16) & 1 \
  ); \
}

#define TESTINST3(instruction, RMval, RNval, RD, RM, RN, carryin) \
{ \
  unsigned int out;  \
  unsigned int cpsr; \
\
  __asm__ volatile(  \
    "msr  cpsr_fs, %4;"  \
    "mov " #RM ",%2;"  \
    "mov " #RN ",%3;"  \
    instruction ";"    \
    "mov %0," #RD ";"  \
    "mrs %1,cpsr;"     \
    : "=&r" (out), "=&r" (cpsr)               \
    : "r" (RMval), "r" (RNval), "r" (gen_cin(carryin))   \
    : #RD, #RM, #RN, "cc", "memory"           \
  ); \
  printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c%c ge[3:0]=%d%d%d%d\n", \
    instruction, out, RMval, RNval, \
    carryin ? 1 : 0,  \
    cpsr & 0xffff0000, \
    ((1<<31) & cpsr) ? 'N' : ' ', \
    ((1<<30) & cpsr) ? 'Z' : ' ', \
    ((1<<29) & cpsr) ? 'C' : ' ', \
    ((1<<28) & cpsr) ? 'V' : ' ', \
    ((1<<27) & cpsr) ? 'Q' : ' ', \
    (cpsr >> 19) & 1, (cpsr >> 18) & 1, (cpsr >> 17) & 1, (cpsr >> 16) & 1 \
  ); \
}

#define TESTINST4(instruction, RMval, RNval, RSval, RD, RM, RN, RS, carryin) \
{ \
  unsigned int out;  \
  unsigned int cpsr; \
\
  __asm__ volatile( \
    "msr  cpsr_fs, %5;"  \
    "mov " #RM ",%2;"  \
    "mov " #RN ",%3;"  \
    "mov " #RS ",%4;"  \
    instruction ";"    \
    "mov %0," #RD ";"  \
    "mrs %1,cpsr;"     \
    : "=&r" (out), "=&r" (cpsr) \
    : "r" (RMval), "r" (RNval), "r" (RSval), "r" (gen_cin(carryin))  \
    : #RD, #RM, #RN, #RS, "cc", "memory" \
  ); \
  printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x rs 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c%c ge[3:0]=%d%d%d%d\n", \
    instruction, out, RMval, RNval, RSval, \
    carryin ? 1 : 0,   \
    cpsr & 0xffff0000, \
    ((1<<31) & cpsr) ? 'N' : ' ', \
    ((1<<30) & cpsr) ? 'Z' : ' ', \
    ((1<<29) & cpsr) ? 'C' : ' ', \
    ((1<<28) & cpsr) ? 'V' : ' ', \
    ((1<<27) & cpsr) ? 'Q' : ' ', \
    (cpsr >> 19) & 1, (cpsr >> 18) & 1, (cpsr >> 17) & 1, (cpsr >> 16) & 1 \
  ); \
}

#define TESTINST4_2OUT(instruction, RDval, RD2val, RMval, RSval, RD, RD2, RM, RS, carryin) \
{ \
  unsigned int out;  \
  unsigned int out2; \
  unsigned int cpsr; \
\
  __asm__ volatile(  \
    "msr  cpsr_fs, %7;"  \
    "mov " #RD ",%3;"  \
    "mov " #RD2 ",%4;" \
    "mov " #RM ",%5;"  \
    "mov " #RS ",%6;"  \
    instruction ";"    \
    "mov %0," #RD ";"  \
    "mov %1," #RD2 ";" \
    "mrs %2,cpsr;"     \
    : "=&r" (out), "=&r" (out2), "=&r" (cpsr)  \
    : "r" (RDval), "r" (RD2val), "r" (RMval), "r" (RSval), "r" (gen_cin(carryin)) \
    : #RD, #RD2, #RM, #RS, "cc", "memory"      \
  ); \
  printf("%s :: rd 0x%08x rd2 0x%08x, rm 0x%08x rs 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c%c ge[3:0]=%d%d%d%d\n", \
    instruction, out, out2, RMval, RSval, \
    carryin ? 1 : 0,   \
    cpsr & 0xffff0000, \
    ((1<<31) & cpsr) ? 'N' : ' ', \
    ((1<<30) & cpsr) ? 'Z' : ' ', \
    ((1<<29) & cpsr) ? 'C' : ' ', \
    ((1<<28) & cpsr) ? 'V' : ' ', \
    ((1<<27) & cpsr) ? 'Q' : ' ', \
    (cpsr >> 19) & 1, (cpsr >> 18) & 1, (cpsr >> 17) & 1, (cpsr >> 16) & 1 \
  ); \
}

/* helpers */
#define TESTCARRY { int c = 0; for (c = 0; c < 2; c++) {
#define TESTCARRYEND }}




int main(int argc, char **argv)
{
  printf("MUL\n");
  TESTINST3("mul  r0, r1, r2", 0,          0,          r0, r1, r2, 0);
  TESTINST3("mul  r0, r1, r2", 0xffffffff, 0,          r0, r1, r2, 0);
  TESTINST3("mul  r0, r1, r2", 0,          0xffffffff, r0, r1, r2, 0);
  TESTINST3("mul  r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("mul  r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("mul  r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

#if 0
  printf("MULS\n");
  TESTINST3("muls r0, r1, r2", 0, 0, r0, r1, r2, 0);
  TESTINST3("muls r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
  TESTINST3("muls r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("muls r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("muls r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("muls r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);
#endif

  printf("MLA\n");
  TESTINST4("mla  r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4("mla  r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mla  r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mla  r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);

#if 0
  printf("MLAS\n");
  TESTINST4("mlas r0, r1, r2, r3", 0,          0,          1, r0, r1, r2, r3, 0);
  TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0,          1, r0, r1, r2, r3, 0);
  TESTINST4("mlas r0, r1, r2, r3", 0,          0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mlas r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mlas r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);
#endif

  printf("MLS\n");
  TESTINST4("mls  r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4("mls  r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4("mls  r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mls  r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mls  r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
  TESTINST4("mls  r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);

  printf("UMULL\n");
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umull  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#if 0
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif
  printf("SMULL\n");
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#if 0
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif

  printf("UMLAL\n");
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlal  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#if 0
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif

  printf("SMLAL\n");
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlal  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#if 0
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
  TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif

  printf("CLZ\n");
  TESTCARRY
  TESTINST2("clz  r0, r1", 0, r0, r1, c);
  TESTINST2("clz  r0, r1", 1, r0, r1, c);
  TESTINST2("clz  r0, r1", 0x10, r0, r1, c);
  TESTINST2("clz  r0, r1", 0xffffffff, r0, r1, c);
  TESTCARRYEND

  printf("extend instructions\n");
  TESTINST2("uxtb r0, r1", 0, r0, r1, 0);
  TESTINST2("uxtb r0, r1", 1, r0, r1, 0);
  TESTINST2("uxtb r0, r1", 0xff, r0, r1, 0);
  TESTINST2("uxtb r0, r1", 0xffffffff, r0, r1, 0);
  TESTINST2("sxtb r0, r1", 0, r0, r1, 0);
  TESTINST2("sxtb r0, r1", 1, r0, r1, 0);
  TESTINST2("sxtb r0, r1", 0xff, r0, r1, 0);
  TESTINST2("sxtb r0, r1", 0xffffffff, r0, r1, 0);

  TESTINST2("uxth r0, r1", 0, r0, r1, 0);
  TESTINST2("uxth r0, r1", 1, r0, r1, 0);
  TESTINST2("uxth r0, r1", 0xffff, r0, r1, 0);
  TESTINST2("uxth r0, r1", 0xffffffff, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0, r0, r1, 0);
  TESTINST2("sxth r0, r1", 1, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0x7fff, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0xffff, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0x10ffff, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0x107fff, r0, r1, 0);
  TESTINST2("sxth r0, r1", 0xffffffff, r0, r1, 0);

  TESTINST2("uxtb r0, r1, ror #0", 0x000000ff, r0, r1, 0);
  TESTINST2("uxtb r0, r1, ror #8", 0x000000ff, r0, r1, 0);
  TESTINST2("uxtb r0, r1, ror #8", 0x0000ff00, r0, r1, 0);
  TESTINST2("uxtb r0, r1, ror #16", 0x00ff0000, r0, r1, 0);
  TESTINST2("uxtb r0, r1, ror #24", 0xff000000, r0, r1, 0);

  TESTINST2("uxtb16 r0, r1", 0xffffffff, r0, r1, 0);
  TESTINST2("uxtb16 r0, r1, ror #16", 0x0000ffff, r0, r1, 0);
  TESTINST2("sxtb16 r0, r1", 0xffffffff, r0, r1, 0);
  TESTINST2("sxtb16 r0, r1", 0x00ff00ff, r0, r1, 0);
  TESTINST2("sxtb16 r0, r1", 0x007f007f, r0, r1, 0);

  printf("------------ BFI ------------\n");
  /* bfi  rDst, rSrc, #lsb-in-dst, #number-of-bits-to-copy */
  TESTINST2("bfi  r0, r1, #0, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #1, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #2, #11", 0xAAAAAAAA, r0, r1, 0);

  TESTINST2("bfi  r0, r1, #19, #11", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #20, #11", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #21, #11", 0xFFFFFFFF, r0, r1, 0);

  TESTINST2("bfi  r0, r1, #0, #32", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #1, #31", 0xFFFFFFFF, r0, r1, 0);

  TESTINST2("bfi  r0, r1, #29, #3", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #30, #2", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfi  r0, r1, #31, #1", 0xFFFFFFFF, r0, r1, 0);

  printf("------------ BFC ------------\n");
  /* bfi  rDst, #lsb-in-dst, #number-of-bits-to-copy */
  TESTINST2("bfc  r0, #0, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("bfc  r0, #1, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("bfc  r0, #2, #11", 0xAAAAAAAA, r0, r1, 0);

  TESTINST2("bfc  r0, #19, #11", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfc  r0, #20, #11", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfc  r0, #21, #11", 0xFFFFFFFF, r0, r1, 0);

  TESTINST2("bfc  r0, #0, #32", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfc  r0, #1, #31", 0xFFFFFFFF, r0, r1, 0);

  TESTINST2("bfc  r0, #29, #3", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfc  r0, #30, #2", 0xFFFFFFFF, r0, r1, 0);
  TESTINST2("bfc  r0, #31, #1", 0xFFFFFFFF, r0, r1, 0);

  printf("------------ SBFX ------------\n");
  /* sbfx rDst, rSrc, #lsb, #width */
  TESTINST2("sbfx  r0, r1, #0, #1", 0x00000000, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #0, #1", 0x00000001, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #1", 0x00000000, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #1", 0x00000001, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #1", 0x00000002, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #1", 0x00000003, r0, r1, 0);

  TESTINST2("sbfx  r0, r1, #0, #2", 0x00000000, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #0, #2", 0x00000001, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #2", 0x00000000, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #2", 0x00000001, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #2", 0x00000002, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #2", 0x00000003, r0, r1, 0);

  TESTINST2("sbfx  r0, r1, #0, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #1, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #2, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #2, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #31, #1", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("sbfx  r0, r1, #30, #2", 0xAAAAAAAA, r0, r1, 0);

  printf("------------ UBFX ------------\n");
  /* ubfx rDst, rSrc, #lsb, #width */
  TESTINST2("ubfx  r0, r1, #0, #1", 0x00000000, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #0, #1", 0x00000001, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #1", 0x00000000, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #1", 0x00000001, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #1", 0x00000002, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #1", 0x00000003, r0, r1, 0);

  TESTINST2("ubfx  r0, r1, #0, #2", 0x00000000, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #0, #2", 0x00000001, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #2", 0x00000000, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #2", 0x00000001, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #2", 0x00000002, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #2", 0x00000003, r0, r1, 0);

  TESTINST2("ubfx  r0, r1, #0, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #1, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #2, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #2, #11", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #31, #1", 0xAAAAAAAA, r0, r1, 0);
  TESTINST2("ubfx  r0, r1, #30, #2", 0xAAAAAAAA, r0, r1, 0);

  printf("------------ SMUL{B,T}{B,T} ------------\n");
  /* SMULbb rD, rN, rM */
  TESTINST3("smulbb r0, r1, r2", 0x00030000, 0x00040000,  r0, r1, r2, 0);
  TESTINST3("smulbb r0, r1, r2", 0x00030001, 0x00040002,  r0, r1, r2, 0);
  TESTINST3("smulbb r0, r1, r2", 0x00038001, 0x00047fff,  r0, r1, r2, 0);
  TESTINST3("smulbb r0, r1, r2", 0x00037fff, 0x00047fff,  r0, r1, r2, 0);
  TESTINST3("smulbb r0, r1, r2", 0x0003ffff, 0x0004ffff,  r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smulbb r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);
  /* SMULtt rD, rN, rM */
  TESTINST3("smultt r0, r1, r2", 0x00000003, 0x00000004,  r0, r1, r2, 0);
  TESTINST3("smultt r0, r1, r2", 0x00010003, 0x00020004,  r0, r1, r2, 0);
  TESTINST3("smultt r0, r1, r2", 0x80010003, 0x7fff0004,  r0, r1, r2, 0);
  TESTINST3("smultt r0, r1, r2", 0x7fff0003, 0x7fff0004,  r0, r1, r2, 0);
  TESTINST3("smultt r0, r1, r2", 0xffff0003, 0xffff0004,  r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smultt r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);
  /* SMULtb rD, rN, rM */
  TESTINST3("smultb r0, r1, r2", 0x00000003, 0x00040000,  r0, r1, r2, 0);
  TESTINST3("smultb r0, r1, r2", 0x00010003, 0x00040002,  r0, r1, r2, 0);
  TESTINST3("smultb r0, r1, r2", 0x80010003, 0x00047fff,  r0, r1, r2, 0);
  TESTINST3("smultb r0, r1, r2", 0x7fff0003, 0x00047fff,  r0, r1, r2, 0);
  TESTINST3("smultb r0, r1, r2", 0xffff0003, 0x0004ffff,  r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smultb r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);
  /* SMULbt rD, rN, rM */
  TESTINST3("smulbt r0, r1, r2", 0x00030000, 0x00000004,  r0, r1, r2, 0);
  TESTINST3("smulbt r0, r1, r2", 0x00030001, 0x00020004,  r0, r1, r2, 0);
  TESTINST3("smulbt r0, r1, r2", 0x00038001, 0x7fff0004,  r0, r1, r2, 0);
  TESTINST3("smulbt r0, r1, r2", 0x00037fff, 0x7fff0004,  r0, r1, r2, 0);
  TESTINST3("smulbt r0, r1, r2", 0x0003ffff, 0xffff0004,  r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smulbt r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);

  printf("-------------- SMULW{B,T} --------------\n");
  /* SMULWB rD, rN, rM : Rn x Rm[31..16] */
  TESTINST3("smulwb r0, r1, r2", 0x00000003, 0x00020004, r0, r1, r2, 0);
  TESTINST3("smulwb r0, r1, r2", 0x00010003, 0x47ff0004, r0, r1, r2, 0);
  TESTINST3("smulwb r0, r1, r2", 0x80010003, 0x7fff0004, r0, r1, r2, 0);
  TESTINST3("smulwb r0, r1, r2", 0x7fff0003, 0x7fff0004, r0, r1, r2, 0);
  TESTINST3("smulwb r0, r1, r2", 0xffff0003, 0xffff0004, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smulwb r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);
  /* SMULWT rD, rN, rM - Rn x Rm[15.. 0] */
  TESTINST3("smulwt r0, r1, r2", 0x00000003, 0x00040000, r0, r1, r2, 0);
  TESTINST3("smulwt r0, r1, r2", 0x00010003, 0x00040002, r0, r1, r2, 0);
  TESTINST3("smulwt r0, r1, r2", 0x80010003, 0x00047fff, r0, r1, r2, 0);
  TESTINST3("smulwt r0, r1, r2", 0x7fff0003, 0x00047fff, r0, r1, r2, 0);
  TESTINST3("smulwt r0, r1, r2", 0xffff0003, 0x0004ffff, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x2575feb2, 0xd2c4287c, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xfb412431, 0x4b90362d, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x004dfbe5, 0xe87927cc, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xf6a3fa3c, 0x083b3571, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xbf17fb9a, 0xb9743941, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x2c0bd024, 0xbce5f924, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x3e976e2e, 0xcc3c201c, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xb4bfb365, 0x1ebaf88e, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x288593c0, 0x722d5e20, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x4d7ff5b4, 0xa1d6f791, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x4557be13, 0x7b11bee7, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xadcf5772, 0xa5631488, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x989a7235, 0xb10bcc65, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x4d6f393a, 0x73f39fca, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x24a3291e, 0x5648e540, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xdd91eebf, 0xc54f79e6, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xf7ce2ec6, 0x5fc92974, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xbc1083e8, 0x7e08184e, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xa617cc31, 0x71c8315f, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xdfe1e8f0, 0x9493110e, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x6ef49020, 0xba8a7e0d, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x3dc4e36b, 0x21568e39, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x52db4a9d, 0x55fcc8cf, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x3564c76c, 0x14434a2a, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x27836b0c, 0x3c855ca8, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x62ff7c30, 0x30ece28e, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x40955fdf, 0x057b562c, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x3b34c270, 0x27e1475b, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x7fdcda96, 0xd05893a7, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xb6ab141d, 0x2dc43624, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x403d53cb, 0x5328d58c, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x21ef1aef, 0x87488a4a, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x31458a23, 0xbb246228, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x848af791, 0x339d8d88, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xda3bacdc, 0x70974249, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x649d5cbd, 0x8a8d4e7d, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xc0c8c881, 0xeb1b4335, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x7dd81a20, 0x0cd6b508, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x6892886c, 0x6731e282, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x112dcffc, 0xb6edf28f, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xabfabbe6, 0x4b4ec9ca, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xe52aabf8, 0xc1037fa4, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xf2f4df1f, 0xcb4ab48f, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x435f909a, 0xaf8f7e18, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x2106ba5f, 0x87df4510, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x246a6376, 0xabf4e8e1, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x1046a1a3, 0xf4c0eeac, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0x638ca515, 0x006a54f2, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xf63e7a9d, 0x79f74493, r0, r1, r2, 0);
TESTINST3("smulwt r0, r1, r2", 0xbd6845cd, 0x9c09e313, r0, r1, r2, 0);

  printf("------------ PKHBT / PKHTB ------------\n");
  /* PKHBT */
  TESTINST3("pkhbt r0, r1, r2, lsl #0",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #1",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #2",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #3",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #22", 0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhbt r0, r1, r2, lsl #31", 0x11223344, 0x55667788, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0x50c28082, 0xc1553709, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0x17962e8f, 0x69ec0212, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0xc57243b7, 0x03fa9bb5, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0x7eb226ac, 0xf52e9fbf, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0xbce0f026, 0x7fcbe5a9, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0xa5757252, 0x2dd01366, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0xf4a477c1, 0x5e4b1cbf, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0x76723a21, 0x464a21cc, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0x74d01105, 0xe8108f1b, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2", 0xc1273e2c, 0xcd90d604, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #0",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #1",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #2",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #3",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #8",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #12", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #24", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #31", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #0",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #1",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #2",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #3",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #8",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #12", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #24", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #31", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #0",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #1",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #2",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #3",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #8",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #12", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #24", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #31", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #0",  0xd5dc5407, 0xf87b961e, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #1",  0xd65db979, 0xc61b323b, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #2",  0xa3268abe, 0xed2cbf78, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #3",  0xbf73f0a5, 0x2fb714c9, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x281703ed, 0x925ef472, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #8",  0xeaa652c7, 0x137741f4, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #12", 0x71fbde8b, 0xdba5bd25, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x884c0ad8, 0xc00b821a, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #24", 0xe1bb8606, 0x58293969, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #31", 0xa3cfd624, 0x6077fb1f, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #0",  0x40b094e2, 0x17913309, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #1",  0x5388b5cd, 0x86582032, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #2",  0x5de41558, 0xccfa1c7e, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #3",  0x23ba1b46, 0x4437983c, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #4",  0x48d06549, 0xa9085781, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #8",  0xc6b4ac58, 0xb2aead21, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #12", 0xc2bdf597, 0xdde1e6a4, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #16", 0x852e3a72, 0x157b0dea, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #24", 0xe7aa57b4, 0x1584bd74, r0,r1,r2, 0);
TESTINST3("pkhbt r0, r1, r2, lsl #31", 0xd4b64d54, 0xc53aaba9, r0,r1,r2, 0);
  /* PKHTB */
  TESTINST3("pkhtb r0, r1, r2, asr #0",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #1",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #2",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #3",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #4",  0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #16", 0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #22", 0x11223344, 0x55667788, r0,r1,r2, 0);
  TESTINST3("pkhtb r0, r1, r2, asr #31", 0x11223344, 0x55667788, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0x50c28082, 0xc1553709, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0x17962e8f, 0x69ec0212, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0xc57243b7, 0x03fa9bb5, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0x7eb226ac, 0xf52e9fbf, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0xbce0f026, 0x7fcbe5a9, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0xa5757252, 0x2dd01366, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0xf4a477c1, 0x5e4b1cbf, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0x76723a21, 0x464a21cc, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0x74d01105, 0xe8108f1b, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2", 0xc1273e2c, 0xcd90d604, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #0",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #1",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #2",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #3",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #4",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #8",  0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #12", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #16", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #24", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #31", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #32", 0x5f986e68, 0x35232047, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #0",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #1",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #2",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #3",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #4",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #8",  0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #12", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #16", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #24", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #31", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #32", 0x36f26261, 0x89d2ef86, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #0",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #1",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #2",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #3",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #4",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #8",  0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #12", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #16", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #24", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #31", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #32", 0x216158cb, 0x57a50a01, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #0",  0xd5dc5407, 0xf87b961e, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #1",  0xd65db979, 0xc61b323b, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #2",  0xa3268abe, 0xed2cbf78, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #3",  0xbf73f0a5, 0x2fb714c9, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #4",  0x281703ed, 0x925ef472, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #8",  0xeaa652c7, 0x137741f4, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #12", 0x71fbde8b, 0xdba5bd25, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #16", 0x884c0ad8, 0xc00b821a, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #24", 0xe1bb8606, 0x58293969, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #31", 0xa3cfd624, 0x6077fb1f, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #32", 0xa3cfd624, 0x6077fb1f, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #0",  0x40b094e2, 0x17913309, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #1",  0x5388b5cd, 0x86582032, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #2",  0x5de41558, 0xccfa1c7e, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #3",  0x23ba1b46, 0x4437983c, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #4",  0x48d06549, 0xa9085781, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #8",  0xc6b4ac58, 0xb2aead21, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #12", 0xc2bdf597, 0xdde1e6a4, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #16", 0x852e3a72, 0x157b0dea, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #24", 0xe7aa57b4, 0x1584bd74, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #31", 0xd4b64d54, 0xc53aaba9, r0,r1,r2, 0);
TESTINST3("pkhtb r0, r1, r2, asr #32", 0xd4b64d54, 0xc53aaba9, r0,r1,r2, 0);

  printf("----------------- USAT ----------------- \n");
  TESTINST2("usat  r0, #0,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat  r0, #1,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat  r0, #5,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat  r0, #8,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat  r0, #11, r1", 0x11110000, r0, r1, 0);
  TESTINST2("usat  r0, #13, r1", 0x11110000, r0, r1, 0);
  TESTINST2("usat  r0, #15, r1", 0x11110000, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1",          0xebbff82b, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #0",  0x5f986e68, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #0",  0xe7aa57b4, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #0",  0x89d2ef86, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #8",  0xc53aaba9, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #8",  0x216158cb, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, lsl #8",  0x3cd6cd94, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #0",  0xf87b961e, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #0",  0xc61b323b, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #0",  0xa3268abe, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #8",  0xbf73f0a5, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #8",  0x925ef472, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, lsl #8",  0x137741f4, r0, r1, 0);
TESTINST2("usat  r0, #24, r1, lsl #2",  0x50c28082, r0, r1, 0);
TESTINST2("usat  r0, #16, r1, lsl #3",  0x17962e8f, r0, r1, 0);
TESTINST2("usat  r0, #12, r1, lsl #4",  0xc57243b7, r0, r1, 0);
TESTINST2("usat  r0, #8,  r1, lsl #8",  0xf20fb90f, r0, r1, 0);
TESTINST2("usat  r0, #4,  r1, lsl #12", 0xbb151055, r0, r1, 0);
TESTINST2("usat  r0, #3,  r1, lsl #16", 0x957440d2, r0, r1, 0);
TESTINST2("usat  r0, #2,  r1, lsl #24", 0x728b7771, r0, r1, 0);
TESTINST2("usat  r0, #1,  r1, lsl #31", 0xf13c20f3, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1",          0xebbff82b, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0x5f986e68, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0xe7aa57b4, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0x89d2ef86, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0xc53aaba9, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0x216158cb, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0x3cd6cd94, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xf87b961e, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xc61b323b, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xa3268abe, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0xbf73f0a5, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0x925ef472, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0x137741f4, r0, r1, 0);
TESTINST2("usat  r0, #24, r1, asr #2",  0x50c28082, r0, r1, 0);
TESTINST2("usat  r0, #16, r1, asr #3",  0x17962e8f, r0, r1, 0);
TESTINST2("usat  r0, #12, r1, asr #4",  0xc57243b7, r0, r1, 0);
TESTINST2("usat  r0, #8,  r1, asr #8",  0xf20fb90f, r0, r1, 0);
TESTINST2("usat  r0, #4,  r1, asr #12", 0xbb151055, r0, r1, 0);
TESTINST2("usat  r0, #3,  r1, asr #16", 0x957440d2, r0, r1, 0);
TESTINST2("usat  r0, #2,  r1, asr #24", 0x728b7771, r0, r1, 0);
TESTINST2("usat  r0, #1,  r1, asr #31", 0xf13c20f3, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1",          0xebbff82b, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0x5f986e68, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0xe7aa57b4, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #0",  0x89d2ef86, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0xc53aaba9, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0x216158cb, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #8",  0x3cd6cd94, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xf87b961e, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xc61b323b, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #0",  0xa3268abe, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0xbf73f0a5, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0x925ef472, r0, r1, 0);
TESTINST2("usat  r0, #0,  r1, asr #8",  0x137741f4, r0, r1, 0);
TESTINST2("usat  r0, #24, r1, asr #2",  0x50c28082, r0, r1, 0);
TESTINST2("usat  r0, #16, r1, asr #3",  0x17962e8f, r0, r1, 0);
TESTINST2("usat  r0, #12, r1, asr #4",  0xc57243b7, r0, r1, 0);
TESTINST2("usat  r0, #8,  r1, asr #8",  0xf20fb90f, r0, r1, 0);
TESTINST2("usat  r0, #4,  r1, asr #12", 0xbb151055, r0, r1, 0);
TESTINST2("usat  r0, #3,  r1, asr #16", 0x957440d2, r0, r1, 0);
TESTINST2("usat  r0, #2,  r1, asr #24", 0x728b7771, r0, r1, 0);
TESTINST2("usat  r0, #1,  r1, asr #31", 0xf13c20f3, r0, r1, 0);
#ifndef __thumb__
TESTINST2("usat  r0, #0,  r1, asr #32", 0xa9085781, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #32", 0x40b094e2, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #32", 0x17913309, r0, r1, 0);
TESTINST2("usat  r0, #31, r1, asr #32", 0x5388b5cd, r0, r1, 0);
TESTINST2("usat  r0, #24, r1, asr #32", 0x86582032, r0, r1, 0);
TESTINST2("usat  r0, #16, r1, asr #32", 0x5de41558, r0, r1, 0);
TESTINST2("usat  r0, #12, r1, asr #32", 0xccfa1c7e, r0, r1, 0);
TESTINST2("usat  r0, #8,  r1, asr #32", 0x23ba1b46, r0, r1, 0);
TESTINST2("usat  r0, #4,  r1, asr #32", 0x4437983c, r0, r1, 0);
TESTINST2("usat  r0, #3,  r1, asr #32", 0x48d06549, r0, r1, 0);
TESTINST2("usat  r0, #2,  r1, asr #32", 0xa9085781, r0, r1, 0);
TESTINST2("usat  r0, #1,  r1, asr #32", 0xc6b4ac58, r0, r1, 0);
#endif

  printf("------------ USAT16 sat_imm ------------ \n");
  TESTINST2("usat16  r0, #0,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat16  r0, #1,  r1", 0xffcdabcd, r0, r1, 0);
  TESTINST2("usat16  r0, #5,  r1", 0x0123feff, r0, r1, 0);
  TESTINST2("usat16  r0, #8,  r1", 0x0123abcd, r0, r1, 0);
  TESTINST2("usat16  r0, #11, r1", 0x11110000, r0, r1, 0);
  TESTINST2("usat16  r0, #13, r1", 0x1111f111, r0, r1, 0);
  TESTINST2("usat16  r0, #15, r1", 0x00001111, r0, r1, 0);
TESTINST2("usat16  r0, #0,  r1", 0xebbff82b, r0, r1, 0);
TESTINST2("usat16  r0, #1,  r1", 0xebbff82b, r0, r1, 0);
TESTINST2("usat16  r0, #3,  r1", 0x50c28082, r0, r1, 0);
TESTINST2("usat16  r0, #5,  r1", 0x17962e8f, r0, r1, 0);
TESTINST2("usat16  r0, #8,  r1", 0xc57243b7, r0, r1, 0);
TESTINST2("usat16  r0, #10, r1", 0xf20fb90f, r0, r1, 0);
TESTINST2("usat16  r0, #11, r1", 0xbb151055, r0, r1, 0);
TESTINST2("usat16  r0, #13, r1", 0x957440d2, r0, r1, 0);
TESTINST2("usat16  r0, #14, r1", 0x728b7771, r0, r1, 0);
TESTINST2("usat16  r0, #15, r1", 0xf13c20f3, r0, r1, 0);
TESTINST2("usat16  r0, #0,  r1", 0x86398371, r0, r1, 0);
TESTINST2("usat16  r0, #1,  r1", 0x03d0fb78, r0, r1, 0);
TESTINST2("usat16  r0, #3,  r1", 0xd0d49b7c, r0, r1, 0);
TESTINST2("usat16  r0, #5,  r1", 0x76354a58, r0, r1, 0);
TESTINST2("usat16  r0, #8,  r1", 0x9fa45fb7, r0, r1, 0);
TESTINST2("usat16  r0, #10, r1", 0x7572bdec, r0, r1, 0);
TESTINST2("usat16  r0, #11, r1", 0xfea59eb6, r0, r1, 0);
TESTINST2("usat16  r0, #13, r1", 0xf2669090, r0, r1, 0);
TESTINST2("usat16  r0, #14, r1", 0xbc1ff573, r0, r1, 0);
TESTINST2("usat16  r0, #15, r1", 0x7eb226ac, r0, r1, 0);
TESTINST2("usat16  r0, #0,  r1", 0x22b65db1, r0, r1, 0);
TESTINST2("usat16  r0, #1,  r1", 0x776c41c7, r0, r1, 0);
TESTINST2("usat16  r0, #3,  r1", 0xe50dd77c, r0, r1, 0);
TESTINST2("usat16  r0, #5,  r1", 0xd6f9a698, r0, r1, 0);
TESTINST2("usat16  r0, #8,  r1", 0xeda5110c, r0, r1, 0);
TESTINST2("usat16  r0, #10, r1", 0x0be36f70, r0, r1, 0);
TESTINST2("usat16  r0, #11, r1", 0xd759eb72, r0, r1, 0);
TESTINST2("usat16  r0, #13, r1", 0xd9c4b1f4, r0, r1, 0);
TESTINST2("usat16  r0, #14, r1", 0xa29eb320, r0, r1, 0);
TESTINST2("usat16  r0, #15, r1", 0xcf1e4487, r0, r1, 0);
TESTINST2("usat16  r0, #0,  r1", 0x2eb68500, r0, r1, 0);
TESTINST2("usat16  r0, #1,  r1", 0xcdb7ed11, r0, r1, 0);
TESTINST2("usat16  r0, #3,  r1", 0x2eaea305, r0, r1, 0);
TESTINST2("usat16  r0, #5,  r1", 0x6ebd04d9, r0, r1, 0);
TESTINST2("usat16  r0, #8,  r1", 0xa5ec1aa8, r0, r1, 0);
TESTINST2("usat16  r0, #10, r1", 0x72f33509, r0, r1, 0);
TESTINST2("usat16  r0, #11, r1", 0xa3e6f759, r0, r1, 0);
TESTINST2("usat16  r0, #13, r1", 0xfaceab39, r0, r1, 0);
TESTINST2("usat16  r0, #14, r1", 0x2738f0ff, r0, r1, 0);
TESTINST2("usat16  r0, #15, r1", 0xe79fd570, r0, r1, 0);
TESTINST2("usat16  r0, #0,  r1", 0x55ea3e4e, r0, r1, 0);
TESTINST2("usat16  r0, #1,  r1", 0x2b62ba5a, r0, r1, 0);
TESTINST2("usat16  r0, #3,  r1", 0x9b41bfb1, r0, r1, 0);
TESTINST2("usat16  r0, #5,  r1", 0x557c7ba2, r0, r1, 0);
TESTINST2("usat16  r0, #8,  r1", 0x2973c051, r0, r1, 0);
TESTINST2("usat16  r0, #10, r1", 0x6a228b19, r0, r1, 0);
TESTINST2("usat16  r0, #11, r1", 0x0cdafabe, r0, r1, 0);
TESTINST2("usat16  r0, #13, r1", 0x50865114, r0, r1, 0);
TESTINST2("usat16  r0, #14, r1", 0xd83b849b, r0, r1, 0);
TESTINST2("usat16  r0, #15, r1", 0xca5e5605, r0, r1, 0);

  printf("---------------- UADD16 ---------------- \n");
  TESTINST3("uadd16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);

  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000001, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0x00000001, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000001, 0x00000001, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0x0000ffff, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x0000ffff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00010000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00010000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0x00000000, 0xffff0000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0xffff0000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uadd16 r0, r1, r2", 0xffff0000, 0xffff0000, r0, r1, r2, 0);

TESTINST3("uadd16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("uadd16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- SADD16 ---------------- \n");
  TESTINST3("sadd16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);

  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000001, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0x00000001, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000001, 0x00000001, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0x0000ffff, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x0000ffff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00010000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00010000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0x00000000, 0xffff0000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0xffff0000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sadd16 r0, r1, r2", 0xffff0000, 0xffff0000, r0, r1, r2, 0);

TESTINST3("sadd16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("sadd16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- USUB16 ---------------- \n");
  TESTINST3("usub16 r0, r1, r2", 0x04000022, 0x03000011, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);

  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000001, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0x00000001, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000001, 0x00000001, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0x0000ffff, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x0000ffff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00010000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00010000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0x00000000, 0xffff0000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0xffff0000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usub16 r0, r1, r2", 0xffff0000, 0xffff0000, r0, r1, r2, 0);

TESTINST3("usub16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("usub16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- SSUB16 ---------------- \n");
  TESTINST3("ssub16 r0, r1, r2", 0x04000022, 0x03000011, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);

  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000001, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0x00000001, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000001, 0x00000001, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0x0000ffff, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x0000ffff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00010000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00010000, 0x00010000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0x00000000, 0xffff0000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0xffff0000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("ssub16 r0, r1, r2", 0xffff0000, 0xffff0000, r0, r1, r2, 0);

TESTINST3("ssub16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("ssub16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- UADD8 ----------------- \n");
  TESTINST3("uadd8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0x00000318, 0xff00ff09, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0x00020318, 0xff07ff09, r0, r1, r2, 0);
  TESTINST3("uadd8 r0, r1, r2", 0xff07ff09, 0x00020318, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("uadd8 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- USUB8 ----------------- \n");
  TESTINST3("usub8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0x00000318, 0xff00ff09, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0x00020318, 0xff07ff09, r0, r1, r2, 0);
  TESTINST3("usub8 r0, r1, r2", 0xff07ff09, 0x00020318, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("usub8 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- QADD16 ---------------- \n");
  TESTINST3("qadd16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("qadd16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("qadd16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("qadd16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qadd16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- QSUB16 ---------------- \n");
  TESTINST3("qsub16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("qsub16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("qsub16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("qsub16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qsub16 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- QSAX ----------------- \n");
  TESTINST3("qsax r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qsax r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("qsax r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qsax r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("qsax r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("qsax r0, r1, r2", 0x00030003, 0x00640064, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qsax r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- QASX ----------------- \n");
  TESTINST3("qasx r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qasx r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("qasx r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qasx r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("qasx r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("qasx r0, r1, r2", 0x00030003, 0x00640064, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qasx r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- SASX ----------------- \n");
  TESTINST3("sasx r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sasx r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("sasx r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sasx r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("sasx r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("sasx r0, r1, r2", 0x00030003, 0x00640064, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("sasx r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- SMUAD ----------------- \n");
  TESTINST3("smuad r0, r1, r2", 0x80008000, 0x80008000, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0xffffffff, 0xfffc0001, r0, r1, r2, 0);
  TESTINST3("smuad r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("smuad r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);
  printf("----------------- SMUADX ---------------- \n");
  TESTINST3("smuadx r0, r1, r2", 0x80008000, 0x80008000, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0xffffffff, 0xfffc0001, r0, r1, r2, 0);
  TESTINST3("smuadx r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("smuadx r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- SMLAD ----------------- \n");
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
  TESTINST4("smlad  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlad  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

  printf("----------------- SMLADX ----------------- \n");
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
  TESTINST4("smladx  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
TESTINST4("smladx  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smladx  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smladx  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);

  printf("------------ SMLABB, SMLATT, SMLATB, SMLABT ------------\n");
  /* smlabb rD, rN, rM, rA */
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x00030000, 0x00040000, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x00030001, 0x00040002, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x00038001, 0x00047fff, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x00037fff, 0x00047fff, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x0003ffff, 0x0004ffff, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabb r0, r1, r2, r3", 
            0x0003fffc, 0x0004ffff, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlabb  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);
  /* smlatt rD, rN, rM, rA */
  TESTINST4("smlatt r0, r1, r2, r3", 
            0x00000003, 0x00000004, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlatt r0, r1, r2, r3", 
            0x00010003, 0x00020004, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatt r0, r1, r2, r3", 
            0x80010003, 0x7fff0004, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatt r0, r1, r2, r3", 
            0x7fff0003, 0x7fff0004, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatt r0, r1, r2, r3", 
            0xffff0003, 0xffff0004, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatt r0, r1, r2, r3", 
            0xfffc0003, 0xffff0004, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlatt  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);
  /* smlatb rD, rN, rM, rA */
  TESTINST4("smlatb r0, r1, r2, r3", 
            0x00000003, 0x00040000, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlatb r0, r1, r2, r3", 
            0x00010003, 0x00040002, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatb r0, r1, r2, r3", 
            0x80010003, 0x00047fff, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatb r0, r1, r2, r3", 
            0x7fff0003, 0x00047fff, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatb r0, r1, r2, r3", 
            0xffff0003, 0x0004ffff, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlatb r0, r1, r2, r3", 
            0xfffc0003, 0x0004ffff, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlatb  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);
  /* smlabt rD, rN, rM, rA */
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x00030000, 0x00000004, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x00030001, 0x00020004, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x00038001, 0x7fff0004, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x00037fff, 0x7fff0004, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x0003ffff, 0xffff0004, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlabt r0, r1, r2, r3", 
            0x0003fffc, 0xffff0004, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlabt  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

  printf("------------ UQSUB8 -----------------------------------\n");
  TESTINST3("uqsub8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0x00000318, 0xff00ff09, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0x00020318, 0xff07ff09, r0, r1, r2, 0);
  TESTINST3("uqsub8 r0, r1, r2", 0xff07ff09, 0x00020318, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uqsub8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ UQADD8 -----------------------------------\n");
  TESTINST3("uqadd8 r0, r1, r2", 0x0009ffff, 0x001800aa, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0x00aa0018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0x0000aa18, 0xff00ff09, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0xff9fefcc, 0xff9ffedd, r0, r1, r2, 0);
  TESTINST3("uqadd8 r0, r1, r2", 0xff07ff09, 0xaa020318, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uqadd8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ SEL --------------------------------------\n");
  TESTINST3("sel r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("sel r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("sel r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("sel r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("sel r0, r1, r2", 0xfffcffff, 0xffff0001, r0, r1, r2, 0);
  TESTINST3("sel r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sel r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ QSUB8-------------------------------------\n");
  TESTINST3("qsub8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x7fff00ff, 0x80017f01, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qsub8 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("qsub8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ QADD8-------------------------------------\n");
  TESTINST3("qadd8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x7fff00ff, 0x80017f01, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qadd8 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("qadd8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ SHADD8 -----------------------------------\n");
  TESTINST3("shadd8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x7fff00ff, 0x80017f01, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("shadd8 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("shadd8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ UHADD8 -----------------------------------\n");
  TESTINST3("uhadd8 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x7fff00ff, 0x80017f01, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uhadd8 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uhadd8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ UHADD16 -----------------------------------\n");
  TESTINST3("uhadd16 r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x7fff00ff, 0x80017f01, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x80008000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("uhadd16 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uhadd16 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("----------------- SSAT ----------------- \n");
  TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x80008000, r0, r1, 0);
  TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x80008000, r0, r1, 0);
  TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x80008000, r0, r1, 0);
  TESTINST2("ssat  r0, #12, r1, ASR #16", 0x80008000, r0, r1, 0);
  TESTINST2("ssat  r0, #16, r1, LSL #12", 0xffff0009, r0, r1, 0);
  TESTINST2("ssat  r0, #18, r1, LSL #8",  0xffff0009, r0, r1, 0);
  TESTINST2("ssat  r0, #24, r1, ASR #6",  0xffff0009, r0, r1, 0);
  TESTINST2("ssat  r0, #31, r1, ASR #1",  0xffff0009, r0, r1, 0);
TESTINST2("ssat  r0, #1,   r1", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #1,   r1", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #1,   r1", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #1,   r1", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #32,  r1", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #1,  r1, LSL #31", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #3,  r1, LSL #28", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #6,  r1, LSL #24", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #8,  r1, ASR #18", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #12, r1, ASR #16", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #16, r1, LSL #12", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #18, r1, LSL #8", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #24, r1, ASR #6", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #28, r1, ASR #3", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #31, r1, ASR #1", 0xffc134df, r0, r1, 0);
#ifndef __thumb__
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #1, r1, ASR #32", 0xffc134df, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x256bfdd6, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0xc02a0c05, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0xee2fa46e, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x97a7da20, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0xa231d5e6, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x10e1968a, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x0e089270, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x9e8e0185, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0x3096f12e, r0, r1, 0);
TESTINST2("ssat  r0, #32, r1, ASR #32", 0xffc134df, r0, r1, 0);
#endif

  printf("---------------- SADD8 ----------------- \n");
  TESTINST3("sadd8 r0, r1, r2", 0x00f7ffff, 0x00e800fd, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0x00e800fd, 0x00f7ffff, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0x00fd00e8, 0xffff00f7, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0xffff00f7, 0x00fd0018, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0x0000fd18, 0xff00fff7, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0xffff00f7, 0x00fd00e8, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0x00fefd18, 0xff07fff7, r0, r1, r2, 0);
  TESTINST3("sadd8 r0, r1, r2", 0xff07fff7, 0x00fefde8, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sadd8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("---------------- SSUB8 ----------------- \n");
  TESTINST3("ssub8 r0, r1, r2", 0x00f7ffff, 0x00e800fd, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0x00e800fd, 0x00f7ffff, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0x00fd00e8, 0xffff00f7, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0xffff00f7, 0x00fd0018, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0x0000fd18, 0xff00fff7, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0xffff00f7, 0x00fd00e8, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0x00fefd18, 0xff07fff7, r0, r1, r2, 0);
  TESTINST3("ssub8 r0, r1, r2", 0xff07fff7, 0x00fefde8, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xb8035b5b, 0xce0ce1ed, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x146275d8, 0xaae3433f, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x2c07a5b4, 0x32fa0095, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x8ed8287c, 0x02c90120, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x29300837, 0x0b02c58a, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xb0d20777, 0x3e2e1bd7, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xd5fe2dc4, 0xdd914bf7, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x1d66879f, 0xf2b64835, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xae930a1a, 0x5ef1f1a8, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x1ffe53d9, 0x815bb75b, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x3dba1164, 0x3ada0280, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xda4ba05b, 0x90f9833d, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x81616d13, 0x51f31d95, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x0849a0c2, 0x0872f25a, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xf1e03d7e, 0x91edc21d, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x6034078d, 0x181c436b, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x3edad6b6, 0x82aceb7a, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x0557c6fc, 0x6cc9bfa8, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x7f808c15, 0x81874a02, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x6b1422c7, 0x33921b00, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x3ccad3f7, 0xd7ce1909, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x3e435701, 0x85fbf196, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xb4e16b6e, 0x6e13680a, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x89436f88, 0x44858efc, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x9002bc30, 0x390d2c2f, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xbea121ab, 0x953ff6ec, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x80657c40, 0x6ffed89f, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x7795635d, 0x5e6e32dd, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xe4999bf2, 0xec0c2f30, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x5736ed46, 0x231348c0, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x4f9ddd1b, 0x95bca5d8, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x5765b203, 0xc1553709, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x0112b30a, 0x69ec0212, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x74bd0223, 0x03fa9bb5, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x899d9192, 0xf52e9fbf, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x64a365ef, 0x2dd01366, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("ssub8 r0, r1, r2", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ SXTAB ------------\n");
  TESTINST3("sxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #8",  0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #0",  0x31415927, 0x27182819, r0, r1, r2, 0);

  TESTINST3("sxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #8",  0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("sxtab r0, r1, r2, ROR #0",  0x31415927, 0x27182899, r0, r1, r2, 0);

TESTINST3("sxtab r0, r1, r2, ROR #24", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #24", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtab r0, r1, r2, ROR #16", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #16", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtab r0, r1, r2, ROR #8", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #8", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtab r0, r1, r2, ROR #0", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtab r0, r1, r2, ROR #0", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ UXTAB ------------\n");
  TESTINST3("uxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #8",  0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #0",  0x31415927, 0x27182819, r0, r1, r2, 0);

  TESTINST3("uxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #8",  0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab r0, r1, r2, ROR #0",  0x31415927, 0x27182899, r0, r1, r2, 0);

TESTINST3("uxtab r0, r1, r2, ROR #24", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #24", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab r0, r1, r2, ROR #16", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #16", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab r0, r1, r2, ROR #8", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #8", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab r0, r1, r2, ROR #0", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab r0, r1, r2, ROR #0", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("----------- UXTAB16 -----------\n");
  TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #8",  0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #0",  0x31415927, 0x27182819, r0, r1, r2, 0);

  TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #8",  0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #0",  0x31415927, 0x27182899, r0, r1, r2, 0);
  TESTINST3("uxtab16 r0, r1, r2, ROR #0",  0x3141FFFF, 0x27182899, r0, r1, r2, 0);

TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ SXTAH ------------\n");
  TESTINST3("sxtah r0, r1, r2, ROR #24", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #16", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27182819, r0, r1, r2, 0);

  TESTINST3("sxtah r0, r1, r2, ROR #24", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #16", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("sxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27189819, r0, r1, r2, 0);

TESTINST3("sxtah r0, r1, r2, ROR #24", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #24", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtah r0, r1, r2, ROR #16", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #16", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtah r0, r1, r2, ROR #8", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #8", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("sxtah r0, r1, r2, ROR #0", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("sxtah r0, r1, r2, ROR #0", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ UXTAH ------------\n");
  TESTINST3("uxtah r0, r1, r2, ROR #24", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #16", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27182819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27182819, r0, r1, r2, 0);

  TESTINST3("uxtah r0, r1, r2, ROR #24", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #16", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27189819, r0, r1, r2, 0);
  TESTINST3("uxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27189819, r0, r1, r2, 0);

TESTINST3("uxtah r0, r1, r2, ROR #24", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #24", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtah r0, r1, r2, ROR #16", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #16", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtah r0, r1, r2, ROR #8", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #8", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

TESTINST3("uxtah r0, r1, r2, ROR #0", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("uxtah r0, r1, r2, ROR #0", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);

  printf("------------ SMLAWB ------------\n");
  /* smlawb rD, rN, rM, rA */
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x00030000, 0x00040000, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x00030001, 0x00040002, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x00038001, 0x00047fff, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x00037fff, 0x00047fff, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x0003ffff, 0x0004ffff, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawb r0, r1, r2, r3", 
            0x0003fffc, 0x0004ffff, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlawb  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

  printf("------------ SMLAWT ------------\n");
  /* smlawt rD, rN, rM, rA */
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x00030000, 0x00040000, 0x00000000, r0,r1,r2,r3, 0);
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x00030001, 0x00040002, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x00038001, 0x00047fff, 0x00005fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x00037fff, 0x00047fff, 0x00007fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x0003ffff, 0x0004ffff, 0x7fff7fff, r0,r1,r2,r3, 0);
  TESTINST4("smlawt r0, r1, r2, r3", 
            0x0003fffc, 0x0004ffff, 0xffffffff, r0,r1,r2,r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlawt  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);


  printf("----------------- SMLSD ----------------- \n");
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
  TESTINST4("smlsd  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("smlsd  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

  printf("----------------- SMLSDX ----------------- \n");
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
  TESTINST4("smlsdx  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
TESTINST4("smlsdx  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("smlsdx  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("smlsdx  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);


  printf("----------------- SMUSD ----------------- \n");
  TESTINST3("smusd r0, r1, r2", 0x80008000, 0x80008000, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0xffffffff, 0xfffc0001, r0, r1, r2, 0);
  TESTINST3("smusd r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("smusd r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);
  printf("----------------- SMUSDX ---------------- \n");
  TESTINST3("smusdx r0, r1, r2", 0x80008000, 0x80008000, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0xffffffff, 0xfffc0001, r0, r1, r2, 0);
  TESTINST3("smusdx r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("smusdx r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- USAD8 ---------------- \n");
  TESTINST3("usad8 r0, r1, r2", 0x80008000, 0x80008000, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0x7fff7fff, 0x00000000, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0x7fff7fff, 0x00010001, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0x80008000, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0x00640064, 0x00030003, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0xffffffff, 0xfffc0001, r0, r1, r2, 0);
  TESTINST3("usad8 r0, r1, r2", 0xfff70fff, 0x00030003, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("usad8 r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("----------------- USADA8 ----------------- \n");
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
  TESTINST4("usada8  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
TESTINST4("usada8  r0, r1, r2, r3", 
          0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

  printf("---------------- QADD ---------------- \n");
  TESTINST3("qadd r0, r1, r2", 0x00000000, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("qadd r0, r1, r2", 0x00000001, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("qadd r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qadd r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qadd r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

  printf("---------------- QSUB ---------------- \n");
  TESTINST3("qsub r0, r1, r2", 0x00000000, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0x00000001, 0x7fffffff, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0x00000000, 0x00000000, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0x0009ffff, 0x00180003, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0x00180003, 0x0009ffff, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0x00030018, 0xffff0009, r0, r1, r2, 0);
  TESTINST3("qsub r0, r1, r2", 0xffff0009, 0x00030018, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xd83b849b, 0xca5e5605, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x0cdafabe, 0x50865114, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x2738f0ff, 0x6a228b19, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xfaceab39, 0x2973c051, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xa3e6f759, 0x557c7ba2, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x72f33509, 0x9b41bfb1, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xa5ec1aa8, 0x2b62ba5a, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x6ebd04d9, 0x55ea3e4e, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x2eaea305, 0xe79fd570, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x22b65db1, 0xcdb7ed11, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x776c41c7, 0x2eb68500, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xe50dd77c, 0xd6f9a698, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x0be36f70, 0xeda5110c, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xebbff82b, 0xd759eb72, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x50c28082, 0xd9c4b1f4, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x17962e8f, 0xa29eb320, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xc57243b7, 0xcf1e4487, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x7eb226ac, 0xf20fb90f, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xbce0f026, 0xbb151055, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xa5757252, 0x957440d2, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xf4a477c1, 0x728b7771, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x76723a21, 0xf13c20f3, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x74d01105, 0x86398371, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xc1273e2c, 0x03d0fb78, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xdd9b7653, 0xd0d49b7c, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xdde62fd1, 0x76354a58, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xc3fb4a96, 0x9fa45fb7, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xa1a10f56, 0x7572bdec, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x4b7d4fd9, 0xfea59eb6, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x9d0ddffc, 0xf2669090, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x4f82d17c, 0xbc1ff573, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x08215ca2, 0x345f67e6, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xf23595d0, 0x3f39d77e, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xf244c158, 0xfb2db55b, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x256bfdd6, 0x13aebedf, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xc02a0c05, 0x5b013000, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xee2fa46e, 0xed95b542, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x97a7da20, 0x60bb5ee8, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xa231d5e6, 0xd9000a64, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x10e1968a, 0x624f9467, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x0e089270, 0xa8c64d94, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x9e8e0185, 0x6b4f637a, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x3096f12e, 0x11f5f4b9, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xffc134df, 0x0b02eb0c, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);
TESTINST3("qsub r0, r1, r2", 0xf808434e, 0xefeab836, r0, r1, r2, 0);

/*
TESTINST3("theinsn", 0xf7b0b13e, 0x5e4b1cbf, r0, r1, r2, 0);
TESTINST3("theinsn", 0x44de5ca9, 0x464a21cc, r0, r1, r2, 0);
TESTINST3("theinsn", 0x299da970, 0xe8108f1b, r0, r1, r2, 0);
TESTINST3("theinsn", 0xf5818cfb, 0xcd90d604, r0, r1, r2, 0);
TESTINST3("theinsn", 0xaa5e9444, 0x8217b7df, r0, r1, r2, 0);
TESTINST3("theinsn", 0xe60743c3, 0x7acb4de3, r0, r1, r2, 0);
TESTINST3("theinsn", 0x73c29060, 0x868e7c7d, r0, r1, r2, 0);
TESTINST3("theinsn", 0x5f77532e, 0x1d133d3d, r0, r1, r2, 0);
TESTINST3("theinsn", 0x4e5e0760, 0x8f6d3264, r0, r1, r2, 0);
TESTINST3("theinsn", 0x21ba2fb3, 0xde99ac2f, r0, r1, r2, 0);
*/

  return 0;
}
