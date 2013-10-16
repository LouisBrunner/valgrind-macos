
/* This is a test program that checks the parsing of instructions with
   xacquire and xrelease prefixes.  The tested insns are, afaics,
   exactly those listed in the Intel description for the two prefixes
   ("XACQUIRE/XRELEASE -- Hardware Lock Elision Prefix Hints"). */

#include <stdio.h>

typedef  unsigned long long int  ULong;

#define CAT2(_x,_y) _x##_y
#define CAT(_x,_y) CAT2(_x,_y)

#define GEN_BINARY(_insn) \
   void CAT(do_,_insn) ( void ) \
   { \
      volatile ULong n = 0x5555555555555555ULL; \
      ULong some = 0x271831415927D459ULL; \
      __asm__ __volatile__( \
         "\t" \
         "stc"                                            "\n\t" \
         "xacquire lock " #_insn "q $123456789, (%0)"     "\n\t" \
         "xrelease lock " #_insn "q $123456789, (%0)"     "\n\t" \
         "xacquire lock " #_insn "l $0x12345FE, (%0)"     "\n\t" \
         "xrelease lock " #_insn "l $0x12345FE, (%0)"     "\n\t" \
         "xacquire lock " #_insn "w $0x9876,    (%0)"     "\n\t" \
         "xrelease lock " #_insn "w $0x9876,    (%0)"     "\n\t" \
         "xacquire lock " #_insn "b $0x45,      (%0)"     "\n\t" \
         "xrelease lock " #_insn "b $0x45,      (%0)"     "\n\t" \
         "xacquire lock " #_insn "q %1,         (%0)"     "\n\t" \
         "xrelease lock " #_insn "q %1,         (%0)"     "\n\t" \
         "xacquire lock " #_insn "l %k1,        (%0)"     "\n\t" \
         "xrelease lock " #_insn "l %k1,        (%0)"     "\n\t" \
         "xacquire lock " #_insn "w %w1,        (%0)"     "\n\t" \
         "xrelease lock " #_insn "w %w1,        (%0)"     "\n\t" \
         "xacquire lock " #_insn "b %b1,        (%0)"     "\n\t" \
         "xrelease lock " #_insn "b %b1,        (%0)"     "\n\t" \
         : : "r"(&n), "r"(some) : "cc", "memory" \
      ); \
      printf("result for '%-3s' is %016llx\n", #_insn, n);  \
   }

GEN_BINARY(add)
GEN_BINARY(adc)
GEN_BINARY(and)
GEN_BINARY(or)
GEN_BINARY(sbb)
GEN_BINARY(sub)
GEN_BINARY(xor)

#define GEN_UNARY(_insn) \
   void CAT(do_,_insn) ( void ) \
   { \
      volatile ULong n = 0x5555555555555555ULL; \
      __asm__ __volatile__( \
         "\t" \
         "stc"                                "\n\t" \
         "xacquire lock " #_insn "q (%0)"     "\n\t" \
         "xrelease lock " #_insn "q (%0)"     "\n\t" \
         "xacquire lock " #_insn "l (%0)"     "\n\t" \
         "xrelease lock " #_insn "l (%0)"     "\n\t" \
         "xacquire lock " #_insn "w (%0)"     "\n\t" \
         "xrelease lock " #_insn "w (%0)"     "\n\t" \
         "xacquire lock " #_insn "b (%0)"     "\n\t" \
         "xrelease lock " #_insn "b (%0)"     "\n\t" \
         : : "r"(&n) : "cc", "memory" \
      ); \
      printf("result for '%-3s' is %016llx\n", #_insn, n);  \
   }

GEN_UNARY(dec)
GEN_UNARY(inc)
GEN_UNARY(neg)
GEN_UNARY(not)

void do_btc ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   __asm__ __volatile__(
      "xacquire lock btcq %1,  (%0)"     "\n\t"
      "xacquire lock btcq $57, (%0)"     "\n\t"
      "xrelease lock btcq %1,  (%0)"     "\n\t"
      "xrelease lock btcq $55, (%0)"     "\n\t"
      "xacquire lock btcl %k1, (%0)"     "\n\t"
      "xacquire lock btcl $27, (%0)"     "\n\t"
      "xrelease lock btcl %k1, (%0)"     "\n\t"
      "xrelease lock btcl $25, (%0)"     "\n\t"
      "xacquire lock btcw %w1, (%0)"     "\n\t"
      "xacquire lock btcw $12, (%0)"     "\n\t"
      "xrelease lock btcw %w1, (%0)"     "\n\t"
      "xrelease lock btcw $11, (%0)"     "\n\t"
      : : "r"(&n), "r"(6ULL) : "cc", "memory"
   );
   printf("result for '%-3s' is %016llx\n", "btc", n); \
}

void do_btr ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   __asm__ __volatile__(
      "xacquire lock btrq %1,  (%0)"     "\n\t"
      "xacquire lock btrq $57, (%0)"     "\n\t"
      "xrelease lock btrq %1,  (%0)"     "\n\t"
      "xrelease lock btrq $55, (%0)"     "\n\t"
      "xacquire lock btrl %k1, (%0)"     "\n\t"
      "xacquire lock btrl $27, (%0)"     "\n\t"
      "xrelease lock btrl %k1, (%0)"     "\n\t"
      "xrelease lock btrl $25, (%0)"     "\n\t"
      "xacquire lock btrw %w1, (%0)"     "\n\t"
      "xacquire lock btrw $12, (%0)"     "\n\t"
      "xrelease lock btrw %w1, (%0)"     "\n\t"
      "xrelease lock btrw $11, (%0)"     "\n\t"
      : : "r"(&n), "r"(6ULL) : "cc", "memory"
   );
   printf("result for '%-3s' is %016llx\n", "btr", n); \
}

void do_bts ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   __asm__ __volatile__(
      "xacquire lock btsq %1,  (%0)"     "\n\t"
      "xacquire lock btsq $57, (%0)"     "\n\t"
      "xrelease lock btsq %1,  (%0)"     "\n\t"
      "xrelease lock btsq $55, (%0)"     "\n\t"
      "xacquire lock btsl %k1, (%0)"     "\n\t"
      "xacquire lock btsl $27, (%0)"     "\n\t"
      "xrelease lock btsl %k1, (%0)"     "\n\t"
      "xrelease lock btsl $25, (%0)"     "\n\t"
      "xacquire lock btsw %w1, (%0)"     "\n\t"
      "xacquire lock btsw $12, (%0)"     "\n\t"
      "xrelease lock btsw %w1, (%0)"     "\n\t"
      "xrelease lock btsw $11, (%0)"     "\n\t"
      : : "r"(&n), "r"(6ULL) : "cc", "memory"
   );
   printf("result for '%-3s' is %016llx\n", "bts", n); \
}

void do_cmpxchg ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   ULong some = 0x271831415927D459ULL;
   __asm__ __volatile__(
      "\t"
      "stc"                                         "\n\t"
      // zero out rax and get the flags in a known state
      "xorq    %%rax, %%rax"                        "\n\t"
      "xacquire lock cmpxchgq %1,         (%0)"     "\n\t"
      "xrelease lock cmpxchgq %1,         (%0)"     "\n\t"
      "xacquire lock cmpxchgl %k1,        (%0)"     "\n\t"
      "xrelease lock cmpxchgl %k1,        (%0)"     "\n\t"
      "xacquire lock cmpxchgw %w1,        (%0)"     "\n\t"
      "xrelease lock cmpxchgw %w1,        (%0)"     "\n\t"
      "xacquire lock cmpxchgb %b1,        (%0)"     "\n\t"
      "xrelease lock cmpxchgb %b1,        (%0)"     "\n\t"
      : : "r"(&n), "r"(some) : "cc", "memory", "rax"
   );
   printf("result for '%-3s' is %016llx\n", "cmpxchg", n);
}

void do_cmpxchg8b ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   __asm__ __volatile__(
      "xorq     %%rax, %%rax"     "\n\t"
      "xorq     %%rdx, %%rdx"     "\n\t"
      "movabsq $0x1122334455667788, %%rcx"   "\n\t"
      "movabsq $0xffeeddccbbaa9988, %%rdx"   "\n\t"
      "xacquire lock cmpxchg8b (%0)"     "\n\t"
      "xrelease lock cmpxchg8b (%0)"     "\n\t"
      : : "r"(&n) : "cc", "memory", "rax", "rdx", "rcx", "rdx"
   );
   printf("result for '%-3s' is %016llx\n", "cmpxchg8b", n);
}

void do_xadd ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   ULong some = 0x271831415927D459ULL;
   __asm__ __volatile__(
      "\t"
      "stc"                                         "\n\t"
      // zero out rax and get the flags in a known state
      "xorq    %%rax, %%rax"                        "\n\t"
      "xacquire lock xaddq %1,         (%0)"     "\n\t"
      "xrelease lock xaddq %1,         (%0)"     "\n\t"
      "xacquire lock xaddl %k1,        (%0)"     "\n\t"
      "xrelease lock xaddl %k1,        (%0)"     "\n\t"
      "xacquire lock xaddw %w1,        (%0)"     "\n\t"
      "xrelease lock xaddw %w1,        (%0)"     "\n\t"
      "xacquire lock xaddb %b1,        (%0)"     "\n\t"
      "xrelease lock xaddb %b1,        (%0)"     "\n\t"
      : : "r"(&n), "r"(some) : "cc", "memory", "rax"
      // not sure this constraint string is really correct, since %1
      // is written as well as read, in this case.  But I can't figure
      // out how to tell gcc that.
   );
   printf("result for '%-3s' is %016llx\n", "xadd", n);
}

void do_xchg ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   ULong some = 0x271831415927D459ULL;
   __asm__ __volatile__(
      "\t"
      "stc"                                         "\n\t"
      // zero out rax and get the flags in a known state
      "xorq    %%rax, %%rax"                        "\n\t"
      "xacquire lock xchgq %1,         (%0)"     "\n\t"
      "xrelease lock xchgq %1,         (%0)"     "\n\t"
      "xacquire lock xchgl %k1,        (%0)"     "\n\t"
      "xrelease lock xchgl %k1,        (%0)"     "\n\t"
      "xacquire lock xchgw %w1,        (%0)"     "\n\t"
      "xrelease lock xchgw %w1,        (%0)"     "\n\t"
      "xacquire lock xchgb %b1,        (%0)"     "\n\t"
      "xrelease lock xchgb %b1,        (%0)"     "\n\t"
      : : "r"(&n), "r"(some) : "cc", "memory", "rax"
      // not sure this constraint string is really correct, since %1
      // is written as well as read, in this case.  But I can't figure
      // out how to tell gcc that.
   );
   printf("result for '%-3s' is %016llx\n", "xchg", n);
}

void do_xchg_no_lock ( void )
{
   volatile ULong n = 0x5555555555555555ULL;
   ULong some = 0x271831415927D459ULL;
   __asm__ __volatile__(
      "\t"
      "stc"                                         "\n\t"
      // zero out rax and get the flags in a known state
      "xorq    %%rax, %%rax"                        "\n\t"
      "xacquire xchgq %1,         (%0)"     "\n\t"
      "xrelease xchgq %1,         (%0)"     "\n\t"
      "xacquire xchgl %k1,        (%0)"     "\n\t"
      "xrelease xchgl %k1,        (%0)"     "\n\t"
      "xacquire xchgw %w1,        (%0)"     "\n\t"
      "xrelease xchgw %w1,        (%0)"     "\n\t"
      "xacquire xchgb %b1,        (%0)"     "\n\t"
      "xrelease xchgb %b1,        (%0)"     "\n\t"
      : : "r"(&n), "r"(some) : "cc", "memory", "rax"
      // not sure this constraint string is really correct, since %1
      // is written as well as read, in this case.  But I can't figure
      // out how to tell gcc that.
   );
   printf("result for '%-3s' is %016llx\n", "xchg-no-lock", n);
}

void do_mov ( void )
{
   // According to the Intel docs, we only need to allow xrelease here.
   volatile ULong n = 0x5555555555555555ULL;
   ULong some = 0x271831415927D459ULL;
   __asm__ __volatile__(
      "\t"
      "xrelease movq %1,   0(%0)"     "\n\t"
      "xrelease movl %k1,  1(%0)"     "\n\t"
      "xrelease movw %w1,  3(%0)"     "\n\t"
      "xrelease movb %b1,  7(%0)"     "\n\t"
      : : "r"(&n), "r"(some) : "cc", "memory"
   );
   printf("result for '%-3s' is %016llx\n", "mov-reg", n);
   n = 0xAAAAAAAAAAAAAAAAULL;
   __asm__ __volatile__(
      "\t"
      "xrelease movq $-0x79876543, 0(%0)"     "\n\t"
      "xrelease movl $0xEFCDAB89, 1(%0)"     "\n\t"
      "xrelease movw $0xF00D,     3(%0)"     "\n\t"
      "xrelease movb $0x42,       7(%0)"     "\n\t"
      : : "r"(&n) : "cc", "memory"
   );
   printf("result for '%-3s' is %016llx\n", "mov-imm", n);
}

int main ( void )
{
  do_add();
  do_adc();
  do_and();
  do_or();
  do_sbb();
  do_sub();
  do_xor();
  do_dec();
  do_inc();
  do_neg();
  do_not();
  do_btc();
  do_btr();
  do_bts();
  do_cmpxchg();
  do_cmpxchg8b();
  do_xadd();
  do_xchg();
  do_xchg_no_lock();
  do_mov();
  return 0;
}
