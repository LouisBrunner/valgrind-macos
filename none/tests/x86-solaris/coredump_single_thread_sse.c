/* Tests that Valgrind coredump support for SSE registers works correctly
   by producing a core dump analyzable by mdb.
   Basic register set is tested in coredump_single_thread. */

#include <stdio.h>
#include <sys/types.h>

__attribute__((noinline))
static void inner(void)
{
   /* Set XMM registers to apriori known values.
      Unfortunately there is no instruction to load
      an immediate value directly into xmm register. */
   __asm__ __volatile__("\n"
      "pushl $0x12345678\n"
      "pushl $0x9abcdef0\n"
      "pushl $0xfedbca98\n"
      "pushl $0x76543210\n"
      "movups (%%esp), %%xmm0\n"
      "pushl $0x23456789\n"
      "pushl $0x09876543\n"
      "pushl $0x21fedcba\n"
      "pushl $0x9467feca\n"
      "movups (%%esp), %%xmm1\n"
      "pushl $0xabcdabcd\n"
      "pushl $0xcedecede\n"
      "pushl $0xfabafaba\n"
      "pushl $0x50656754\n"
      "movups (%%esp), %%xmm2\n"
      "pushl $0x03050608\n"
      "pushl $0x1d1b4b15\n"
      "pushl $0x25272120\n"
      "pushl $0x373a3d35\n"
      "movups (%%esp), %%xmm3\n"
      "pushl $0x9abcdef0\n"
      "pushl $0x76543210\n"
      "pushl $0x12345678\n"
      "pushl $0xfedbca98\n"
      "movups (%%esp), %%xmm4\n"
      "pushl $0x9467feca\n"
      "pushl $0x23456789\n"
      "pushl $0x21fedcba\n"
      "pushl $0x09876543\n"
      "movups (%%esp), %%xmm5\n"
      "pushl $0x50656754\n"
      "pushl $0xcedecede\n"
      "pushl $0xabcdabcd\n"
      "pushl $0xfabafaba\n"
      "movups (%%esp), %%xmm6\n"
      "pushl $0x373a3d35\n"
      "pushl $0x1d1b4b15\n"
      "pushl $0x03050608\n"
      "pushl $0x25272120\n"
      "movups (%%esp), %%xmm7\n"
      "movl $0x1, %%eax\n"
      "movl $0x1234, (%%eax)\n"  // should cause SEGV here
      : // no output registers
      : // no input registers
      : "memory", "%xmm0", "%xmm1", "%xmm2", "%xmm3",
                  "%xmm4", "%xmm5", "%xmm6", "%xmm7");
}

__attribute__((noinline))
static void outer(void)
{
   inner();
}

int main(int argc, const char *argv[])
{
   outer();
   return 0;
}
