/* This test used to cause an assertion in the address space manager */

__attribute__((noinline))
static void inner(void)
{
   /* Set other registers to apriori known values. */
   __asm__ __volatile__(
      "movl $0x101, %%eax\n"
      "movl $0x102, %%ebx\n"
      "movl $0x103, %%ecx\n"
      "movl $0x104, %%edx\n"
      "movl $0x105, %%esi\n"
      "movl $0x106, %%edi\n"
      // not %ebp as mdb is then not able to reconstruct stack trace
      // clobbering %esp is really bad, but that is kind of the point
      // we don't add it to the clobber list since gcc9 warns about that
      "movl $0x108, %%esp\n"
      "movl $0x1234, (%%eax)\n"  // should cause SEGV here
      "ud2"                      // should never get here
      : // no output registers
      : // no input registers
      : "memory", "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi");
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
