/* This test used to cause an assertion in the address space manager */

__attribute__((noinline))
static void inner(void)
{
   /* Set registers to apriori known values. */
   __asm__ __volatile__(
      "movq $0x101, %%rax\n"
      "movq $0x102, %%rbx\n"
      "movq $0x103, %%rcx\n"
      "movq $0x104, %%rdx\n"
      "movq $0x105, %%rsi\n"
      "movq $0x106, %%rdi\n"
      "movq $0x107, %%r8\n"
      "movq $0x108, %%r9\n"
      "movq $0x109, %%r10\n"
      "movq $0x10a, %%r11\n"
      "movq $0x10b, %%r12\n"
      "movq $0x10c, %%r13\n"
      "movq $0x10d, %%r14\n"
      "movq $0x10e, %%r15\n"
      // not %rbp as mdb is then not able to reconstruct stack trace
      // Do change %rsp (to test a bogus stack pointer),
      // but don't add %rsp to the clobber list since gcc ignores it
      // and since gcc >= 9.0 errors about it
      // see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52813
      "movq $0x10f, %%rsp\n"
      "movq $0x1234, (%%rax)\n"  // should cause SEGV here
      "ud2"                      // should never get here
      : // no output registers
      : // no input registers
      : "memory", "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi",
        "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15");
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
