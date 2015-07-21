/* Tests that Valgrind coredump support works correctly by producing
   a core dump analyzable by mdb. */ 

#include <stdio.h>
#include <sys/types.h>

__attribute__((noinline))
static void inner(void)
{
   uint16_t cs, ds, ss, es, fs, gs;

   /* Print segment registers as they cannot be set to arbitrary
      values. */
   __asm__ __volatile__(
      "movw %%cs, %0\n"
      "movw %%ds, %1\n"
      "movw %%ss, %2\n"
      "movw %%es, %3\n"
      "movw %%fs, %4\n"
      "movw %%gs, %5\n"
      : "=m" (cs), "=m" (ds), "=m" (ss), "=m" (es), "=m" (fs), "=m" (gs));
   printf("cs=%#x ds=%#x ss=%#x es=%#x fs=%#x gs=%#x\n",
          cs, ds, ss, es, fs, gs);

   /* Set other registers to apriori known values. */
   __asm__ __volatile__(
      "movl $0x101, %%eax\n"
      "movl $0x102, %%ebx\n"
      "movl $0x103, %%ecx\n"
      "movl $0x104, %%edx\n"
      "movl $0x105, %%esi\n"
      "movl $0x106, %%edi\n"
      // not %ebp as mdb is then not able to reconstruct stack trace
      "movl $0x108, %%esp\n"
      "movl $0x1234, (%%eax)\n"  // should cause SEGV here
      "ud2"                      // should never get here
      : // no output registers
      : // no input registers
      : "memory", "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%esp");
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
