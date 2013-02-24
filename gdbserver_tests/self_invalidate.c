/* Based on reproducer done by Dejan Jevtic */
int main (void)
{
#if defined(VGA_amd64)
   /* Note that small changes in the code below 
      caused the bug not to be triggered anymore.
      E.g. removing the dec %%eax avoids the assert
      while this removal causes one more loop to be
      executed. */
__asm__ __volatile__
   ("mov $30, %%eax\n\t"
    "top:\n\t"
    "mov $-4, %%ebx\n\t"
    "add %%ebx, %%eax\n\t"
    "dec %%eax\n\t"
    "cmp    $0x0,%%eax\n\t"
    "jne top\n\t"
    "mov $60, %%eax\n\t"
    "mov $0, %%rdi\n\t"
    "syscall\n\t"
    : : : "eax", "ebx", "rdi"
    );
#elif defined(VGA_mips32) || defined(VGA_mips64)
__asm__ __volatile__
   ("li $t0, 42\n\t"
    "top:\n\t"
    "li $t1, -4\n\t"
    "addu $t0, $t0, $t1\n\t"
    "li $t2, -2\n\t"
    "addu $t0, $t0, $t2\n\t"
    "addiu $t0, $t0, -1\n\t"
    "bnez $t0, top\n\t"
    "nop\n\t"
    : : : "t0", "t1"
    );
#endif
 return 0;
}
