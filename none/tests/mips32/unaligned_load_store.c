#include <stdio.h>
unsigned int mem[] = {
   0xaabbccdd, 0x11223344, 0x01823194, 0x01823a08,
   0x00000000, 0x77ff528c, 0x77deb460, 0x00000001
};

void printMem(char* s)
{
   int i;
   printf("%s\n", s);
   for (i=0; i<7 ; i=i+1)
      printf("mem[%d]: 0x%x\n", i, mem[i]);
}

int main ()
{
   printMem("PRE lwl");
   __asm__ volatile("move $a0, %0"       "\n\t"
                    "lw   $t0, 0($a0)"   "\n\t"
                    "lwl  $t0, 4($a0)"   "\n\t"
                    "sw   $t0, 8($a0)"   "\n\t"
                    "lw   $t1, 0($a0)"   "\n\t"
                    "lwl  $t1, 5($a0)"   "\n\t"
                    "sw   $t1, 12($a0)"  "\n\t"
                    "lw   $t2, 0($a0)"   "\n\t"
                    "lwl  $t2, 6($a0)"   "\n\t"
                    "sw   $t2, 16($a0)"  "\n\t"
                    "lw   $t3, 0($a0)"   "\n\t"
                    "lwl  $t3, 7($a0)"   "\n\t"
                    "sw   $t3, 20($a0)"  "\n\t"
                    :
                    : "r" (mem)
                    : "a0", "t0", "t1", "t2", "t3", "cc", "memory"
                   );
   printMem("POST lwl");

   mem[0] = 0xaabbccdd;
   mem[1] = 0x11223344;
   mem[2] = 0x01823194;
   mem[3] = 0x01823a08;
   mem[4] = 0x00000000;
   mem[5] = 0x77ff528c;
   mem[6] = 0x77deb460;
   mem[7] = 0x00000001;

   printMem("PRE lwr");
   __asm__ volatile("move $a0, %0"       "\n\t"
                    "lw   $t0, 0($a0)"   "\n\t"
                    "lwr  $t0, 4($a0)"   "\n\t"
                    "sw   $t0, 8($a0)"   "\n\t"
                    "lw   $t1, 0($a0)"   "\n\t"
                    "lwr  $t1, 5($a0)"   "\n\t"
                    "sw   $t1, 12($a0)"  "\n\t"
                    "lw   $t2, 0($a0)"   "\n\t"
                    "lwr  $t2, 6($a0)"   "\n\t"
                    "sw   $t2, 16($a0)"  "\n\t"
                    "lw   $t3, 0($a0)"   "\n\t"
                    "lwr  $t3, 7($a0)"   "\n\t"
                    "sw   $t3, 20($a0)"  "\n\t"
                    :
                    : "r" (mem)
                    : "a0", "t0", "t1", "t2", "t3", "cc", "memory"
                   );
   printMem("POST lwr");

   return 0;
}
