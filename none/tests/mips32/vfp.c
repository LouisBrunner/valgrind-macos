#include <stdio.h>

unsigned int mem[] = {
   0x4095A266, 0x66666666,
   0xBFF00000, 0x00000000,
   0x3FF00000, 0x00000000,
   0x252a2e2b, 0x262d2d2a,
   0xFFFFFFFF, 0xFFFFFFFF,
   0x41D26580, 0xB487E5C9,
   0x42026580, 0xB750E388,
   0x3E45798E, 0xE2308C3A,
   0x3FBF9ADD, 0x3746F65F
};

float fs_f[] = {
   0, 456.2489562, 3, -1,
   1384.6, -7.2945676, 1000000000, -5786.47,
   1752, 0.0024575, 0.00000001, -248562.76,
   -45786.476, 456.2489562, 34.00046, 45786.476,
   1752065, 107, -45667.24, -7.2945676,
   -347856.475, 356047.56, -1.0, 23.04
};

double fs_d[] = {
   0, 456.2489562, 3, -1,
   1384.6, -7.2945676, 1000000000, -5786.47,
   1752, 0.0024575, 0.00000001, -248562.76,
   -45786.476, 456.2489562, 34.00046, 45786.476,
   1752065, 107, -45667.24, -7.2945676,
   -347856.475, 356047.56, -1.0, 23.04
};

double mem1[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0
};

float mem1f[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0
};
// ldc1 $f0, 0($t1)
#define TESTINSN5LOAD(instruction, RTval, offset, RT) \
{ \
    double out; \
    int out1; \
    int out2; \
   __asm__ volatile( \
     "move $t1, %3\n\t" \
     "li $t0, " #RTval"\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #RT "\n\t" \
     "mfc1 %1, $" #RT "\n\t" \
     "mfc1 %2, $f1\n\t" \
     : "=&f" (out), "=&r" (out1), "=&r" (out2) \
	 : "r" (mem), "r" (RTval) \
	 : "cc", "memory" \
	 ); \
   printf("%s :: ft 0x%x%x\n", \
          instruction, out1, out2); \
}

// lwc1 $f0, 0($t1)
#define TESTINSN5LOADw(instruction, RTval, offset, RT) \
{ \
    double out; \
    int out1; \
   __asm__ volatile( \
     "move $t1, %2\n\t" \
     "li $t0, " #RTval"\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #RT "\n\t" \
     "mfc1 %1, $" #RT "\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (mem), "r" (RTval) \
	 : "cc", "memory" \
	 ); \
   printf("%s :: ft 0x%x\n", \
          instruction, out1); \
}

// lwxc1 $f0, $a3($v0)
#define TESTINSN6LOADw(instruction, indexVal, fd, index, base) \
{ \
    int out; \
   __asm__ volatile( \
     "move $" #base ", %1\n\t" \
     "li $" #index ", " #indexVal"\n\t" \
     instruction "\n\t" \
     "mfc1 %0, $" #fd "\n\t" \
     : "=&r" (out) \
	 : "r" (mem) \
	 : "cc", "memory" \
	 ); \
   printf("%s :: ft 0x%x\n", \
          instruction, out); \
}

// ldxc1 $f0, $a3($v0)
#define TESTINSN6LOADd(instruction, indexVal, fd, index, base) \
{ \
    int out; \
    int out1; \
    int out2; \
   __asm__ volatile( \
     "move $" #base ", %3\n\t" \
     "li $" #index ", " #indexVal"\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #fd "\n\t" \
     "mfc1 %1, $" #fd "\n\t" \
     "mfc1 %2, $f1\n\t" \
     : "=&f" (out), "=&r" (out1), "=&r" (out2) \
	 : "r" (mem) \
	 : "cc", "memory" \
	 ); \
   printf("%s :: ft 0x%x\n", \
          instruction, out); \
}
// sdc1 $f0, 0($t0)
#define TESTINST1(offset) \
{ \
    unsigned int out; \
   __asm__ volatile( \
     "move $t0, %1\n\t" \
     "move $t1, %2\n\t" \
     "ldc1 $f0, "#offset"($t1)\n\t" \
     "sdc1 $f0, "#offset"($t0) \n\t" \
     "lw %0, "#offset"($t0)\n\t" \
     : "=&r" (out) \
	 : "r" (mem1), "r" (fs_d) \
	 : "t1", "t0", "cc", "memory" \
	 ); \
   printf("sdc1 $f0, 0($t0) :: out: 0x%x\n", \
           out); \
}

// sdxc1 $f0, $t2($t0)
#define TESTINST1a(offset) \
{ \
    unsigned int out; \
    unsigned int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "move $t1, %3\n\t" \
     "li $t2, "#offset"\n\t" \
     "ldc1 $f0, "#offset"($t1)\n\t" \
     "sdxc1 $f0, $t2($t0) \n\t" \
     "lw %0, "#offset"($t0)\n\t" \
     "addi $t0, $t0, 4 \n\t" \
     "lw %1, "#offset"($t0)\n\t" \
     : "=&r" (out), "=&r" (out1) \
	 : "r" (mem1), "r" (fs_d) \
	 : "t2", "t1", "t0", "cc", "memory" \
	 ); \
   printf("sdc1 $f0, #t2($t0) :: out: 0x%x : out1: 0x%x\n", \
           out, out1); \
}

// swc1 $f0, 0($t0)
#define TESTINST2(offset) \
{ \
    unsigned int out; \
   __asm__ volatile( \
     "move $t0, %1\n\t" \
     "move $t1, %2\n\t" \
     "lwc1 $f0, "#offset"($t1)\n\t" \
     "swc1 $f0, "#offset"($t0) \n\t" \
     "lw %0, "#offset"($t0)\n\t" \
     : "=&r" (out) \
	 : "r" (mem1f), "r" (fs_f) \
	 : "t1", "t0", "cc", "memory" \
	 ); \
   printf("swc1 $f0, 0($t0) :: out: 0x%x\n", \
           out); \
}

// SWXC1 $f0, $t2($t0)
#define TESTINST2a(offset) \
{ \
    unsigned int out; \
   __asm__ volatile( \
     "move $t0, %1\n\t" \
     "move $t1, %2\n\t" \
     "li $t2, "#offset" \n\t" \
     "lwc1 $f0, "#offset"($t1)\n\t" \
     "swxc1 $f0, $t2($t0) \n\t" \
     "lw %0, "#offset"($t0)\n\t" \
     : "=&r" (out) \
	 : "r" (mem1f), "r" (fs_f) \
	 : "t2", "t1", "t0", "cc", "memory" \
	 ); \
   printf("swxc1 $f0, 0($t0) :: out: 0x%x\n", \
           out); \
}
void ppMem(double *mem, int len)
{
   int i;
   printf("MEM1:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("%lf, %lf, %lf, %lf\n", mem[i], mem[i+1], mem[i+2], mem[i+3]);
      mem[i] = 0;
      mem[i+1] = 0;
      mem[i+2] = 0;
      mem[i+3] = 0;
   }
}

void ppMemF(float *mem, int len)
{
   int i;
   printf("MEM1:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("%lf, %lf, %lf, %lf\n", mem[i], mem[i+1], mem[i+2], mem[i+3]);
      mem[i] = 0;
      mem[i+1] = 0;
      mem[i+2] = 0;
      mem[i+3] = 0;
   }
}

int main()
{
   printf("LDC1\n");
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);

   printf("LWC1\n");
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOADw("lwc1 $f0, 4($t1)", 0, 4, f0);
   TESTINSN5LOADw("lwc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOADw("lwc1 $f0, 12($t1)", 0, 12, f0);
   TESTINSN5LOADw("lwc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOADw("lwc1 $f0, 20($t1)", 0, 20, f0);
   TESTINSN5LOADw("lwc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOADw("lwc1 $f0, 28($t1)", 0, 28, f0);
   TESTINSN5LOADw("lwc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOADw("lwc1 $f0, 36($t1)", 0, 36, f0);
   TESTINSN5LOADw("lwc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOADw("lwc1 $f0, 44($t1)", 0, 44, f0);
   TESTINSN5LOADw("lwc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOADw("lwc1 $f0, 52($t1)", 0, 52, f0);
   TESTINSN5LOADw("lwc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOADw("lwc1 $f0, 60($t1)", 0, 60, f0);
   TESTINSN5LOADw("lwc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOADw("lwc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOADw("lwc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOADw("lwc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOADw("lwc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOADw("lwc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOADw("lwc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOADw("lwc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOADw("lwc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);

#if (__mips==32) && (__mips_isa_rev>=2)
   printf("LWXC1\n");
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 4, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 12, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 20, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 28, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 36, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 44, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 52, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 60, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 4, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 12, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 20, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 28, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 36, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 44, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 52, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 56, f0, a3, v0);

   printf("LDXC1\n");
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
#endif

   printf("SDC1\n");
   TESTINST1(0);
   TESTINST1(8);
   TESTINST1(16);
   TESTINST1(24);
   TESTINST1(32);
   TESTINST1(40);
   TESTINST1(48);
   TESTINST1(56);
   TESTINST1(64);
   ppMem(mem1, 16);

#if (__mips==32) && (__mips_isa_rev>=2) 
   printf("SDXC1\n");
   TESTINST1a(0);
   TESTINST1a(8);
   TESTINST1a(16);
   TESTINST1a(24);
   TESTINST1a(32);
   TESTINST1a(40);
   TESTINST1a(48);
   TESTINST1a(56);
   TESTINST1a(64);
   ppMem(mem1, 16);
#endif

   printf("SWC1\n");
   TESTINST2(0);
   TESTINST2(8);
   TESTINST2(16);
   TESTINST2(24);
   TESTINST2(32);
   TESTINST2(40);
   TESTINST2(48);
   TESTINST2(56);
   TESTINST2(64);
   ppMemF(mem1f, 16);

#if (__mips==32) && (__mips_isa_rev>=2) 
   printf("SWXC1\n");
   TESTINST2a(0);
   TESTINST2a(8);
   TESTINST2a(16);
   TESTINST2a(24);
   TESTINST2a(32);
   TESTINST2a(40);
   TESTINST2a(48);
   TESTINST2a(56);
   TESTINST2a(64);
   ppMemF(mem1f, 16);
#endif

   return 0;
}

