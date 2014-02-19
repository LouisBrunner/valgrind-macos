#include <stdio.h>

const float fs_f[] = {
   0, 456.2489562, 3, -1,
   1384.6, -7.2945676, 1000000000, -5786.47,
   1752, 0.0024575, 0.00000001, -248562.76,
   -45786.476, 456.2489562, 34.00046, 45786.476,
   1752065, 107, -45667.24, -7.2945676,
   -347856.475, 356047.56, -1.0, 23.04
};

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

// mfc1 rt, fs
#define TESTINSNMOVE(instruction, offset, FS, RT) \
{ \
    float out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "lwc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.s %0, $" #FS"\n\t" \
     "move %1, $" #RT "\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (mem) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs %f, rt 0x%x\n", \
          instruction, out, out1); \
}

// mfhc1 rt, fs
#define TESTINSNMOVEd(instruction, offset, FS, RT) \
{ \
    double out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "ldc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FS"\n\t" \
     "move %1, $" #RT "\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (mem) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs %lf, rt 0x%x\n", \
          instruction, out, out1); \
}

// mtc1 rt, fs
#define TESTINSNMOVEt(instruction, offset, FS, RT) \
{ \
    float out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "lw $" #RT ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.s %0, $" #FS"\n\t" \
     "move %1, $" #RT "\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (mem) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs %f, rt 0x%x\n", \
          instruction, out, out1); \
}

// mthc1 rt, fs
#define TESTINSNMOVEtd(instruction, offset, FS, RT) \
{ \
    double out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "lw $" #RT ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FS"\n\t" \
     "move %1, $" #RT "\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (mem) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs %lf, rt 0x%x\n", \
          instruction, out, out1); \
}

// mov.s fd, fs
#define TESTINSNMOVE1s(instruction, offset, FD, FS) \
{ \
    float out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "lwc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.s %0, $" #FD"\n\t" \
     "mfc1 %1, $" #FD"\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (fs_f) \
	 : "cc", "memory" \
	 ); \
   printf("%s :: fs %f, rt 0x%x\n", \
          instruction, out, out1); \
}

// mov.d fd, fs
#define TESTINSNMOVE1d(instruction, offset, FD, FS) \
{ \
    double out; \
    int out1; \
   __asm__ volatile( \
     "move $t0, %2\n\t" \
     "ldc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FD"\n\t" \
     "mfc1 %1, $" #FD"\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (fs_f) \
	 : "cc", "memory" \
	 ); \
   printf("%s ::fs %f, rt 0x%x\n", \
          instruction, out, out1); \
}

// movf rd, rs
#define TESTINSNMOVE2(instruction, RDval, RSval, RD, RS, cc) \
{ \
    int out; \
   __asm__ volatile( \
     "li $t0, 1\n\t" \
     "move $t1, %3\n\t" \
     "mtc1 $t0, $f0\n\t" \
     "mtc1 $t1, $f2\n\t" \
     "c.eq.s $f0, $f2\n\t" \
     "move $" #RS ", %1\n\t" \
     "move $" #RD ", %2\n\t" \
     instruction "\n\t" \
     "move %0, $" #RD "\n\t" \
     : "=&r" (out) \
	 : "r" (RSval), "r" (RDval), "r" (cc) \
	 : "t0", "t1", #RD, #RS, "cc", "memory" \
	 ); \
   printf("%s :: out: 0x%x, RDval: 0x%x, RSval: 0x%x, cc: %d\n", \
          instruction, out, RDval, RSval, cc); \
}

// movf.s fd, fs
#define TESTINSNMOVE2s(instruction, FD, FS, cc, offset) \
{ \
   float out; \
   __asm__ volatile( \
     "li $t0, 1\n\t" \
     "move $t1, %1\n\t" \
     "mtc1 $t0, $f0\n\t" \
     "mtc1 $t1, $f2\n\t" \
     "c.eq.s $f0, $f2\n\t" \
     "move $t0, %2\n\t" \
     "lwc1 $" #FD ", 4($t0)\n\t" \
     "lwc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.s %0, $" #FD"\n\t" \
     : "=&f" (out) \
	 : "r" (cc), "r" (fs_f) \
	 : "t0", "t1", "cc", "memory" \
	 ); \
   printf("%s :: out: %f, cc: %d\n", \
          instruction, out, cc); \
}

// movf.d fd, fs
#if (__mips_fpr==64)
#define TESTINSNMOVE2d(instruction, FD, FS, cc, offset) \
{ \
   double out; \
   int out1; \
   int out2; \
   __asm__ volatile( \
     "li $t0, 1\n\t" \
     "mtc1 $t0, $f0\n\t" \
     "mtc1 %3,  $f2\n\t" \
     "move $t0, %4\n\t" \
     "ldc1 $f4, 8($t0)\n\t" \
     "c.eq.s $f0, $f2\n\t" \
     "ldc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FD"\n\t" \
     "mfc1 %1, $f4\n\t" \
     "mfhc1 %2, $f4\n\t" \
     : "=&f" (out), "=&r" (out1), "=&r" (out2) \
	 : "r" (cc), "r" (mem) \
	 : "t0", "t1", "cc", "memory" \
	 ); \
   printf("%s :: out: 0x%x 0x%x, cc: %d\n", \
          instruction, out1, out2, cc); \
}
#else
#define TESTINSNMOVE2d(instruction, FD, FS, cc, offset) \
{ \
   double out; \
   int out1; \
   int out2; \
   __asm__ volatile( \
     "li $t0, 1\n\t" \
     "move $t1, %3\n\t" \
     "mtc1 $t0, $f0\n\t" \
     "mtc1 $t1, $f2\n\t" \
     "move $t0, %4\n\t" \
     "ldc1 $f4, 8($t0)\n\t" \
     "c.eq.s $f0, $f2\n\t" \
     "ldc1 $" #FS ", "#offset"($t0)\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FD"\n\t" \
     "mfc1 %1, $f4\n\t" \
     "mfc1 %2, $f5\n\t" \
     : "=&f" (out), "=&r" (out1), "=&r" (out2) \
	 : "r" (cc), "r" (mem) \
	 : "t0", "t1", "cc", "memory" \
	 ); \
   printf("%s :: out: 0x%x 0x%x, cc: %d\n", \
          instruction, out1, out2, cc); \
}
#endif

// movn.s fd, fs, rt
#define TESTINSNMOVEN1s(instruction, offset, RTval, FD, FS, RT) \
{ \
    float out; \
    int out1; \
   __asm__ volatile( \
     "move $" #RT ", %3\n\t" \
     "move $t0, %2\n\t" \
     "lwc1 $" #FS ", "#offset"($t0)\n\t" \
     "mtc1 $0, $" #FD "\n\t" \
     instruction "\n\t" \
     "mov.s %0, $" #FD"\n\t" \
     "mfc1 %1, $" #FD"\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (fs_f), "r" (RTval) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs rt 0x%x\n", \
          instruction, out1); \
}

// movn.d fd, fs, rt
#define TESTINSNMOVEN1d(instruction, offset, RTval, FD, FS, RT) \
{ \
    double out; \
    int out1; \
   __asm__ volatile( \
     "move $" #RT ", %3\n\t" \
     "move $t0, %2\n\t" \
     "ldc1 $" #FS ", "#offset"($t0)\n\t" \
     "mtc1 $0, $" #FD "\n\t" \
     "mtc1 $0, $" #FD + 1"\n\t" \
     instruction "\n\t" \
     "mov.d %0, $" #FD"\n\t" \
     "mfc1 %1, $" #FD"\n\t" \
     : "=&f" (out), "=&r" (out1) \
	 : "r" (fs_f), "r" (RTval) \
	 : #RT, "cc", "memory" \
	 ); \
   printf("%s :: fs %lf, rt 0x%x\n", \
          instruction, out, out1); \
}

int main()
{
   printf("MFC1\n");
   TESTINSNMOVE("mfc1 $t1, $f0",  0, f0, t1);
   TESTINSNMOVE("mfc1 $t2, $f1", 4, f1, t2);
   TESTINSNMOVE("mfc1 $t3, $f2",  8, f2, t3);
   TESTINSNMOVE("mfc1 $t4, $f3", 12, f3, t4);
   TESTINSNMOVE("mfc1 $t5, $f4", 16, f4, t5);
   TESTINSNMOVE("mfc1 $t6, $f5", 20, f5, t6);
   TESTINSNMOVE("mfc1 $t7, $f6", 24, f6, t7);
   TESTINSNMOVE("mfc1 $v0, $f7", 28, f7, v0);
   TESTINSNMOVE("mfc1 $v1, $f8", 32, f8, v1);
   TESTINSNMOVE("mfc1 $s0, $f9", 36, f9, s0);
   TESTINSNMOVE("mfc1 $s1, $f10", 40, f10, s1);
   TESTINSNMOVE("mfc1 $s2, $f11", 44, f11, s2);
   TESTINSNMOVE("mfc1 $s3, $f12", 48, f12, s3);
   TESTINSNMOVE("mfc1 $s4, $f13", 52, f13, s4);
   TESTINSNMOVE("mfc1 $s5, $f14", 56, f14, s5);
   TESTINSNMOVE("mfc1 $s6, $f15", 60, f15, s6);
   TESTINSNMOVE("mfc1 $s7, $f16", 64, f16, s7);
   TESTINSNMOVE("mfc1 $a0, $f17", 0, f17, a0);
   TESTINSNMOVE("mfc1 $a1, $f18", 4, f18, a1);
   TESTINSNMOVE("mfc1 $a2, $f19", 8, f19, a2);
   TESTINSNMOVE("mfc1 $a3, $f20", 12, f20, a3);
   TESTINSNMOVE("mfc1 $v0, $f21", 16, f21, v0);
   TESTINSNMOVE("mfc1 $v1, $f22", 20, f22, v1);
   TESTINSNMOVE("mfc1 $t8, $f23", 24, f23, t8);
   TESTINSNMOVE("mfc1 $t9, $f24", 28, f24, t9);
   TESTINSNMOVE("mfc1 $t1, $f25", 32, f25, t1);
   TESTINSNMOVE("mfc1 $t2, $f26", 36, f26, t2);

   printf("MTC1\n");
   TESTINSNMOVEt("mtc1 $t1, $f0",  0, f0, t1);
   TESTINSNMOVEt("mtc1 $t2, $f1", 4, f1, t2);
   TESTINSNMOVEt("mtc1 $t3, $f2",  8, f2, t3);
   TESTINSNMOVEt("mtc1 $t4, $f3", 12, f3, t4);
   TESTINSNMOVEt("mtc1 $t5, $f4", 16, f4, t5);
   TESTINSNMOVEt("mtc1 $t6, $f5", 20, f5, t6);
   TESTINSNMOVEt("mtc1 $t7, $f6", 24, f6, t7);
   TESTINSNMOVEt("mtc1 $v0, $f7", 28, f7, v0);
   TESTINSNMOVEt("mtc1 $v1, $f8", 32, f8, v1);
   TESTINSNMOVEt("mtc1 $s0, $f9", 36, f9, s0);
   TESTINSNMOVEt("mtc1 $s1, $f10", 40, f10, s1);
   TESTINSNMOVEt("mtc1 $s2, $f11", 44, f11, s2);
   TESTINSNMOVEt("mtc1 $s3, $f12", 48, f12, s3);
   TESTINSNMOVEt("mtc1 $s4, $f13", 52, f13, s4);
   TESTINSNMOVEt("mtc1 $s5, $f14", 56, f14, s5);
   TESTINSNMOVEt("mtc1 $s6, $f15", 60, f15, s6);
   TESTINSNMOVEt("mtc1 $s7, $f16", 64, f16, s7);
   TESTINSNMOVEt("mtc1 $a0, $f17", 2, f17, a0);
   TESTINSNMOVEt("mtc1 $a1, $f18", 6, f18, a1);
   TESTINSNMOVEt("mtc1 $a2, $f19", 10, f19, a2);
   TESTINSNMOVEt("mtc1 $a3, $f20", 14, f20, a3);
   TESTINSNMOVEt("mtc1 $v0, $f21", 18, f21, v0);
   TESTINSNMOVEt("mtc1 $v1, $f22", 22, f22, v1);
   TESTINSNMOVEt("mtc1 $t8, $f23", 26, f23, t8);
   TESTINSNMOVEt("mtc1 $t9, $f24", 30, f24, t9);
   TESTINSNMOVEt("mtc1 $t1, $f25", 34, f25, t1);
   TESTINSNMOVEt("mtc1 $t2, $f26", 38, f26, t2);

   printf("MOV.S\n");
   TESTINSNMOVE1s("mov.s $f0, $f0",  0, f0, f0);
   TESTINSNMOVE1s("mov.s $f0, $f1", 4, f0, f1);
   TESTINSNMOVE1s("mov.s $f1, $f2",  8, f1, f2);
   TESTINSNMOVE1s("mov.s $f2, $f3", 12, f2, f3);
   TESTINSNMOVE1s("mov.s $f3, $f4", 16, f3, f4);
   TESTINSNMOVE1s("mov.s $f4, $f5", 20, f4, f5);
   TESTINSNMOVE1s("mov.s $f5, $f6", 24, f5, f6);
   TESTINSNMOVE1s("mov.s $f6, $f7", 28, f6, f7);
   TESTINSNMOVE1s("mov.s $f7, $f8", 32, f7, f8);
   TESTINSNMOVE1s("mov.s $f8, $f9", 36, f8, f9);
   TESTINSNMOVE1s("mov.s $f9, $f10", 40, f9, f10);
   TESTINSNMOVE1s("mov.s $f10, $f11", 44, f10, f11);
   TESTINSNMOVE1s("mov.s $f11, $f12", 48, f11, f12);
   TESTINSNMOVE1s("mov.s $f12, $f13", 52, f12, f13);
   TESTINSNMOVE1s("mov.s $f13, $f14", 56, f13, f14);
   TESTINSNMOVE1s("mov.s $f14, $f15", 60, f14, f15);
   TESTINSNMOVE1s("mov.s $f15, $f16", 64, f15, f16);
   TESTINSNMOVE1s("mov.s $f16, $f17", 0, f16, f17);
   TESTINSNMOVE1s("mov.s $f17, $f18", 4, f17, f18);
   TESTINSNMOVE1s("mov.s $f18, $f19", 8, f18, f19);
   TESTINSNMOVE1s("mov.s $f19, $f20", 12, f19, f20);
   TESTINSNMOVE1s("mov.s $f20, $f21", 16, f20, f21);
   TESTINSNMOVE1s("mov.s $f21, $f22", 20, f21, f22);
   TESTINSNMOVE1s("mov.s $f22, $f23", 24, f22, f23);
   TESTINSNMOVE1s("mov.s $f23, $f24", 28, f23, f24);
   TESTINSNMOVE1s("mov.s $f24, $f25", 32, f24, f25);
   TESTINSNMOVE1s("mov.s $f25, $f26", 36, f25, f26);

   printf("MOV.D\n");
   TESTINSNMOVE1d("mov.d $f0, $f0",  0, f0, f0);
   TESTINSNMOVE1d("mov.d $f0, $f0", 8, f0, f0);
   TESTINSNMOVE1d("mov.d $f0, $f2",  16, f0, f2);
   TESTINSNMOVE1d("mov.d $f2, $f4", 24, f2, f4);
   TESTINSNMOVE1d("mov.d $f2, $f4", 32, f2, f4);
   TESTINSNMOVE1d("mov.d $f4, $f6", 40, f4, f6);
   TESTINSNMOVE1d("mov.d $f4, $f6", 48, f4, f6);
   TESTINSNMOVE1d("mov.d $f6, $f8", 56, f6, f8);
   TESTINSNMOVE1d("mov.d $f6, $f8", 64, f6, f8);
   TESTINSNMOVE1d("mov.d $f8, $f10", 0, f8, f10);
   TESTINSNMOVE1d("mov.d $f8, $f10", 8, f8, f10);
   TESTINSNMOVE1d("mov.d $f10, $f12", 16, f10, f12);
   TESTINSNMOVE1d("mov.d $f10, $f12", 24, f10, f12);
   TESTINSNMOVE1d("mov.d $f12, $f14", 32, f12, f14);
   TESTINSNMOVE1d("mov.d $f12, $f14", 40, f12, f14);
   TESTINSNMOVE1d("mov.d $f14, $f16", 48, f14, f16);
   TESTINSNMOVE1d("mov.d $f14, $f16", 56, f14, f16);
   TESTINSNMOVE1d("mov.d $f16, $f18", 64, f16, f18);
   TESTINSNMOVE1d("mov.d $f16, $f18", 0, f16, f18);
   TESTINSNMOVE1d("mov.d $f18, $f20", 8, f18, f20);
   TESTINSNMOVE1d("mov.d $f18, $f20", 16, f18, f20);
   TESTINSNMOVE1d("mov.d $f20, $f22", 24, f20, f22);
   TESTINSNMOVE1d("mov.d $f20, $f22", 32, f20, f22);
   TESTINSNMOVE1d("mov.d $f22, $f24", 40, f22, f24);
   TESTINSNMOVE1d("mov.d $f22, $f24", 48, f22, f24);
   TESTINSNMOVE1d("mov.d $f24, $f26", 56, f24, f26);
   TESTINSNMOVE1d("mov.d $f24, $f26", 64, f24, f26);

   printf("MOVF\n");
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0xffffffff, 0xffffffff, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  555, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0, 5, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0, -1, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0xffffffff, 25, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0xffffffff, 0, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc0",  0xffffffff, 66, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0xffffffff, 0xffffffff, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  555, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0, 5, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0, -1, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0xffffffff, 25, t0, t1, 0);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0xffffffff, 0, t0, t1, 1);
   TESTINSNMOVE2("movf $t0, $t1, $fcc4",  0xffffffff, 66, t0, t1, 0);

   printf("MOVF.S\n");
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 0);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 4);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 8);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 12);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 16);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 20);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 24);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 28);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 32);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 36)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 40)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 44)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 48)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 52)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 1, 56)
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 4);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 12);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 16);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 20);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 24);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 28);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 32);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 36);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 40);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 44);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 52);
   TESTINSNMOVE2s("movf.s $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("MOVF.D\n");
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 0);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 8);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 16);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 24);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 32);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 40);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 48);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 56);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 64);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 0)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 8)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 16)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 24)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 32)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 1, 40)
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 56);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 64);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 16);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 24);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 32);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 40);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 56);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 64);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2d("movf.d $f4, $f6, $fcc0", f4, f6, 0, 16);

   printf("MOVN.S\n");
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 0, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 4, 1, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 8, 0xffff, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 12, -1, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 16, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 20, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 24, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 28, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 32, 125487, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 36, 68, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 40, -122544, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 44, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 48, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 52, 0xffffffff, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 56, 0x80000000, f0, f2, t3);
   TESTINSNMOVEN1s("movn.s $f0, $f2, $t3", 60, 0x7fffffff, f0, f2, t3);

   printf("MOVN.D\n");
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 0, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 4, 1, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 8, 0xffff, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 12, -1, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 16, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 20, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 24, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 28, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 32, 125487, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 36, 68, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 40, -122544, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 44, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 48, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 52, 0xffffffff, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 56, 0x80000000, f0, f2, t3);
   TESTINSNMOVEN1s("movn.d $f0, $f2, $t3", 60, 0x7fffffff, f0, f2, t3);

   printf("MOVT\n");
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0xffffffff, 0xffffffff, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  555, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0, 5, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0, -1, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0xffffffff, 25, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0xffffffff, 0, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc0",  0xffffffff, 66, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0xffffffff, 0xffffffff, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  555, 0xffffffff, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0, 5, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0, -1, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0xffffffff, 25, t0, t1, 0);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0xffffffff, 0, t0, t1, 1);
   TESTINSNMOVE2("movt $t0, $t1, $fcc4",  0xffffffff, 66, t0, t1, 0);

   printf("MOVT.S\n");
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 0);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 4);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 8);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 12);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 16);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 20);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 24);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 28);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 32);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 36)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 40)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 44)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 48)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 52)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 1, 56)
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 4);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 12);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 16);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 20);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 24);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 28);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 32);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 36);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 40);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 44);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 52);
   TESTINSNMOVE2s("movt.s $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("MOVT.D\n");
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 0);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 8);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 16);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 24);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 32);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 40);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 48);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 56);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 64);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 0)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 8)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 16)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 24)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 32)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 1, 40)
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 56);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 64);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 16);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 24);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 32);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 40);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 48);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 56);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 64);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 0);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 8);
   TESTINSNMOVE2d("movt.d $f4, $f6, $fcc0", f4, f6, 0, 16);

   printf("MOVZ.S\n");
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 0, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 4, 1, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 8, 0xffff, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 12, -1, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 16, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 20, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 24, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 24, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 28, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 32, 125487, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 36, 68, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 40, -122544, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 44, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 48, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 52, 0xffffffff, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 56, 0x80000000, f0, f2, t3);
   TESTINSNMOVEN1s("movz.s $f0, $f2, $t3", 60, 0x7fffffff, f0, f2, t3);

   printf("MOVZ.D\n");
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 0, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 4, 1, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 8, 0xffff, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 12, -1, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 16, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 20, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 24, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 28, 5, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 32, 125487, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 36, 68, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 40, -122544, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 44, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 48, 0, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 52, 0xffffffff, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 56, 0x80000000, f0, f2, t3);
   TESTINSNMOVEN1s("movz.d $f0, $f2, $t3", 60, 0x7fffffff, f0, f2, t3);
   return 0;
}
