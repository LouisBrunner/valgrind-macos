
/* How to compile:
   gcc -O -g -Wall -mcpu=cortex-a8 -o testarmv6int testarmv6int.c
*/

#include <stdio.h>

/* test macros to generate and output the result of a single instruction */
#define TESTINST2(instruction, RMval, RD, RM, carryin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
		"movs %3,%3;" \
		"msrne cpsr_fs,#(1<<29);" \
		"msreq cpsr_fs,#0;" \
		"mov " #RM ",%2;" \
                /* set #RD to 0x55555555 so we can see which parts get overwritten */ \
                "mov " #RD ", #0x55" "\n\t" \
                "orr " #RD "," #RD "," #RD ", LSL #8" "\n\t" \
                "orr " #RD "," #RD "," #RD ", LSL #16" "\n\t" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (carryin) \
		: #RD, #RM, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, \
		carryin ? 1 : 0, \
		cpsr & 0xff0f0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

#define TESTINST3(instruction, RMval, RNval, RD, RM, RN, carryin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
		"movs %4,%4;" \
		"msrne cpsr_fs,#(1<<29);" \
		"msreq cpsr_fs,#0;" \
		"mov " #RM ",%2;" \
		"mov " #RN ",%3;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (RNval), "r" (carryin) \
		: #RD, #RM, #RN, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, RNval, \
		carryin ? 1 : 0, \
		cpsr & 0xff0f0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

#define TESTINST4(instruction, RMval, RNval, RSval, RD, RM, RN, RS, carryin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
		"movs %5,%5;" \
		"msrne cpsr_fs,#(1<<29);" \
		"msreq cpsr_fs,#0;" \
		"mov " #RM ",%2;" \
		"mov " #RN ",%3;" \
		"mov " #RS ",%4;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (RNval), "r" (RSval), "r" (carryin) \
		: #RD, #RM, #RN, #RS, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x rs 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, RNval, RSval, \
		carryin ? 1 : 0, \
		cpsr & 0xff0f0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

#define TESTINST4_2OUT(instruction, RDval, RD2val, RMval, RSval, RD, RD2, RM, RS, carryin) \
{ \
	unsigned int out; \
	unsigned int out2; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
		"movs %7,%7;" \
		"msrne cpsr_fs,#(1<<29);" \
		"msreq cpsr_fs,#0;" \
		"mov " #RD ",%3;" \
		"mov " #RD2 ",%4;" \
		"mov " #RM ",%5;" \
		"mov " #RS ",%6;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mov %1," #RD2 ";" \
		"mrs %2,cpsr;" \
		: "=&r" (out), "=&r" (out2), "=&r" (cpsr) \
		: "r" (RDval), "r" (RD2val), "r" (RMval), "r" (RSval), "r" (carryin) \
		: #RD, #RD2, #RM, #RS, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rd2 0x%08x, rm 0x%08x rs 0x%08x, carryin %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, out2, RMval, RSval, \
		carryin ? 1 : 0, \
		cpsr & 0xff0f0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

/* helpers */
#define TESTCARRY { int c = 0; for (c = 0; c < 2; c++) {
#define TESTCARRYEND }}




int main(int argc, char **argv)
{

	printf("MOV\n");
	TESTINST2("mov  r0, r1", 1, r0, r1, 0);
	TESTINST2("cpy  r0, r1", 1, r0, r1, 0);
	TESTINST2("mov  r0, #0", 0, r0, r1, 0);
	TESTINST2("mov  r0, #1", 0, r0, r1, 0);
	TESTCARRY
	TESTINST2("movs r0, r1", 1, r0, r1, c);
	TESTINST2("movs r0, r1", 0, r0, r1, c);
	TESTINST2("movs r0, r1", 0x80000000, r0, r1, c);
	TESTINST2("movs r0, #0", 0, r0, r1, c);
	TESTINST2("movs r0, #1", 0, r0, r1, c);
	TESTCARRYEND

	printf("MVN\n");
	TESTINST2("mvn  r0, r1", 1, r0, r1, 0);
	TESTCARRY
	TESTINST2("mvns r0, r1", 1, r0, r1, c);
	TESTINST2("mvns r0, r1", 0, r0, r1, c);
	TESTINST2("mvns r0, r1", 0x80000000, r0, r1, c);
	TESTCARRYEND

	printf("ADD\n");
	TESTINST3("adds r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 0, 1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 1, 0, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 1, 1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 0, -1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 1, -1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 0x7fffffff, 1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 0x80000000, -1, r0, r1, r2, 0);
	TESTINST3("adds r0, r1, r2", 0x80000000, 0, r0, r1, r2, 0);

	printf("ADC\n");
	TESTINST3("adcs r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("adcs r0, r1, r2", 0, 0, r0, r1, r2, 1);

	printf("LSL\n");
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 1, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 2, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 31, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 32, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 33, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 63, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 64, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 255, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0xffffffff, 256, r0, r1, r2, 0);

	TESTINST3("lsl  r0, r1, r2", 0x1, 0, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0x1, 1, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0x1, 31, r0, r1, r2, 0);
	TESTINST3("lsl  r0, r1, r2", 0x2, 31, r0, r1, r2, 0);

	printf("LSLS\n");
	TESTCARRY
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 0, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 1, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 2, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 31, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 32, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 33, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 63, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 64, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 255, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 256, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0x1, 0, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0x1, 1, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0x1, 31, r0, r1, r2, c);
	TESTINST3("lsls r0, r1, r2", 0x2, 31, r0, r1, r2, c);
	TESTCARRYEND

	printf("LSL immediate\n");
	TESTCARRY
	TESTINST2("lsl  r0, r1, #0", 0xffffffff, r0, r1, c);
	TESTINST2("lsl  r0, r1, #1", 0xffffffff, r0, r1, c);
	TESTINST2("lsl  r0, r1, #31", 0xffffffff, r0, r1, c);
	TESTINST2("lsl  r0, r1, #0", 0x1, r0, r1, c);
	TESTINST2("lsl  r0, r1, #1", 0x1, r0, r1, c);
	TESTINST2("lsl  r0, r1, #31", 0x1, r0, r1, c);
	TESTINST2("lsl  r0, r1, #31", 0x2, r0, r1, c);
	TESTCARRYEND

	printf("LSLS immediate\n");
	TESTCARRY
	TESTINST2("lsls r0, r1, #0", 0xffffffff, r0, r1, c);
	TESTINST2("lsls r0, r1, #1", 0xffffffff, r0, r1, c);
	TESTINST2("lsls r0, r1, #31", 0xffffffff, r0, r1, c);
	TESTINST2("lsls r0, r1, #0", 0x1, r0, r1, c);
	TESTINST2("lsls r0, r1, #1", 0x1, r0, r1, c);
	TESTINST2("lsls r0, r1, #31", 0x1, r0, r1, c);
	TESTINST2("lsls r0, r1, #31", 0x2, r0, r1, c);
	TESTCARRYEND

	printf("LSR\n");
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 1, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 2, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 31, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 32, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 33, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 63, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 64, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 255, r0, r1, r2, 0);
	TESTINST3("lsr  r0, r1, r2", 0xffffffff, 256, r0, r1, r2, 0);

	printf("LSRS\n");
	TESTCARRY
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 0, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 1, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 2, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 31, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 32, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 33, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 63, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 64, r0, r1, r2, c);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 255, r0, r1, r2, c);
	TESTCARRYEND

	printf("LSR immediate\n");
	TESTINST2("lsr  r0, r1, #0", 0xffffffff, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #1", 0xffffffff, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #31", 0xffffffff, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #32", 0xffffffff, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #16", 0x00010000, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #17", 0x00010000, r0, r1, 0);
	TESTINST2("lsr  r0, r1, #18", 0x00010000, r0, r1, 0);

	printf("LSRS immediate\n");
	TESTCARRY
	TESTINST2("lsrs r0, r1, #0", 0xffffffff, r0, r1, c);
	TESTINST2("lsrs r0, r1, #1", 0xffffffff, r0, r1, c);
	TESTINST2("lsrs r0, r1, #31", 0xffffffff, r0, r1, c);
	TESTINST2("lsrs r0, r1, #32", 0xffffffff, r0, r1, c);
	TESTINST2("lsrs r0, r1, #16", 0x00010000, r0, r1, c);
	TESTINST2("lsrs r0, r1, #17", 0x00010000, r0, r1, c);
	TESTINST2("lsrs r0, r1, #18", 0x00010000, r0, r1, c);
	TESTCARRYEND

	printf("ASR\n");
	TESTCARRY
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 1, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 2, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 31, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 32, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 33, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 63, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 64, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 255, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 256, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 0, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 1, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 2, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 31, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 32, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 33, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 63, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 64, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 255, r0, r1, r2, c);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 256, r0, r1, r2, c);
	TESTCARRYEND

	printf("ASRS\n");
	TESTCARRY
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 0, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 1, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 2, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 31, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 32, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 33, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 63, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 64, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 255, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 256, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 0, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 1, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 2, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 31, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 32, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 33, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 63, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 64, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 255, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 256, r0, r1, r2, c);
	TESTCARRYEND

	TESTCARRY
	TESTINST3("asrs r0, r1, r2", 0x8, 0, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x8, 1, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x8, 2, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x8, 3, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x8, 4, r0, r1, r2, c);
	TESTINST3("asrs r0, r1, r2", 0x8, 5, r0, r1, r2, c);
	TESTCARRYEND

	TESTINST3("asrs r0, r1, r2", 0x80000001, 1, r0, r1, r2, 0);
	TESTINST3("asrs r0, r1, r2", 0x80000001, 2, r0, r1, r2, 0);

	printf("ASR immediate\n");
	TESTINST2("asr  r0, r1, #0", 0xffffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #1", 0xffffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #31", 0xffffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #32", 0xffffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #0", 0x7fffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #1", 0x7fffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #31", 0x7fffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #32", 0x7fffffff, r0, r1, 0);
	TESTINST2("asr  r0, r1, #16", 0x00010000, r0, r1, 0);
	TESTINST2("asr  r0, r1, #17", 0x00010000, r0, r1, 0);
	TESTINST2("asr  r0, r1, #18", 0x00010000, r0, r1, 0);

	printf("ASRS immediate\n");
	TESTCARRY
	TESTINST2("asrs r0, r1, #0", 0xffffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #1", 0xffffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #31", 0xffffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #32", 0xffffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #0", 0x7fffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #1", 0x7fffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #31", 0x7fffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #32", 0x7fffffff, r0, r1, c);
	TESTINST2("asrs r0, r1, #16", 0x00010000, r0, r1, c);
	TESTINST2("asrs r0, r1, #17", 0x00010000, r0, r1, c);
	TESTINST2("asrs r0, r1, #18", 0x00010000, r0, r1, c);
	TESTCARRYEND

	printf("ROR\n");
	TESTCARRY
	TESTINST3("ror  r0, r1, r2", 0x00088000, 0, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x80088000, 1, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 1, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 2, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 31, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 32, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 33, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 63, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 64, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 255, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 256, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x80088000, 256, r0, r1, r2, c);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 257, r0, r1, r2, c);
	TESTCARRYEND

	printf("RORS\n");
	TESTCARRY
	TESTINST3("rors r0, r1, r2", 0x00088000, 0, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x80088000, 0, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 1, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 2, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 31, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 32, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 33, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 63, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 64, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 255, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 256, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x80088000, 256, r0, r1, r2, c);
	TESTINST3("rors r0, r1, r2", 0x00088000, 257, r0, r1, r2, c);
	TESTCARRYEND

	printf("ROR immediate\n");
	TESTCARRY
	TESTINST2("ror  r0, r1, #0", 0x00088000, r0, r1, c);
	TESTINST2("ror  r0, r1, #1", 0x00088000, r0, r1, c);
	TESTINST2("ror  r0, r1, #31", 0x00088000, r0, r1, c);
	TESTINST2("ror  r0, r1, #16", 0x00010000, r0, r1, c);
	TESTINST2("ror  r0, r1, #17", 0x00010000, r0, r1, c);
	TESTINST2("ror  r0, r1, #18", 0x00010000, r0, r1, c);
	TESTCARRYEND

	printf("RORS immediate\n");
	TESTCARRY
	TESTINST2("rors r0, r1, #0", 0x00088000, r0, r1, c);
	TESTINST2("rors r0, r1, #1", 0x00088000, r0, r1, c);
	TESTINST2("rors r0, r1, #31", 0x00088000, r0, r1, c);
	TESTINST2("rors r0, r1, #16", 0x00010000, r0, r1, c);
	TESTINST2("rors r0, r1, #17", 0x00010000, r0, r1, c);
	TESTINST2("rors r0, r1, #18", 0x00010000, r0, r1, c);
	TESTCARRYEND

	printf("shift with barrel shifter\n");
	TESTCARRY
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 31, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 32, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 255, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 256, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 31, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 32, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 255, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 31, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 32, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 255, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 256, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, c);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 1, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 2, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 3, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 4, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 5, r0, r1, r2, r3, c);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x80088000, 0, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 1, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 31, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 32, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 255, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 256, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x80088000, 256, r0, r1, r2, r3, c);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 257, r0, r1, r2, r3, c);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 31, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 32, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 255, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 256, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 31, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 32, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 255, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, c);

	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 0, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x80088000, 0, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 1, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 31, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 32, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 255, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 256, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x80088000, 256, r0, r1, r2, r3, c);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 257, r0, r1, r2, r3, c);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("adcs r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("adcs r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, c);
	TESTINST4("adcs r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTINST4("adcs r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, c);
	TESTCARRYEND

	printf("MUL\n");
	TESTINST3("mul  r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

	printf("MULS\n");
	TESTINST3("muls r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

	printf("MLA\n");
	TESTINST4("mla  r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);

	printf("MLAS\n");
	TESTINST4("mlas r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);

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
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);

	printf("SMULL\n");
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);

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
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);

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
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);

	printf("SMLALD\n");
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
        TESTINST4_2OUT("smlald  r0, r1, r2, r3", 0xfff4ffff, 0xff8fff3f, 0xfffff6ff, 0xfff9ffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
       TESTINST4_2OUT("smlaldx r0, r1, r2, r3", 0xfff4ffff, 0xff8fff3f, 0xfffff6ff, 0xfff9ffff, r0, r1, r2, r3, 0);

	printf("SMLSLD\n");
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
       TESTINST4_2OUT("smlsld  r0, r1, r2, r3", 0xfff4ffff, 0xff8fff3f, 0xfffff6ff, 0xfff9ffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
       TESTINST4_2OUT("smlsldx r0, r1, r2, r3", 0xfff4ffff, 0xff8fff3f, 0xfffff6ff, 0xfff9ffff, r0, r1, r2, r3, 0);

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

	printf("------------ SMULL{B,T}{B,T} ------------\n");
        /* SMULxx rD, rN, rM */

	TESTINST3("smulbb r0, r1, r2", 0x00030000, 0x00040000,  r0, r1, r2, 0);
	TESTINST3("smulbb r0, r1, r2", 0x00030001, 0x00040002,  r0, r1, r2, 0);
	TESTINST3("smulbb r0, r1, r2", 0x00038001, 0x00047fff,  r0, r1, r2, 0);
	TESTINST3("smulbb r0, r1, r2", 0x00037fff, 0x00047fff,  r0, r1, r2, 0);
	TESTINST3("smulbb r0, r1, r2", 0x0003ffff, 0x0004ffff,  r0, r1, r2, 0);

	printf("------------ SXTAB ------------\n");
        TESTINST3("sxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #8", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #0", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("sxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #8", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab r0, r1, r2, ROR #0", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);

	printf("------------ UXTAB ------------\n");
        TESTINST3("uxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #8", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #0", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("uxtab r0, r1, r2, ROR #24", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #16", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #8", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab r0, r1, r2, ROR #0", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);

	printf("------------ SXTAH ------------\n");
        TESTINST3("sxtah r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("sxtah r0, r1, r2, ROR #24", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #16", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);

	printf("------------ UXTAH ------------\n");
        TESTINST3("uxtah r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("uxtah r0, r1, r2, ROR #24", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #16", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #8 ", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtah r0, r1, r2, ROR #0 ", 0x31415927, 0x27189819, 
                  r0, r1, r2, 0);

	printf("------------ PLD/PLDW (begin) ------------\n");
        /* These don't have any effect on the architected state, so,
           uh, there's no result values to check.  Just _do_ some of
           them and check Valgrind's instruction decoder eats them up
           without complaining. */
        { int alocal;
          printf("pld  reg +/- imm12  cases\n");
          __asm__ __volatile__( "pld [%0, #128]" : :/*in*/"r"(&alocal) );
          __asm__ __volatile__( "pld [%0, #-128]" : :/*in*/"r"(&alocal) );
          __asm__ __volatile__( "pld [r15, #-128]" : :/*in*/"r"(&alocal) );

          // apparently pldw is v7 only
          //__asm__ __volatile__( "pldw [%0, #128]" : :/*in*/"r"(&alocal) );
          //__asm__ __volatile__( "pldw [%0, #-128]" : :/*in*/"r"(&alocal) );
          //__asm__ __volatile__( "pldw [r15, #128]" : :/*in*/"r"(&alocal) );

          printf("pld  reg +/- shifted reg  cases\n");
          __asm__ __volatile__( "pld [%0, %1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, LSL #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, LSR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, ASR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, ROR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, RRX]" : : /*in*/"r"(&alocal), "r"(0) );
        }
	printf("------------ PLD/PLDW (done) ------------\n");

	printf("------------ RBIT ------------\n");
	TESTINST2("rbit r0, r1", 0x00000000, r0, r1, 0);
	TESTINST2("rbit r0, r1", 0xFFFFFFFF, r0, r1, 0);
	TESTINST2("rbit r0, r1", 0x80000000, r0, r1, 0);
	TESTINST2("rbit r0, r1", 0x00000001, r0, r1, 0);
	TESTINST2("rbit r0, r1", 0x31415927, r0, r1, 0);
	TESTINST2("rbit r0, r1", 0x14141562, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xabe8391f, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x9028aa80, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xead1fc6d, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x35c98c55, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x534af1eb, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x45511b08, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x90077f71, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xde8ca84b, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xe37a0dda, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xe5b83d4b, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0xbb6d14ec, r0, r1, 0);
   TESTINST2("rbit r0, r1", 0x68983cc9, r0, r1, 0);

	printf("------------ REV ------------\n");
	TESTINST2("rev r0, r1", 0x00000000, r0, r1, 0);
	TESTINST2("rev r0, r1", 0xFFFFFFFF, r0, r1, 0);
	TESTINST2("rev r0, r1", 0x80000000, r0, r1, 0);
	TESTINST2("rev r0, r1", 0x00000001, r0, r1, 0);
	TESTINST2("rev r0, r1", 0x31415927, r0, r1, 0);
	TESTINST2("rev r0, r1", 0x14141562, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xabe8391f, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x9028aa80, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xead1fc6d, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x35c98c55, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x534af1eb, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x45511b08, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x90077f71, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xde8ca84b, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xe37a0dda, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xe5b83d4b, r0, r1, 0);
   TESTINST2("rev r0, r1", 0xbb6d14ec, r0, r1, 0);
   TESTINST2("rev r0, r1", 0x68983cc9, r0, r1, 0);

	printf("------------ REV16 ------------\n");
	TESTINST2("rev16 r0, r1", 0x00000000, r0, r1, 0);
	TESTINST2("rev16 r0, r1", 0xFFFFFFFF, r0, r1, 0);
	TESTINST2("rev16 r0, r1", 0x80000000, r0, r1, 0);
	TESTINST2("rev16 r0, r1", 0x00000001, r0, r1, 0);
	TESTINST2("rev16 r0, r1", 0x31415927, r0, r1, 0);
	TESTINST2("rev16 r0, r1", 0x14141562, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xabe8391f, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x9028aa80, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xead1fc6d, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x35c98c55, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x534af1eb, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x45511b08, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x90077f71, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xde8ca84b, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xe37a0dda, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xe5b83d4b, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0xbb6d14ec, r0, r1, 0);
   TESTINST2("rev16 r0, r1", 0x68983cc9, r0, r1, 0);

        printf("------------ REVSH ------------\n");
        TESTINST2("revsh r0, r1", 0x00000000, r0, r1, 0);
        TESTINST2("revsh r0, r1", 0xFFFFFFFF, r0, r1, 0);
        TESTINST2("revsh r0, r1", 0x80000000, r0, r1, 0);
        TESTINST2("revsh r0, r1", 0x00000001, r0, r1, 0);
        TESTINST2("revsh r0, r1", 0x31415927, r0, r1, 0);
        TESTINST2("revsh r0, r1", 0x14141562, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xabe8391f, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x9028aa80, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xead1fc6d, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x35c98c55, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x534af1eb, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x45511b08, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x90077f71, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xde8ca84b, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xe37a0dda, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xe5b83d4b, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0xbb6d14ec, r0, r1, 0);
   TESTINST2("revsh r0, r1", 0x68983cc9, r0, r1, 0);

	printf("------------ NOP (begin) ------------\n");
        printf("nop\n");
        __asm__ __volatile__("nop" ::: "memory","cc");
        printf("nopeq\n");
        __asm__ __volatile__("nopeq" ::: "memory","cc");
        printf("nopne\n");
        __asm__ __volatile__("nopne" ::: "memory","cc");
	printf("------------ NOP (end) ------------\n");

	printf("------------ SMMUL ------------\n");
        TESTINST3("smmul   r0, r1, r2", 0, 0, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
        TESTINST3("smmul   r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);

        TESTINST3("smmulr  r0, r1, r2", 0, 0, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0xe444dc25, 0xd5eef620, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0x06ea9b2a, 0xa2108661, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0x448f3a5f, 0x17aecf57, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0x4b0c2337, 0xffa63d6c, r0, r1, r2, 0);
        TESTINST3("smmulr  r0, r1, r2", 0xf91d5f56, 0x088bc0f9, r0, r1, r2, 0);

	printf("------------ UMAAL ------------\n");
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);

	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0x27182846, 0x31415927, 0x14141356, 0x1773250A, 
                       r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umaal  r0, r1, r2, r3", 0x01415927, 0x74141356, 0x5773250A, 0xA7182846,
                       r0, r1, r2, r3, 1);

        printf("----------------- SMMLA{R} ----------------- \n");
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
        TESTINST4("smmla  r0, r1, r2, r3", 
                  0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
        TESTINST4("smmlar  r0, r1, r2, r3", 
                  0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

        printf("----------------- SMMLS{R} ----------------- \n");
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
        TESTINST4("smmls  r0, r1, r2, r3", 
                  0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x80008000, 0x80008000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00000000, 0x00000000, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x7fff7fff, 0x00010001, 0x00000001, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x80008000, 0xffffffff, 0x0000001f, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x00640064, 0x00030003, 0x00000020, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xffffffff, 0xfffc0001, 0x000000ff, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xfff70fff, 0x00030003, 0x00000100, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xb8035b5b, 0xce0ce1ed, 0x5f986e68, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x35232047, 0x146275d8, 0xaae3433f, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xe7aa57b4, 0x1584bd74, 0x2c07a5b4, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x32fa0095, 0x36f26261, 0x89d2ef86, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x8ed8287c, 0x02c90120, 0xd4b64d54, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xc53aaba9, 0x29300837, 0x0b02c58a, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x216158cb, 0x57a50a01, 0xb0d20777, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x3e2e1bd7, 0x3cd6cd94, 0x7e376198, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xd5fe2dc4, 0xdd914bf7, 0xd5dc5407, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xf87b961e, 0x1d66879f, 0xf2b64835, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xd65db979, 0xc61b323b, 0xae930a1a, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x5ef1f1a8, 0xbf73f0a5, 0x2fb714c9, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x1ffe53d9, 0x815bb75b, 0xa3268abe, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xed2cbf78, 0xc6ffabb6, 0xef9e9fd9, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xeaa652c7, 0x137741f4, 0x3dba1164, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x3ada0280, 0x71fbde8b, 0xdba5bd25, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xda4ba05b, 0x90f9833d, 0x884c0ad8, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xc00b821a, 0x7fa1d5a6, 0x9a4ff1b8, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xe1bb8606, 0x58293969, 0x81616d13, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x51f31d95, 0xa3cfd624, 0x6077fb1f, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x0849a0c2, 0x0872f25a, 0x40b094e2, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x17913309, 0xf1e03d7e, 0x91edc21d, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x5388b5cd, 0x86582032, 0x6034078d, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x181c436b, 0x5de41558, 0xccfa1c7e, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x23ba1b46, 0x4437983c, 0x48d06549, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xa9085781, 0xc6b4ac58, 0xb2aead21, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xc2bdf597, 0xdde1e6a4, 0x852e3a72, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x157b0dea, 0xf0d5ff94, 0xe7b87e39, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x3edad6b6, 0x82aceb7a, 0x0557c6fc, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x6cc9bfa8, 0x7f808c15, 0x81874a02, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x6b1422c7, 0x33921b00, 0x3ccad3f7, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xd7ce1909, 0x3e435701, 0x85fbf196, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xb4e16b6e, 0x6e13680a, 0x89436f88, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x44858efc, 0x9002bc30, 0x390d2c2f, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xbea121ab, 0x953ff6ec, 0x80657c40, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x6ffed89f, 0x3e8c49b7, 0x11bd07d1, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x7795635d, 0x5e6e32dd, 0xe4999bf2, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xec0c2f30, 0x5736ed46, 0x231348c0, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x4f9ddd1b, 0x95bca5d8, 0x5765b203, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xc1553709, 0x0112b30a, 0x69ec0212, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x74bd0223, 0x03fa9bb5, 0x899d9192, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xf52e9fbf, 0xb4c510a7, 0x7fcbe5a9, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x64a365ef, 0x2dd01366, 0xf7b0b13e, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x5e4b1cbf, 0x44de5ca9, 0x464a21cc, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x299da970, 0xe8108f1b, 0xf5818cfb, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xcd90d604, 0xaa5e9444, 0x8217b7df, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xe60743c3, 0x7acb4de3, 0x73c29060, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x868e7c7d, 0x5f77532e, 0x1d133d3d, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0x4e5e0760, 0x8f6d3264, 0x21ba2fb3, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xde99ac2f, 0x0be36f70, 0xeda5110c, r0, r1, r2, r3, 0);
        TESTINST4("smmlsr  r0, r1, r2, r3", 
                  0xc57243b7, 0xcf1e4487, 0xf20fb90f, r0, r1, r2, r3, 0);

	return 0;
}
