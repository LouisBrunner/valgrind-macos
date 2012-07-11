
/* How to compile:
   gcc -O -g -Wall -mcpu=cortex-a8 -o v6intThumb none/tests/arm/v6intThumb.c
*/

#include <stdio.h>

static int gen_cvin(cvin)
{
  int r = ((cvin & 2) ? (1<<29) : 0) | ((cvin & 1) ? (1<<28) : 0);
  r |= (1 << 31) | (1 << 30);
  return r;
}

/* test macros to generate and output the result of a single instruction */


// 1 registers in the insn, zero args: rD = op()
#define TESTINST1(instruction, RD, cvin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %2;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (gen_cvin(cvin))        \
		: #RD, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}



// 1 registers in the insn, one args: rD = op(rD)
#define TESTINST1x(instruction, RDval, RD, cvin)       \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %2;" \
                "mov " #RD ",%3;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (gen_cvin(cvin)), "r"(RDval) \
		: #RD, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

// 2 registers in the insn, one arg: rD = op(rM)
#define TESTINST2(instruction, RMval, RD, RM, cvin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %3;" \
		"mov " #RM ",%2;" \
                /* set #RD to 0x55555555 so we can see which parts get overwritten */ \
                "mov " #RD ", #0x55" "\n\t" \
                "orr " #RD "," #RD "," #RD ", LSL #8" "\n\t" \
                "orr " #RD "," #RD "," #RD ", LSL #16" "\n\t" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (gen_cvin(cvin))        \
		: #RD, #RM, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}


// 2 registers in the insn, two args: rD = op(rD, rM)
#define TESTINST2x(instruction, RDval, RMval, RD, RM, cvin)       \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %3;" \
		"mov " #RM ",%2;" \
                "mov " #RD ",%4;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (gen_cvin(cvin)), "r"(RDval) \
		: #RD, #RM, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}



#define TESTINST3(instruction, RMval, RNval, RD, RM, RN, cvin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %4;" \
		"mov " #RM ",%2;" \
		"mov " #RN ",%3;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (RNval), "r" (gen_cvin(cvin))    \
		: #RD, #RM, #RN, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, RNval, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

#define TESTINST4(instruction, RMval, RNval, RSval, RD, RM, RN, RS, cvin) \
{ \
	unsigned int out; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %5;" \
		"mov " #RM ",%2;" \
		"mov " #RN ",%3;" \
		"mov " #RS ",%4;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mrs %1,cpsr;" \
		: "=&r" (out), "=&r" (cpsr) \
		: "r" (RMval), "r" (RNval), "r" (RSval), "r" (gen_cvin(cvin)) \
		: #RD, #RM, #RN, #RS, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rm 0x%08x, rn 0x%08x rs 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, RMval, RNval, RSval, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

#define TESTINST4_2OUT(instruction, RDval, RD2val, RMval, RSval, RD, RD2, RM, RS, cvin) \
{ \
	unsigned int out; \
	unsigned int out2; \
	unsigned int cpsr; \
\
	__asm__ volatile( \
                "msr cpsr_f, %7;" \
		"mov " #RD ",%3;" \
		"mov " #RD2 ",%4;" \
		"mov " #RM ",%5;" \
		"mov " #RS ",%6;" \
		instruction ";" \
		"mov %0," #RD ";" \
		"mov %1," #RD2 ";" \
		"mrs %2,cpsr;" \
		: "=&r" (out), "=&r" (out2), "=&r" (cpsr) \
		: "r" (RDval), "r" (RD2val), "r" (RMval), "r" (RSval), "r" (gen_cvin(cvin)) \
		: #RD, #RD2, #RM, #RS, "cc", "memory" \
	); \
	printf("%s :: rd 0x%08x rd2 0x%08x, rm 0x%08x rs 0x%08x, c:v-in %d, cpsr 0x%08x %c%c%c%c\n", \
		instruction, out, out2, RMval, RSval, \
		cvin, \
		cpsr & 0xffff0000, \
		((1<<31) & cpsr) ? 'N' : ' ', \
		((1<<30) & cpsr) ? 'Z' : ' ', \
		((1<<29) & cpsr) ? 'C' : ' ', \
		((1<<28) & cpsr) ? 'V' : ' ' \
		); \
}

/* helpers */
#define NOCARRY { int cv = 0; {
#define TESTCARRY { int cv = 0; for (cv = 0; cv < 4; cv++) {
#define TESTCARRYEND }}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

static int old_main(void)
{

	printf("MOV\n");
	TESTINST2("mov  r0, r1", 1, r0, r1, 0);
	TESTINST2("cpy  r0, r1", 1, r0, r1, 0);
	TESTINST2("mov  r0, #0", 0, r0, r1, 0);
	TESTINST2("mov  r0, #1", 0, r0, r1, 0);
	TESTCARRY
	TESTINST2("movs r0, r1", 1, r0, r1, cv);
	TESTINST2("movs r0, r1", 0, r0, r1, cv);
	TESTINST2("movs r0, r1", 0x80000000, r0, r1, cv);
	TESTINST2("movs r0, #0", 0, r0, r1, cv);
	TESTINST2("movs r0, #1", 0, r0, r1, cv);
	TESTCARRYEND

	printf("MVN\n");
	TESTINST2("mvn  r0, r1", 1, r0, r1, 0);
	TESTCARRY
	TESTINST2("mvns r0, r1", 1, r0, r1, cv);
	TESTINST2("mvns r0, r1", 0, r0, r1, cv);
	TESTINST2("mvns r0, r1", 0x80000000, r0, r1, cv);
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
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 0, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 1, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 2, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 31, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 32, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 33, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 63, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 64, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 255, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0xffffffff, 256, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0x1, 0, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0x1, 1, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0x1, 31, r0, r1, r2, cv);
	TESTINST3("lsls r0, r1, r2", 0x2, 31, r0, r1, r2, cv);
	TESTCARRYEND

	printf("LSL immediate\n");
	TESTCARRY
	TESTINST2("lsl  r0, r1, #0", 0xffffffff, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #1", 0xffffffff, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #31", 0xffffffff, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #0", 0x1, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #1", 0x1, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #31", 0x1, r0, r1, cv);
	TESTINST2("lsl  r0, r1, #31", 0x2, r0, r1, cv);
	TESTCARRYEND

	printf("LSLS immediate\n");
	TESTCARRY
	TESTINST2("lsls r0, r1, #0", 0xffffffff, r0, r1, cv);
	TESTINST2("lsls r0, r1, #1", 0xffffffff, r0, r1, cv);
	TESTINST2("lsls r0, r1, #31", 0xffffffff, r0, r1, cv);
	TESTINST2("lsls r0, r1, #0", 0x1, r0, r1, cv);
	TESTINST2("lsls r0, r1, #1", 0x1, r0, r1, cv);
	TESTINST2("lsls r0, r1, #31", 0x1, r0, r1, cv);
	TESTINST2("lsls r0, r1, #31", 0x2, r0, r1, cv);
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
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 0, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 1, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 2, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 31, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 32, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 33, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 63, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 64, r0, r1, r2, cv);
	TESTINST3("lsrs r0, r1, r2", 0xffffffff, 255, r0, r1, r2, cv);
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
	TESTINST2("lsrs r0, r1, #0", 0xffffffff, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #1", 0xffffffff, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #31", 0xffffffff, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #32", 0xffffffff, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #16", 0x00010000, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #17", 0x00010000, r0, r1, cv);
	TESTINST2("lsrs r0, r1, #18", 0x00010000, r0, r1, cv);
	TESTCARRYEND

	printf("ASR\n");
	TESTCARRY
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 1, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 2, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 31, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 32, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 33, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 63, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 64, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 255, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0xffffffff, 256, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 0, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 1, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 2, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 31, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 32, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 33, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 63, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 64, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 255, r0, r1, r2, cv);
	TESTINST3("asr  r0, r1, r2", 0x7fffffff, 256, r0, r1, r2, cv);
	TESTCARRYEND

	printf("ASRS\n");
	TESTCARRY
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 0, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 1, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 2, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 31, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 32, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 33, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 63, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 64, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 255, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0xffffffff, 256, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 0, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 1, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 2, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 31, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 32, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 33, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 63, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 64, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 255, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x7fffffff, 256, r0, r1, r2, cv);
	TESTCARRYEND

	TESTCARRY
	TESTINST3("asrs r0, r1, r2", 0x8, 0, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x8, 1, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x8, 2, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x8, 3, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x8, 4, r0, r1, r2, cv);
	TESTINST3("asrs r0, r1, r2", 0x8, 5, r0, r1, r2, cv);
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
	TESTINST2("asrs r0, r1, #0", 0xffffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #1", 0xffffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #31", 0xffffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #32", 0xffffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #0", 0x7fffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #1", 0x7fffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #31", 0x7fffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #32", 0x7fffffff, r0, r1, cv);
	TESTINST2("asrs r0, r1, #16", 0x00010000, r0, r1, cv);
	TESTINST2("asrs r0, r1, #17", 0x00010000, r0, r1, cv);
	TESTINST2("asrs r0, r1, #18", 0x00010000, r0, r1, cv);
	TESTCARRYEND

#if 0
	printf("ROR\n");
	TESTCARRY
	TESTINST3("ror  r0, r1, r2", 0x00088000, 0, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x80088000, 1, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 1, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 2, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 31, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 32, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 33, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 63, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 64, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 255, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 256, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x80088000, 256, r0, r1, r2, cv);
	TESTINST3("ror  r0, r1, r2", 0x00088000, 257, r0, r1, r2, cv);
	TESTCARRYEND

	printf("RORS\n");
	TESTCARRY
	TESTINST3("rors r0, r1, r2", 0x00088000, 0, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x80088000, 0, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 1, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 2, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 31, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 32, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 33, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 63, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 64, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 255, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 256, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x80088000, 256, r0, r1, r2, cv);
	TESTINST3("rors r0, r1, r2", 0x00088000, 257, r0, r1, r2, cv);
	TESTCARRYEND

	printf("ROR immediate\n");
	TESTCARRY
	TESTINST2("ror  r0, r1, #0", 0x00088000, r0, r1, cv);
	TESTINST2("ror  r0, r1, #1", 0x00088000, r0, r1, cv);
	TESTINST2("ror  r0, r1, #31", 0x00088000, r0, r1, cv);
	TESTINST2("ror  r0, r1, #16", 0x00010000, r0, r1, cv);
	TESTINST2("ror  r0, r1, #17", 0x00010000, r0, r1, cv);
	TESTINST2("ror  r0, r1, #18", 0x00010000, r0, r1, cv);
	TESTCARRYEND

	printf("RORS immediate\n");
	TESTCARRY
	TESTINST2("rors r0, r1, #0", 0x00088000, r0, r1, cv);
	TESTINST2("rors r0, r1, #1", 0x00088000, r0, r1, cv);
	TESTINST2("rors r0, r1, #31", 0x00088000, r0, r1, cv);
	TESTINST2("rors r0, r1, #16", 0x00010000, r0, r1, cv);
	TESTINST2("rors r0, r1, #17", 0x00010000, r0, r1, cv);
	TESTINST2("rors r0, r1, #18", 0x00010000, r0, r1, cv);
	TESTCARRYEND
#endif
#if 0
	printf("shift with barrel shifter\n");
	TESTCARRY
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 31, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 32, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 255, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsl r3", 0, 0xffffffff, 256, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 31, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 32, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 255, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 31, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 32, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 255, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x7fffffff, 256, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, cv);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 1, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 2, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 3, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 4, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, asr r3", 0, 0x8, 5, r0, r1, r2, r3, cv);
	TESTCARRYEND

	TESTCARRY
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x80088000, 0, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 1, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 31, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 32, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 255, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 256, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x80088000, 256, r0, r1, r2, r3, cv);
	TESTINST4("add  r0, r1, r2, ror r3", 0, 0x00088000, 257, r0, r1, r2, r3, cv);
	TESTCARRYEND
#endif
#if 0
	TESTCARRY
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 31, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 32, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 255, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsl r3", 0, 0xffffffff, 256, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 31, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 32, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 255, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, lsr r3", 0, 0xffffffff, 256, r0, r1, r2, r3, cv);

	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 0, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x80088000, 0, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 1, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 31, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 32, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 255, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 256, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x80088000, 256, r0, r1, r2, r3, cv);
	TESTINST4("adds r0, r1, r2, ror r3", 0, 0x00088000, 257, r0, r1, r2, r3, cv);
	TESTCARRYEND
#endif

#if 0
	TESTCARRY
	TESTINST4("adcs r0, r1, r2, lsl r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("adcs r0, r1, r2, lsr r3", 0, 0xffffffff, 0, r0, r1, r2, r3, cv);
	TESTINST4("adcs r0, r1, r2, lsl r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTINST4("adcs r0, r1, r2, lsr r3", 0, 0xffffffff, 1, r0, r1, r2, r3, cv);
	TESTCARRYEND
#endif

	printf("MUL\n");
	TESTINST3("mul  r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
	TESTINST3("mul  r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);

#if 0
	printf("MULS\n");
	TESTINST3("muls r0, r1, r2", 0, 0, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0xffffffff, 0, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0xffffffff, 0xffffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0x7fffffff, 0x7fffffff, r0, r1, r2, 0);
	TESTINST3("muls r0, r1, r2", 0x0000ffff, 0x0000ffff, r0, r1, r2, 0);
#endif

	printf("MLA\n");
	TESTINST4("mla  r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mla  r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);

#if 0
	printf("MLAS\n");
	TESTINST4("mlas r0, r1, r2, r3", 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0xffffffff, 0xffffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0x7fffffff, 0x7fffffff, 1, r0, r1, r2, r3, 0);
	TESTINST4("mlas r0, r1, r2, r3", 0x0000ffff, 0x0000ffff, 1, r0, r1, r2, r3, 0);
#endif

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
#if 0
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif
	printf("SMULL\n");
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smull  r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#if 0
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smulls r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif

#if 0
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
#endif
#if 0
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("umlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif
#if 0
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
#endif
#if 0
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 1, 1, 0, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0xffffffff, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 0, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 1, 1, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffff, 0xffff, r0, r1, r2, r3, 0);
	TESTINST4_2OUT("smlals r0, r1, r2, r3", 0, 0, 0xffffffff, 0xffffffff, r0, r1, r2, r3, 0);
#endif
	printf("CLZ\n");
	TESTCARRY
	TESTINST2("clz  r0, r1", 0, r0, r1, cv);
	TESTINST2("clz  r0, r1", 1, r0, r1, cv);
	TESTINST2("clz  r0, r1", 0x10, r0, r1, cv);
	TESTINST2("clz  r0, r1", 0xffffffff, r0, r1, cv);
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
#if 0
	TESTINST2("uxtb16 r0, r1", 0xffffffff, r0, r1, 0);
	TESTINST2("uxtb16 r0, r1, ror #16", 0x0000ffff, r0, r1, 0);
	TESTINST2("sxtb16 r0, r1", 0xffffffff, r0, r1, 0);
	TESTINST2("sxtb16 r0, r1", 0x00ff00ff, r0, r1, 0);
	TESTINST2("sxtb16 r0, r1", 0x007f007f, r0, r1, 0);
#endif
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
#if 0
	printf("------------ SXTAB16 ------------\n");
        TESTINST3("sxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #8", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #0", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("sxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #8", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("sxtab16 r0, r1, r2, ROR #0", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
#endif
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
#if 0
	printf("------------ UXTAB16 ------------\n");
        TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x31415927, 0x27182819, 
                  r0, r1, r2, 0);

        TESTINST3("uxtab16 r0, r1, r2, ROR #24", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #16", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #8", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
        TESTINST3("uxtab16 r0, r1, r2, ROR #0", 0x31415927, 0x27182899, 
                  r0, r1, r2, 0);
#endif
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
#if 0
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
#if 0
          __asm__ __volatile__( "pld [%0, %1, LSR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, ASR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, ROR #1]" : : /*in*/"r"(&alocal), "r"(0) );
          __asm__ __volatile__( "pld [%0, %1, RRX]" : : /*in*/"r"(&alocal), "r"(0) );
#endif
        }
	printf("------------ PLD/PLDW (done) ------------\n");
#endif

	return 0;
}


////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////


int main ( void )
{
   // 16 bit instructions

   printf("CMP-16 0x10a\n");
   TESTCARRY
   TESTINST3("cmp r3, r6", 0,          0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", 1,          0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", 0,          1,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", -1,         0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", 0,          -1,          r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", 0,          0x80000000,  r6/*fake*/, r3, r6, 0);
   TESTINST3("cmp r3, r6", 0x80000000, 0,           r6/*fake*/, r3, r6, 0);
   TESTCARRYEND

   printf("CMN-16 0x10a\n");
   TESTCARRY
   TESTINST3("cmn r3, r6", 0,          0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", 1,          0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", 0,          1,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", -1,         0,           r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", 0,          -1,          r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", 0,          0x80000000,  r6/*fake*/, r3, r6, 0);
   TESTINST3("cmn r3, r6", 0x80000000, 0,           r6/*fake*/, r3, r6, 0);
   TESTCARRYEND

   printf("TST-16 0x108\n");
   TESTCARRY
   TESTINST3("tst r3, r6", 0,          0,           r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", 1,          0,           r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", 0,          1,           r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", 1,          1,           r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", -1,         0,           r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", 0,          -1,          r6/*fake*/, r3, r6, cv);
   TESTINST3("tst r3, r6", -1,         -1,          r6/*fake*/, r3, r6, cv);
   TESTCARRYEND

   printf("NEGS-16 0x109\n");
   TESTINST2("negs r0, r1", 1, r0, r1, 0);
   TESTCARRY
   TESTINST2("negs r0, r1", 1, r0, r1, cv);
   TESTINST2("negs r0, r1", 0, r0, r1, cv);
   TESTINST2("negs r0, r1", 0x80000000, r0, r1, cv);
   TESTINST2("negs r0, r1", 0x80000001, r0, r1, cv);
   TESTINST2("negs r0, r1", 0xFFFFFFFF, r0, r1, cv);
   TESTINST2("negs r0, r1", 0x7FFFFFFF, r0, r1, cv);
   TESTCARRYEND

   printf("MVNS-16 0x10F\n");
   TESTINST2("mvns r0, r1", 1, r0, r1, 0);
   TESTCARRY
   TESTINST2("mvns r0, r1", 1, r0, r1, cv);
   TESTINST2("mvns r0, r1", 0, r0, r1, cv);
   TESTINST2("mvns r0, r1", 0x80000000, r0, r1, cv);
   TESTINST2("mvns r0, r1", 0x80000001, r0, r1, cv);
   TESTINST2("mvns r0, r1", 0xFFFFFFFF, r0, r1, cv);
   TESTINST2("mvns r0, r1", 0x7FFFFFFF, r0, r1, cv);
   TESTCARRYEND

   printf("ORRS-16 0x10C\n");
   TESTCARRY
   TESTINST2x("orrs r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("orrs r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("ANDS-16 0x100\n");
   TESTCARRY
   TESTINST2x("ands r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("ands r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("EORS-16 0x101\n");
   TESTCARRY
   TESTINST2x("eors r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("eors r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("MULS-16 0x10d\n");
   TESTCARRY
   TESTINST2x("muls r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("muls r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("BICS-16 0x10E\n");
   TESTCARRY
   TESTINST2x("bics r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("bics r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("ADCS-16 0x105\n");
   TESTCARRY
   TESTINST2x("adcs r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("adcs r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("SBCS-16 0x100\n");
   TESTCARRY
   TESTINST2x("sbcs r1, r2", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x00000000, 0x00000000, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x00000001, 0x00000000, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x00000000, 0x00000001, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x80000000, 0x00000000, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x00000000, 0x80000000, r1, r2, cv);
   TESTINST2x("sbcs r1, r2", 0x80000000, 0x80000000, r1, r2, cv);
   TESTCARRYEND

   printf("UXTB-16 0x2CB\n");
   TESTCARRY
   TESTINST2("uxtb r1, r2", 0x31415927, r1, r2, cv);
   TESTINST2("uxtb r1, r2", 0x31415997, r1, r2, cv);
   TESTCARRYEND

   printf("SXTB-16 0x2C9\n");
   TESTCARRY
   TESTINST2("sxtb r1, r2", 0x31415927, r1, r2, cv);
   TESTINST2("sxtb r1, r2", 0x31415997, r1, r2, cv);
   TESTCARRYEND

   printf("UXTH-16 0x2CA\n");
   TESTCARRY
   TESTINST2("uxth r1, r2", 0x31415927, r1, r2, cv);
   TESTINST2("uxth r1, r2", 0x31419597, r1, r2, cv);
   TESTCARRYEND

   printf("SXTH-16 0x2C8\n");
   TESTCARRY
   TESTINST2("sxth r1, r2", 0x31415927, r1, r2, cv);
   TESTINST2("sxth r1, r2", 0x31419597, r1, r2, cv);
   TESTCARRYEND

   printf("LSLS-16 0x102\n");
   TESTCARRY
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000000, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000001, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000002, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x0000000F, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000010, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x0000001F, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000020, r1, r2, cv);
   TESTINST2x("lsls r1, r2", 0x31415927, 0x00000021, r1, r2, cv);
   TESTCARRYEND

   printf("LSRS-16 0x103\n");
   TESTCARRY
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000000, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000001, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000002, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x0000000F, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000010, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x0000001F, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000020, r1, r2, cv);
   TESTINST2x("lsrs r1, r2", 0x31415927, 0x00000021, r1, r2, cv);
   TESTCARRYEND

   printf("ASRS-16 0x104\n");
   TESTCARRY
   TESTINST2x("asrs r1, r2", 0x31415927, 0x00000000, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x91415927, 0x00000001, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x31415927, 0x00000002, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x91415927, 0x0000000F, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x31415927, 0x00000010, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x91415927, 0x0000001F, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x31415927, 0x00000020, r1, r2, cv);
   TESTINST2x("asrs r1, r2", 0x91415927, 0x00000021, r1, r2, cv);
   TESTCARRYEND

   printf("RORS-16 0x107\n");
   TESTCARRY
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000000, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000001, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000002, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x0000000F, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000010, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x0000001F, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000020, r1, r2, cv);
   TESTINST2x("rors r1, r2", 0x31415927, 0x00000021, r1, r2, cv);
   TESTCARRYEND

   printf("ADD(HI)-16\n");
   TESTCARRY
   TESTINST2x("add r5, r12", 0x31415927, 0x12345678, r5, r12, cv);
   TESTINST2x("add r4, r9 ", 0x31415927, 0x12345678, r4, r9,  cv);
   TESTCARRYEND

   printf("CMP(HI)-16 0x10a\n");
   TESTCARRY
   TESTINST3("cmp r5, r12", 0,          0,           r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", 1,          0,           r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", 0,          1,           r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", -1,         0,           r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", 0,          -1,          r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", 0,          0x80000000,  r12/*fake*/, r5, r12, 0);
   TESTINST3("cmp r5, r12", 0x80000000, 0,           r12/*fake*/, r5, r12, 0);
   TESTCARRYEND

   printf("MOV(HI)-16\n");
   TESTCARRY
   TESTINST2x("mov r5, r12", 0x31415927, 0x12345678, r5, r12, cv);
   TESTINST2x("mov r4, r9 ", 0x31415927, 0x12345678, r4, r9,  cv);
   TESTCARRYEND

   printf("ADDS-16 Rd, Rn, #imm3\n");
   TESTCARRY
   TESTINST2x("adds r1, r2, #1", 0x31415927, 0x27181728, r1, r2, cv);
   TESTINST2x("adds r1, r2, #7", 0x31415927, 0x97181728, r1, r2, cv);
   TESTCARRYEND

   printf("ADDS-16 Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("adds r1, r2, r3", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds r1, r2, r3", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("SUBS-16 Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("subs r1, r2, r3", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs r1, r2, r3", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("ADDS-16 Rn, #uimm8\n");
   TESTCARRY
   TESTINST1x("adds r1, #0  ", 0x31415927, r1, cv);
   TESTINST1x("adds r1, #255", 0x31415927, r1, cv);
   TESTINST1x("adds r1, #0  ", 0x91415927, r1, cv);
   TESTINST1x("adds r1, #255", 0x91415927, r1, cv);
   TESTCARRYEND

   printf("SUBS-16 Rn, #uimm8\n");
   TESTCARRY
   TESTINST1x("subs r1, #0  ", 0x31415927, r1, cv);
   TESTINST1x("subs r1, #255", 0x31415927, r1, cv);
   TESTINST1x("subs r1, #0  ", 0x91415927, r1, cv);
   TESTINST1x("subs r1, #255", 0x91415927, r1, cv);
   TESTCARRYEND

   printf("CMP-16 Rn, #uimm8\n");
   TESTCARRY
   TESTINST1x("cmp r1, #0x80  ", 0x00000080, r1, cv);
   TESTINST1x("cmp r1, #0x7f  ", 0x00000080, r1, cv);
   TESTINST1x("cmp r1, #0x81  ", 0x00000080, r1, cv);
   TESTINST1x("cmp r1, #0x80  ", 0xffffff80, r1, cv);
   TESTINST1x("cmp r1, #0x7f  ", 0xffffff80, r1, cv);
   TESTINST1x("cmp r1, #0x81  ", 0xffffff80, r1, cv);
   TESTINST1x("cmp r1, #0x01  ", 0x80000000, r1, cv);
   TESTCARRYEND

   printf("MOVS-16 Rn, #uimm8\n");
   TESTCARRY
   TESTINST1x("movs r1, #0   ", 0x31415927, r1, cv);
   TESTINST1x("movs r1, #0x7f", 0x31415927, r1, cv);
   TESTINST1x("movs r1, #0x80", 0x31415927, r1, cv);
   TESTINST1x("movs r1, #0x81", 0x31415927, r1, cv);
   TESTINST1x("movs r1, #0xff", 0x31415927, r1, cv);
   TESTCARRYEND

   printf("LSLS-16 Rd, Rm, imm5\n");
   TESTCARRY
   TESTINST2("lsls r1, r2, #0   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsls r1, r2, #1   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsls r1, r2, #2   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsls r1, r2, #0xF ", 0x31415927, r1, r2, cv);
   TESTINST2("lsls r1, r2, #0x10", 0x31415927, r1, r2, cv);
   TESTINST2("lsls r1, r2, #0x1F", 0x31415927, r1, r2, cv);
   TESTCARRYEND

   printf("LSRS-16 Rd, Rm, imm5\n");
   TESTCARRY
   TESTINST2("lsrs r1, r2, #0   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsrs r1, r2, #1   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsrs r1, r2, #2   ", 0x31415927, r1, r2, cv);
   TESTINST2("lsrs r1, r2, #0xF ", 0x31415927, r1, r2, cv);
   TESTINST2("lsrs r1, r2, #0x10", 0x31415927, r1, r2, cv);
   TESTINST2("lsrs r1, r2, #0x1F", 0x31415927, r1, r2, cv);
   TESTCARRYEND

   printf("ASRS-16 Rd, Rm, imm5\n");
   TESTCARRY
   TESTINST2("asrs r1, r2, #0   ", 0x31415927, r1, r2, cv);
   TESTINST2("asrs r1, r2, #1   ", 0x91415927, r1, r2, cv);
   TESTINST2("asrs r1, r2, #2   ", 0x31415927, r1, r2, cv);
   TESTINST2("asrs r1, r2, #0xF ", 0x91415927, r1, r2, cv);
   TESTINST2("asrs r1, r2, #0x10", 0x31415927, r1, r2, cv);
   TESTINST2("asrs r1, r2, #0x1F", 0x91415927, r1, r2, cv);
   TESTCARRYEND

   // 32 bit instructions

   printf("(T3) ADD{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("adds.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("adds.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("adds.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("adds.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("add.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("add.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("add.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T4) ADDW Rd, Rn, #uimm12\n");
   TESTCARRY
   TESTINST2("addw r1, r2, #0x000", 0x31415927, r1, r2, cv);
   TESTINST2("addw r1, r2, #0x000", 0x91415927, r1, r2, cv);
   TESTINST2("addw r1, r2, #0xABC", 0x31415927, r1, r2, cv);
   TESTINST2("addw r1, r2, #0xABC", 0x91415927, r1, r2, cv);
   TESTINST2("addw r1, r2, #0xFFF", 0x31415927, r1, r2, cv);
   TESTINST2("addw r1, r2, #0xFFF", 0x91415927, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) CMP.W Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST1x("cmp.w r1, #0xffffffff", 0x31415927, r1, cv);
   TESTINST1x("cmp.w r1, #0xee00ee00", 0x31415927, r1, cv);
   TESTINST1x("cmp.w r1, #255       ", 0,          r1, cv);
   TESTINST1x("cmp.w r1, #0         ", 1,          r1, cv);
   TESTINST1x("cmp.w r1, #1         ", 0,          r1, cv);
   TESTINST1x("cmp.w r1, #0         ", -1,         r1, cv);
   TESTINST1x("cmp.w r1, #-1        ", 0,          r1, cv);
   TESTINST1x("cmp.w r1, #0x80000000", 0,          r1, cv);
   TESTINST1x("cmp.w r1, #0         ", 0x80000000, r1, cv);
   TESTINST1x("cmp.w r1, #0x80000000", 0x80000000, r1, cv);
   TESTINST1x("cmp.w r1, #0x80000000", 0x7fffffff, r1, cv);
   TESTINST1x("cmp.w r1, #0xff000000", 0x80000000, r1, cv);
   TESTINST1x("cmp.w r1, #0x0dd00000", 0x7fffffff, r1, cv);
   TESTCARRYEND

   printf("(T3) CMN.W Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST1x("cmn.w r1, #0xffffffff", 0x31415927, r1, cv);
   TESTINST1x("cmn.w r1, #0xee00ee00", 0x31415927, r1, cv);
   TESTINST1x("cmn.w r1, #255       ", 0,          r1, cv);
   TESTINST1x("cmn.w r1, #0         ", 1,          r1, cv);
   TESTINST1x("cmn.w r1, #1         ", 0,          r1, cv);
   TESTINST1x("cmn.w r1, #0         ", -1,         r1, cv);
   TESTINST1x("cmn.w r1, #-1        ", 0,          r1, cv);
   TESTINST1x("cmn.w r1, #0x80000000", 0,          r1, cv);
   TESTINST1x("cmn.w r1, #0         ", 0x80000000, r1, cv);
   TESTINST1x("cmn.w r1, #0x80000000", 0x80000000, r1, cv);
   TESTINST1x("cmn.w r1, #0x80000000", 0x7fffffff, r1, cv);
   TESTINST1x("cmn.w r1, #0xff000000", 0x80000000, r1, cv);
   TESTINST1x("cmn.w r1, #0x0dd00000", 0x7fffffff, r1, cv);
   TESTCARRYEND

   printf("(T3) TST.W Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST1x("tst.w r1, #0xffffffff", 0x31415927, r1, cv);
   TESTINST1x("tst.w r1, #0xee00ee00", 0x31415927, r1, cv);
   TESTINST1x("tst.w r1, #255       ", 0,          r1, cv);
   TESTINST1x("tst.w r1, #0         ", 1,          r1, cv);
   TESTINST1x("tst.w r1, #1         ", 0,          r1, cv);
   TESTINST1x("tst.w r1, #0         ", -1,         r1, cv);
   TESTINST1x("tst.w r1, #-1        ", 0,          r1, cv);
   TESTINST1x("tst.w r1, #0x80000000", 0,          r1, cv);
   TESTINST1x("tst.w r1, #0         ", 0x80000000, r1, cv);
   TESTINST1x("tst.w r1, #0x80000000", 0x80000000, r1, cv);
   TESTINST1x("tst.w r1, #0x80000000", 0x7fffffff, r1, cv);
   TESTINST1x("tst.w r1, #0xff000000", 0x80000000, r1, cv);
   TESTINST1x("tst.w r1, #0x0dd00000", 0x7fffffff, r1, cv);
   TESTCARRYEND

   printf("(T3) TEQ.W Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST1x("teq.w r1, #0xffffffff", 0x31415927, r1, cv);
   TESTINST1x("teq.w r1, #0xee00ee00", 0x31415927, r1, cv);
   TESTINST1x("teq.w r1, #255       ", 0,          r1, cv);
   TESTINST1x("teq.w r1, #0         ", 1,          r1, cv);
   TESTINST1x("teq.w r1, #1         ", 0,          r1, cv);
   TESTINST1x("teq.w r1, #0         ", -1,         r1, cv);
   TESTINST1x("teq.w r1, #-1        ", 0,          r1, cv);
   TESTINST1x("teq.w r1, #0x80000000", 0,          r1, cv);
   TESTINST1x("teq.w r1, #0         ", 0x80000000, r1, cv);
   TESTINST1x("teq.w r1, #0x80000000", 0x80000000, r1, cv);
   TESTINST1x("teq.w r1, #0x80000000", 0x7fffffff, r1, cv);
   TESTINST1x("teq.w r1, #0xff000000", 0x80000000, r1, cv);
   TESTINST1x("teq.w r1, #0x0dd00000", 0x7fffffff, r1, cv);
   TESTCARRYEND

   printf("(T3) SUB{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("subs.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("subs.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("subs.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("subs.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("sub.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T4) SUBW Rd, Rn, #uimm12\n");
   TESTCARRY
   TESTINST2("subw r1, r2, #0x000", 0x31415927, r1, r2, cv);
   TESTINST2("subw r1, r2, #0x000", 0x91415927, r1, r2, cv);
   TESTINST2("subw r1, r2, #0xABC", 0x31415927, r1, r2, cv);
   TESTINST2("subw r1, r2, #0xABC", 0x91415927, r1, r2, cv);
   TESTINST2("subw r1, r2, #0xFFF", 0x31415927, r1, r2, cv);
   TESTINST2("subw r1, r2, #0xFFF", 0x91415927, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) RSB{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("rsbs.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("rsbs.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("rsb.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) ADC{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("adcs.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("adcs.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("adc.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) SBC{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("sbcs.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("sbcs.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("sbc.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) AND{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("ands.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("ands.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("ands.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("ands.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("and.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("and.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("and.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) ORR{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("orrs.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("orrs.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("orr.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) EOR{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("eors.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("eors.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("eors.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("eors.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("eor.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) BIC{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("bics.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("bics.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("bics.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("bics.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("bic.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T3) ORN{S}.W Rd, Rn, #constT [allegedly]\n");
   TESTCARRY
   TESTINST2("orns.w r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("orns.w r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("orns.w r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("orns.w r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0xffffffff", 0x31415927, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0xee00ee00", 0x31415927, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #255       ", 0,          r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0         ", 1,          r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #1         ", 0,          r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0         ", -1,         r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #-1        ", 0,          r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0x80000000", 0,          r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0         ", 0x80000000, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0x80000000", 0x80000000, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0x80000000", 0x7fffffff, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0xff000000", 0x80000000, r1, r2, cv);
   TESTINST2("orn.w  r1, r2, #0x0dd00000", 0x7fffffff, r1, r2, cv);
   TESTCARRYEND

   printf("ADD{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adds.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("add.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("SUBB{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("subs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sub.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("RSB{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsbs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("rsb.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("ADC{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adcs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("adc.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("SBC{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbcs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("sbc.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

#if 0
   printf("XXX{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxxs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("xxx.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND
#endif

   printf("AND{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("ands.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("and.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("ORR{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orrs.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orr.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("EOR{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eors.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("eor.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("BIC{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bics.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("bic.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("ORN{S}.W Rd, Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x31415927, 0x27181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x31415927, 0x97181728, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 1,          0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0,          1,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", -1,         0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0,          -1,         r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0,          0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x80000000, 0,          r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x80000000, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x7fffffff, 0x80000000, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x80000000, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orns.w r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsl #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, lsr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #0 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #1 ", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #15", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTINST3("orn.w  r1, r2, r3, asr #31", 0x7fffffff, 0x7fffffff, r1, r2, r3, cv);
   TESTCARRYEND

   printf("(T?) LSL{S}.W Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("lsls.w r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("lsl.w  r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTCARRYEND

   printf("(T?) LSR{S}.W Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("lsrs.w r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("lsr.w  r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTCARRYEND

   printf("(T?) ASR{S}.W Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("asrs.w r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x91415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x91415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x91415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("asrs.w r1, r2, r3", 0x91415927, 0x00000021, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x91415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x91415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x91415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("asr.w  r1, r2, r3", 0x91415927, 0x00000021, r1, r2, r3, cv);
   TESTCARRYEND

   printf("(T?) ROR{S}.W Rd, Rn, Rm\n");
   TESTCARRY
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("rors.w r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000000, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000001, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000002, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x0000000F, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000010, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x0000001F, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000020, r1, r2, r3, cv);
   TESTINST3("ror.w  r1, r2, r3", 0x31415927, 0x00000021, r1, r2, r3, cv);
   TESTCARRYEND

   printf("MVN{S}.W Rd, Rn, shift,   and MOV{S}.W ditto\n");
   TESTCARRY
   TESTINST2("lsls.w   r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #0 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #1 ", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #15", 0x7fffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #31", 0x7fffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #15", 0x00000000, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #31", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #0 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #1 ", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #15", 0x00000000, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #31", 0x00000000, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #15", 0x00000001, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #31", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #0 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #1 ", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #15", 0x00000001, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #31", 0x00000001, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #0 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #1 ", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #15", 0x9218abcd, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #31", 0x9218abcd, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("lsls.w   r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("lsrs.w   r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("asrs.w   r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("rors.w   r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("lsl.w    r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("lsr.w    r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("asr.w    r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #15", 0xffffffff, r1, r2, cv);
   TESTINST2("ror.w    r1, r2, #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsl #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, lsr #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, asr #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvns.w   r1, r2, ror #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsl #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, lsr #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, asr #31", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #0 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #1 ", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #15", 0xffffffff, r1, r2, cv);
   TESTINST2("mvn.w    r1, r2, ror #31", 0xffffffff, r1, r2, cv);
   TESTCARRYEND

   printf("(T?) TST.W Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST2x("tst.w  r1, r2, lsl #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, lsr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, asr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, ror #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, lsl #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, lsr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, asr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, ror #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, lsl #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, lsr #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, asr #1", 0x91223344, 0x80000000, r1, r2, cv);
   TESTINST2x("tst.w  r1, r2, ror #1", 0x91223344, 0x00000001, r1, r2, cv);
   TESTCARRYEND

   printf("(T?) TEQ.W Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST2x("teq.w  r1, r2, lsl #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, lsr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, asr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, ror #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, lsl #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, lsr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, asr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, ror #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, lsl #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, lsr #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, asr #1", 0x91223344, 0x80000000, r1, r2, cv);
   TESTINST2x("teq.w  r1, r2, ror #1", 0x91223344, 0x00000001, r1, r2, cv);
   TESTCARRYEND

   printf("(T?) CMP.W Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST2x("cmp.w  r1, r2, lsl #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, asr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, ror #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsl #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, asr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, ror #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsl #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsr #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, asr #1", 0x91223344, 0x80000000, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, ror #1", 0x91223344, 0x00000001, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, lsr #2", 0x15555555, 0x55555555, r1, r2, cv);
   TESTINST2x("cmp.w  r1, r2, ror #1", 0x55555555, 0xaaaaaaaa, r1, r2, cv);
   TESTCARRYEND

   printf("(T?) CMN.W Rn, Rm, {shift}\n");
   TESTCARRY
   TESTINST2x("cmn.w  r1, r2, lsl #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, asr #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, ror #1", 0x11223344, 0x99887766, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsl #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, asr #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, ror #1", 0x11223344, 0x00000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsl #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsr #1", 0x91223344, 0x40000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, asr #1", 0x91223344, 0x80000000, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, ror #1", 0x91223344, 0x00000001, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, lsr #2", 0x15555555, 0x55555555, r1, r2, cv);
   TESTINST2x("cmn.w  r1, r2, ror #1", 0x55555555, 0xaaaaaaaa, r1, r2, cv);
   TESTCARRYEND

   printf("(T2) MOV{S}.W Rd, #constT\n");
   TESTCARRY
   TESTINST1("movs.w  r9, 0x00000000", r9, cv);
   TESTINST1("movs.w  r9, 0x000000FF", r9, cv);
   TESTINST1("movs.w  r9, 0x0000007F", r9, cv);
   TESTINST1("movs.w  r9, 0x00FF00FF", r9, cv);
   TESTINST1("movs.w  r9, 0x007F007F", r9, cv);
   TESTINST1("movs.w  r9, 0x43434343", r9, cv);
   TESTINST1("movs.w  r9, 0x93939393", r9, cv);
   TESTINST1("movs.w  r9, 0x93000000", r9, cv);
   TESTINST1("movs.w  r9, 0x43000000", r9, cv);
   TESTINST1("movs.w  r9, 0x09300000", r9, cv);
   TESTINST1("movs.w  r9, 0x04300000", r9, cv);
   TESTINST1("movs.w  r9, 0x00930000", r9, cv);
   TESTINST1("movs.w  r9, 0x00430000", r9, cv);
   TESTINST1("movs.w  r9, 0x00000930", r9, cv);
   TESTINST1("movs.w  r9, 0x00000430", r9, cv);
   TESTINST1("movs.w  r9, 0x00000093", r9, cv);
   TESTINST1("movs.w  r9, 0x00000043", r9, cv);
   TESTINST1("mov.w   r9, 0x00000000", r9, cv);
   TESTINST1("mov.w   r9, 0x000000FF", r9, cv);
   TESTINST1("mov.w   r9, 0x0000007F", r9, cv);
   TESTINST1("mov.w   r9, 0x00FF00FF", r9, cv);
   TESTINST1("mov.w   r9, 0x007F007F", r9, cv);
   TESTINST1("mov.w   r9, 0x43434343", r9, cv);
   TESTINST1("mov.w   r9, 0x93939393", r9, cv);
   TESTINST1("mov.w   r9, 0x93000000", r9, cv);
   TESTINST1("mov.w   r9, 0x43000000", r9, cv);
   TESTINST1("mov.w   r9, 0x09300000", r9, cv);
   TESTINST1("mov.w   r9, 0x04300000", r9, cv);
   TESTINST1("mov.w   r9, 0x00930000", r9, cv);
   TESTINST1("mov.w   r9, 0x00430000", r9, cv);
   TESTINST1("mov.w   r9, 0x00000930", r9, cv);
   TESTINST1("mov.w   r9, 0x00000430", r9, cv);
   TESTINST1("mov.w   r9, 0x00000093", r9, cv);
   TESTINST1("mov.w   r9, 0x00000043", r9, cv);
   TESTCARRYEND

   printf("(T2) MVN{S}.W Rd, #constT\n");
   TESTCARRY
   TESTINST1("mvns.w  r9, 0x00000000", r9, cv);
   TESTINST1("mvns.w  r9, 0x000000FF", r9, cv);
   TESTINST1("mvns.w  r9, 0x0000007F", r9, cv);
   TESTINST1("mvns.w  r9, 0x00FF00FF", r9, cv);
   TESTINST1("mvns.w  r9, 0x007F007F", r9, cv);
   TESTINST1("mvns.w  r9, 0x43434343", r9, cv);
   TESTINST1("mvns.w  r9, 0x93939393", r9, cv);
   TESTINST1("mvns.w  r9, 0x93000000", r9, cv);
   TESTINST1("mvns.w  r9, 0x43000000", r9, cv);
   TESTINST1("mvns.w  r9, 0x09300000", r9, cv);
   TESTINST1("mvns.w  r9, 0x04300000", r9, cv);
   TESTINST1("mvns.w  r9, 0x00930000", r9, cv);
   TESTINST1("mvns.w  r9, 0x00430000", r9, cv);
   TESTINST1("mvns.w  r9, 0x00000930", r9, cv);
   TESTINST1("mvns.w  r9, 0x00000430", r9, cv);
   TESTINST1("mvns.w  r9, 0x00000093", r9, cv);
   TESTINST1("mvns.w  r9, 0x00000043", r9, cv);
   TESTINST1("mvn.w   r9, 0x00000000", r9, cv);
   TESTINST1("mvn.w   r9, 0x000000FF", r9, cv);
   TESTINST1("mvn.w   r9, 0x0000007F", r9, cv);
   TESTINST1("mvn.w   r9, 0x00FF00FF", r9, cv);
   TESTINST1("mvn.w   r9, 0x007F007F", r9, cv);
   TESTINST1("mvn.w   r9, 0x43434343", r9, cv);
   TESTINST1("mvn.w   r9, 0x93939393", r9, cv);
   TESTINST1("mvn.w   r9, 0x93000000", r9, cv);
   TESTINST1("mvn.w   r9, 0x43000000", r9, cv);
   TESTINST1("mvn.w   r9, 0x09300000", r9, cv);
   TESTINST1("mvn.w   r9, 0x04300000", r9, cv);
   TESTINST1("mvn.w   r9, 0x00930000", r9, cv);
   TESTINST1("mvn.w   r9, 0x00430000", r9, cv);
   TESTINST1("mvn.w   r9, 0x00000930", r9, cv);
   TESTINST1("mvn.w   r9, 0x00000430", r9, cv);
   TESTINST1("mvn.w   r9, 0x00000093", r9, cv);
   TESTINST1("mvn.w   r9, 0x00000043", r9, cv);
   TESTCARRYEND

   printf("(T1) RBIT Rd, Rm\n");
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

	printf("(T1) REV Rd, Rm ------------\n");
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

	printf("(T2) REV Rd, Rm ------------\n");
	TESTINST2("rev r8, r9", 0x00000000, r8, r9, 0);
	TESTINST2("rev r8, r9", 0xFFFFFFFF, r8, r9, 0);
	TESTINST2("rev r8, r9", 0x80000000, r8, r9, 0);
	TESTINST2("rev r8, r9", 0x00000001, r8, r9, 0);
	TESTINST2("rev r8, r9", 0x31415927, r8, r9, 0);
	TESTINST2("rev r8, r9", 0x14141562, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xabe8391f, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x9028aa80, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xead1fc6d, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x35c98c55, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x534af1eb, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x45511b08, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x90077f71, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xde8ca84b, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xe37a0dda, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xe5b83d4b, r8, r9, 0);
   TESTINST2("rev r8, r9", 0xbb6d14ec, r8, r9, 0);
   TESTINST2("rev r8, r9", 0x68983cc9, r8, r9, 0);

	printf("(T1) REV16 Rd, Rm ------------\n");
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

	printf("(T2) REV16 Rd, Rm ------------\n");
	TESTINST2("rev16 r8, r9", 0x00000000, r8, r9, 0);
	TESTINST2("rev16 r8, r9", 0xFFFFFFFF, r8, r9, 0);
	TESTINST2("rev16 r8, r9", 0x80000000, r8, r9, 0);
	TESTINST2("rev16 r8, r9", 0x00000001, r8, r9, 0);
	TESTINST2("rev16 r8, r9", 0x31415927, r8, r9, 0);
	TESTINST2("rev16 r8, r9", 0x14141562, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xabe8391f, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x9028aa80, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xead1fc6d, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x35c98c55, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x534af1eb, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x45511b08, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x90077f71, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xde8ca84b, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xe37a0dda, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xe5b83d4b, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0xbb6d14ec, r8, r9, 0);
   TESTINST2("rev16 r8, r9", 0x68983cc9, r8, r9, 0);

	printf("------------ NOP (begin) ------------\n");
        printf("nop\n");
        __asm__ __volatile__("nop" ::: "memory","cc");
        printf("nop.w\n");
        __asm__ __volatile__("nop.w" ::: "memory","cc");
	printf("------------ NOP (end) ------------\n");

   // plus whatever stuff we can throw in from the old ARM test program
   old_main();

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

   return 0;
}
