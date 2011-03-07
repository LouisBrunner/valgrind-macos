#include <stdio.h>

#define SUB_REG_MEM(insn, s1, s2, NOBORROW)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #NOBORROW "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "Q" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, s2, !NOBORROW, tmp, cc); \
})

#define SUB_REG_REG(insn, s1, s2, NOBORROW)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #NOBORROW "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "d" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, s2, !NOBORROW, tmp, cc); \
})

#define SUB_REG_IMM(insn, s1, s2, NOBORROW)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #NOBORROW "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0," #s2 "\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp)			\
			: "0", "cc");			\
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, (unsigned long) s2, !NOBORROW, tmp, cc); \
})

#define memsweep(i, s2, carryset)				\
({								\
	SUB_REG_MEM(i, 0ul, s2, carryset);			\
	SUB_REG_MEM(i, 1ul, s2, carryset);			\
	SUB_REG_MEM(i, 0xfffful, s2, carryset);			\
	SUB_REG_MEM(i, 0x7ffful, s2, carryset);			\
	SUB_REG_MEM(i, 0x8000ul, s2, carryset);			\
	SUB_REG_MEM(i, 0xfffffffful, s2, carryset);		\
	SUB_REG_MEM(i, 0x80000000ul, s2, carryset);		\
	SUB_REG_MEM(i, 0x7ffffffful, s2, carryset);		\
	SUB_REG_MEM(i, 0xfffffffffffffffful, s2, carryset);	\
	SUB_REG_MEM(i, 0x8000000000000000ul, s2, carryset);	\
	SUB_REG_MEM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define regsweep(i, s2, carryset)				\
({								\
	SUB_REG_REG(i, 0ul, s2, carryset);			\
	SUB_REG_REG(i, 1ul, s2, carryset);			\
	SUB_REG_REG(i, 0xfffful, s2, carryset);			\
	SUB_REG_REG(i, 0x7ffful, s2, carryset);			\
	SUB_REG_REG(i, 0x8000ul, s2, carryset);			\
	SUB_REG_REG(i, 0xfffffffful, s2, carryset);		\
	SUB_REG_REG(i, 0x80000000ul, s2, carryset);		\
	SUB_REG_REG(i, 0x7ffffffful, s2, carryset);		\
	SUB_REG_REG(i, 0xfffffffffffffffful, s2, carryset);	\
	SUB_REG_REG(i, 0x8000000000000000ul, s2, carryset);	\
	SUB_REG_REG(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define immsweep(i, s2, carryset)				\
({								\
	SUB_REG_IMM(i, 0ul, s2, carryset);			\
	SUB_REG_IMM(i, 1ul, s2, carryset);			\
	SUB_REG_IMM(i, 0xfffful, s2, carryset);			\
	SUB_REG_IMM(i, 0x7ffful, s2, carryset);			\
	SUB_REG_IMM(i, 0x8000ul, s2, carryset);			\
	SUB_REG_IMM(i, 0xfffffffful, s2, carryset);		\
	SUB_REG_IMM(i, 0x80000000ul, s2, carryset);		\
	SUB_REG_IMM(i, 0x7ffffffful, s2, carryset);		\
	SUB_REG_IMM(i, 0xfffffffffffffffful, s2, carryset);	\
	SUB_REG_IMM(i, 0x8000000000000000ul, s2, carryset);	\
	SUB_REG_IMM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

