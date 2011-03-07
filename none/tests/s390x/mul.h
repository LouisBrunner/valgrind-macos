#include <stdio.h>

#define MUL_REG_MEM(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			#insn " 2, %2\n"		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "Q" (m2)			\
			: "2","3");			\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, m2, tmp1, tmp2); \
})

#define MUL_REG_REG(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			#insn " 2, %2\n"		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "d" (m2)			\
			: "2","3");			\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, m2, tmp1, tmp2); \
})

#define MUL_REG_IMM(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			#insn " 2, " #m2 "\n"		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			:: "2","3");			\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, (unsigned long) m2, tmp1, tmp2); \
})


#define memsweep(i, m2)					\
({							\
	MUL_REG_MEM(i, 0ul, m2);			\
	MUL_REG_MEM(i, 1ul, m2);			\
	MUL_REG_MEM(i, 0xfffful, m2);			\
	MUL_REG_MEM(i, 0x7ffful, m2);			\
	MUL_REG_MEM(i, 0x8000ul, m2);			\
	MUL_REG_MEM(i, 0xfffffffful, m2);		\
	MUL_REG_MEM(i, 0x80000000ul, m2);		\
	MUL_REG_MEM(i, 0x7ffffffful, m2);		\
	MUL_REG_MEM(i, 0xfffffffffffffffful, m2);	\
	MUL_REG_MEM(i, 0x8000000000000000ul, m2);	\
	MUL_REG_MEM(i, 0x7ffffffffffffffful, m2);	\
})

#define regsweep(i, m2)					\
({							\
	MUL_REG_REG(i, 0ul, m2);			\
	MUL_REG_REG(i, 1ul, m2);			\
	MUL_REG_REG(i, 0xfffful, m2);			\
	MUL_REG_REG(i, 0x7ffful, m2);			\
	MUL_REG_REG(i, 0x8000ul, m2);			\
	MUL_REG_REG(i, 0xfffffffful, m2);		\
	MUL_REG_REG(i, 0x80000000ul, m2);		\
	MUL_REG_REG(i, 0x7ffffffful, m2);		\
	MUL_REG_REG(i, 0xfffffffffffffffful, m2);	\
	MUL_REG_REG(i, 0x8000000000000000ul, m2);	\
	MUL_REG_REG(i, 0x7ffffffffffffffful, m2);	\
})

#define immsweep(i, m2)					\
({							\
	MUL_REG_IMM(i, 0ul, m2);			\
	MUL_REG_IMM(i, 1ul, m2);			\
	MUL_REG_IMM(i, 0xfffful, m2);			\
	MUL_REG_IMM(i, 0x7ffful, m2);			\
	MUL_REG_IMM(i, 0x8000ul, m2);			\
	MUL_REG_IMM(i, 0xfffffffful, m2);		\
	MUL_REG_IMM(i, 0x80000000ul, m2);		\
	MUL_REG_IMM(i, 0x7ffffffful, m2);		\
	MUL_REG_IMM(i, 0xfffffffffffffffful, m2);	\
	MUL_REG_IMM(i, 0x8000000000000000ul, m2);	\
	MUL_REG_IMM(i, 0x7ffffffffffffffful, m2);	\
})

