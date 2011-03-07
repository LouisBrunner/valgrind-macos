#include <stdio.h>

#define DIV_REG_MEM(insn, d1_1, d1_2, d2)		\
({							\
	unsigned long tmp1 = d1_1;			\
	unsigned long tmp2 = d1_2;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			#insn " 2, %2\n"		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "Q" (d2)			\
			: "2","3");			\
	printf(#insn " %16.16lX%16.16lX / %16.16lX = %16.16lX (rem %16.16lX)\n", d1_1, d1_2, d2, tmp2, tmp1); \
})

#define DIV_REG_REG(insn, d1_1, d1_2, d2)		\
({							\
	unsigned long tmp1 = d1_1;			\
	unsigned long tmp2 = d1_2;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			#insn " 2, %2\n"		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "d" (d2)			\
			: "2","3");			\
	printf(#insn " %16.16lX%16.16lX / %16.16lX = %16.16lX (rem %16.16lX)\n", d1_1, d1_2, d2, tmp2, tmp1); \
})


#define memsweep(i, d2)						\
({								\
	DIV_REG_MEM(i, 0x0ul, 0ul, d2);				\
	DIV_REG_MEM(i, 0x0ul, 1ul, d2);				\
	DIV_REG_MEM(i, 0x0ul, 0xfffful, d2);			\
	DIV_REG_MEM(i, 0x0ul, 0x7ffful, d2);			\
	DIV_REG_MEM(i, 0x0ul, 0x8000ul, d2);			\
	DIV_REG_MEM(i, 0x0ul, 0xfffffffful, d2);		\
	DIV_REG_MEM(i, 0x0ul, 0x80000000ul, d2);		\
	DIV_REG_MEM(i, 0x0ul, 0x7ffffffful, d2);		\
	DIV_REG_MEM(i, 0x0ul, 0xfffffffffffffffful, d2);	\
	DIV_REG_MEM(i, 0x0ul, 0x8000000000000000ul, d2);	\
	DIV_REG_MEM(i, 0x0ul, 0x7ffffffffffffffful, d2);	\
	DIV_REG_MEM(i, 0x1ul, 0xaffffffful, d2);		\
})

#define regsweep(i, d2)						\
({								\
	DIV_REG_REG(i, 0x0ul, 0ul, d2);				\
	DIV_REG_REG(i, 0x0ul, 1ul, d2);				\
	DIV_REG_REG(i, 0x0ul, 0xfffful, d2);			\
	DIV_REG_REG(i, 0x0ul, 0x7ffful, d2);			\
	DIV_REG_REG(i, 0x0ul, 0x8000ul, d2);			\
	DIV_REG_REG(i, 0x0ul, 0xfffffffful, d2);		\
	DIV_REG_REG(i, 0x0ul, 0x80000000ul, d2);		\
	DIV_REG_REG(i, 0x0ul, 0x7ffffffful, d2);		\
	DIV_REG_REG(i, 0x0ul, 0xfffffffffffffffful, d2);	\
	DIV_REG_REG(i, 0x0ul, 0x8000000000000000ul, d2);	\
	DIV_REG_REG(i, 0x0ul, 0x7ffffffffffffffful, d2);	\
	DIV_REG_REG(i, 0x1ul, 0xaffffffful, d2);		\
})

