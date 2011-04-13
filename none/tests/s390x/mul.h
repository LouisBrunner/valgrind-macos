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

#define MUL_REG_XIMM(insn, m1, um2, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			insn(2,m2)			\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			:: "2","3");			\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, (unsigned long) 0x##um2##m2, tmp1, tmp2); \
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

#define ximmsweep(i, um2, m2)				\
({							\
	MUL_REG_XIMM(i, 0ul, um2, m2);			\
	MUL_REG_XIMM(i, 1ul, um2, m2);			\
	MUL_REG_XIMM(i, 0xfffful, um2, m2);		\
	MUL_REG_XIMM(i, 0x7ffful, um2, m2);		\
	MUL_REG_XIMM(i, 0x8000ul, um2, m2);		\
	MUL_REG_XIMM(i, 0xfffffffful, um2, m2);		\
	MUL_REG_XIMM(i, 0x80000000ul, um2, m2);		\
	MUL_REG_XIMM(i, 0x7ffffffful, um2, m2);		\
	MUL_REG_XIMM(i, 0xfffffffffffffffful, um2, m2);	\
	MUL_REG_XIMM(i, 0x8000000000000000ul, um2, m2);	\
	MUL_REG_XIMM(i, 0x7ffffffffffffffful, um2, m2);	\
})

#define MUL_MSY(m1, m2)					\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			MSY(2,0,5,000,00)		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "Q" (m2), "d" (addr)		\
			: "2","3");			\
	printf("msy %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, m2, tmp1, tmp2); \
})

#define msysweep(s2)				\
({						\
	MUL_MSY(0ul, s2);			\
	MUL_MSY(1ul, s2);			\
	MUL_MSY(0xfffful, s2);			\
	MUL_MSY(0x7ffful, s2);			\
	MUL_MSY(0x8000ul, s2);			\
	MUL_MSY(0xfffffffful, s2);		\
	MUL_MSY(0x80000000ul, s2);		\
	MUL_MSY(0x7ffffffful, s2);		\
	MUL_MSY(0xfffffffffffffffful, s2);	\
	MUL_MSY(0x8000000000000000ul, s2);	\
	MUL_MSY(0x7ffffffffffffffful, s2);	\
})

#define MUL_MHY(m1, m2)					\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			MHY(2,0,5,000,00)		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "Q" (m2), "d" (addr)		\
			: "2","3");			\
	printf("mhy %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, m2, tmp1, tmp2); \
})

#define mhysweep(s2)				\
({						\
	MUL_MHY(0ul, s2);			\
	MUL_MHY(1ul, s2);			\
	MUL_MHY(0xfffful, s2);			\
	MUL_MHY(0x7ffful, s2);			\
	MUL_MHY(0x8000ul, s2);			\
	MUL_MHY(0xfffffffful, s2);		\
	MUL_MHY(0x80000000ul, s2);		\
	MUL_MHY(0x7ffffffful, s2);		\
	MUL_MHY(0xfffffffffffffffful, s2);	\
	MUL_MHY(0x8000000000000000ul, s2);	\
	MUL_MHY(0x7ffffffffffffffful, s2);	\
})

#define MUL_MFY(m1, m2)					\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %0\n"			\
			"lgr 3, %1\n"			\
			MFY(2,0,5,000,00)		\
			"lgr %0,2\n"			\
			"lgr %1,3\n"			\
			: "+d" (tmp1), "+d" (tmp2)	\
			: "Q" (m2), "d" (addr)		\
			: "2","3");			\
	printf("mfy %16.16lX * %16.16lX = %16.16lX%16.16lX\n", m1, m2, tmp1, tmp2); \
})

#define mfysweep(s2)				\
({						\
	MUL_MFY(0ul, s2);			\
	MUL_MFY(1ul, s2);			\
	MUL_MFY(0xfffful, s2);			\
	MUL_MFY(0x7ffful, s2);			\
	MUL_MFY(0x8000ul, s2);			\
	MUL_MFY(0xfffffffful, s2);		\
	MUL_MFY(0x80000000ul, s2);		\
	MUL_MFY(0x7ffffffful, s2);		\
	MUL_MFY(0xfffffffffffffffful, s2);	\
	MUL_MFY(0x8000000000000000ul, s2);	\
	MUL_MFY(0x7ffffffffffffffful, s2);	\
})
