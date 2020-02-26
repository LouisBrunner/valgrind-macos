#include <stdio.h>

#define MUL_REG_MEM(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	int cc;						\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			insn("2", "%[m2]")		\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "Q" (m2)			\
			: "2","3","cc");		\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
})

#define MUL_REG_REG(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	int cc;						\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			insn("2", "%[m2]")		\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "d" (m2)			\
			: "2","3","cc");		\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
})

#define MUL_REG_REG_REG(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	int cc;						\
	asm volatile(	"xr %[cc],%[cc]\n"		\
			"lghi 2,0\n"                   \
			"lghi 3,0\n"                   \
			insn("2", "%[tmp1]", "%[m2]")	\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "d" (m2)			\
			: "2","3","cc");		\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
})

#define MUL_REG_IMM(insn, m1, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	int cc;						\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			insn("2", #m2)			\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			:: "2","3","cc");		\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, (unsigned long) m2, tmp1, tmp2, cc); \
})

#define MUL_REG_XIMM(insn, m1, um2, m2)			\
({							\
	unsigned long tmp1 = m1;			\
	unsigned long tmp2 = m1;			\
	int cc;						\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			insn(2,m2)			\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			:: "2","3","cc");		\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, (unsigned long) 0x##um2##m2, tmp1, tmp2, cc); \
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

#define regregsweep(i, m2)				\
({							\
	MUL_REG_REG_REG(i, 0ul, m2);			\
	MUL_REG_REG_REG(i, 1ul, m2);			\
	MUL_REG_REG_REG(i, 0xfffful, m2);		\
	MUL_REG_REG_REG(i, 0x7ffful, m2);		\
	MUL_REG_REG_REG(i, 0x8000ul, m2);		\
	MUL_REG_REG_REG(i, 0xfffffffful, m2);		\
	MUL_REG_REG_REG(i, 0x80000000ul, m2);		\
	MUL_REG_REG_REG(i, 0x7ffffffful, m2);		\
	MUL_REG_REG_REG(i, 0xfffffffffffffffful, m2);	\
	MUL_REG_REG_REG(i, 0x8000000000000000ul, m2);	\
	MUL_REG_REG_REG(i, 0x7ffffffffffffffful, m2);	\
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
	int cc;						\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			MSY(2,0,5,000,00)		\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "Q" (m2)			\
			, [addr] "d" (addr)		\
			: "2","3","cc");		\
	printf("msy %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
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
	int cc;						\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			MHY(2,0,5,000,00)		\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2] "+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "Q" (m2)			\
			, [addr] "d" (addr)		\
			: "2","3","cc");		\
	printf("mhy %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
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
	int cc;						\
	register unsigned long *addr asm("5") = &m2;	\
	asm volatile(	"lgr 2, %[tmp1]\n"		\
			"lgr 3, %[tmp2]\n"		\
			"xr %[cc],%[cc]\n"		\
			MFY(2,0,5,000,00)		\
			"ipm %[cc]\n"			\
			"srl %[cc],28\n"		\
			"lgr %[tmp1],2\n"		\
			"lgr %[tmp2],3\n"		\
			: [tmp1] "+d" (tmp1)		\
			, [tmp2]"+d" (tmp2)		\
			, [cc] "=&d" (cc)		\
			: [m2] "Q" (m2)			\
			, [addr] "d" (addr)		\
			: "2","3");			\
	printf("mfy %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", m1, m2, tmp1, tmp2, cc); \
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

#define for_each_m2(f)				\
({						\
	do_regmem_insns(0x0ul);			\
	do_regmem_insns(0x7ffffffffffffffful);	\
	do_regmem_insns(0x8000000000000000ul);	\
	do_regmem_insns(0xfffffffffffffffful);	\
	do_regmem_insns(0x7fffffff00000000ul);	\
	do_regmem_insns(0x8000000000000000ul);	\
	do_regmem_insns(0xffffffff00000000ul);	\
	do_regmem_insns(0x000000007ffffffful);	\
	do_regmem_insns(0x0000000080000000ul);	\
	do_regmem_insns(0x00000000fffffffful);	\
	do_regmem_insns(0x000000000000fffful);	\
	do_regmem_insns(0x0000000000007ffful);	\
	do_regmem_insns(0x0000000000008000ul);	\
	do_regmem_insns(0x000000000000fffful);	\
})
