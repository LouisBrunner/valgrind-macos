#include <stdio.h>

/* Dummy variable. Needed to work around GCC code generation bugs */
volatile long v;

#define XOR_REG_MEM(insn, s1, s2)			\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "Q" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, s2, tmp, cc); \
})

#define XOR_REG_REG(insn, s1, s2)			\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "d" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, s2, tmp, cc); \
})

#define XOR_REG_IMM(insn, s1, s2)			\
({							\
	register unsigned long tmp asm("2") = s1;	\
	int cc;						\
	asm volatile(	insn(2,s2)			\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp)			\
			: "cc");			\
	v = tmp; /* work around GCC code gen bug */     \
	printf(#insn " %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, (unsigned long) 0x##s2, v, cc); \
})

#define XOR_MEM_IMM(insn, s1, s2)			\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	#insn " %0," #s2 "\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+Q" (tmp), "=d" (cc)		\
			: "Q" (tmp)			\
			: "0", "cc");			\
	printf(#insn " %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, (unsigned long) s2, tmp, cc); \
})


#define memsweep(i, s2)					\
({							\
	XOR_REG_MEM(i, 0ul, s2);			\
	XOR_REG_MEM(i, 1ul, s2);			\
	XOR_REG_MEM(i, 0xfffful, s2);			\
	XOR_REG_MEM(i, 0x7ffful, s2);			\
	XOR_REG_MEM(i, 0x8000ul, s2);			\
	XOR_REG_MEM(i, 0xfffffffful, s2);		\
	XOR_REG_MEM(i, 0x80000000ul, s2);		\
	XOR_REG_MEM(i, 0x7ffffffful, s2);		\
	XOR_REG_MEM(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_REG_MEM(i, 0x8000000000000000ul, s2);	\
	XOR_REG_MEM(i, 0xfffffffffffffffful, s2);	\
	XOR_REG_MEM(i, 0x5555555555555555ul, s2);	\
})

#define regsweep(i, s2)					\
({							\
	XOR_REG_REG(i, 0ul, s2);			\
	XOR_REG_REG(i, 1ul, s2);			\
	XOR_REG_REG(i, 0xfffful, s2);			\
	XOR_REG_REG(i, 0x7ffful, s2);			\
	XOR_REG_REG(i, 0x8000ul, s2);			\
	XOR_REG_REG(i, 0xfffffffful, s2);		\
	XOR_REG_REG(i, 0x80000000ul, s2);		\
	XOR_REG_REG(i, 0x7ffffffful, s2);		\
	XOR_REG_REG(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_REG_REG(i, 0x8000000000000000ul, s2);	\
	XOR_REG_REG(i, 0xfffffffffffffffful, s2);	\
	XOR_REG_REG(i, 0x5555555555555555ul, s2);	\
})

#define immsweep(i, s2)					\
({							\
	XOR_REG_IMM(i, 0ul, s2);			\
	XOR_REG_IMM(i, 1ul, s2);			\
	XOR_REG_IMM(i, 0xfffful, s2);			\
	XOR_REG_IMM(i, 0x7ffful, s2);			\
	XOR_REG_IMM(i, 0x8000ul, s2);			\
	XOR_REG_IMM(i, 0xfffffffful, s2);		\
	XOR_REG_IMM(i, 0x80000000ul, s2);		\
	XOR_REG_IMM(i, 0x7ffffffful, s2);		\
	XOR_REG_IMM(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_REG_IMM(i, 0x8000000000000000ul, s2);	\
	XOR_REG_IMM(i, 0xfffffffffffffffful, s2);	\
	XOR_REG_IMM(i, 0x5555555555555555ul, s2);	\
})

#define memimmsweep(i, s2)				\
({							\
	XOR_MEM_IMM(i, 0ul, s2);			\
	XOR_MEM_IMM(i, 1ul, s2);			\
	XOR_MEM_IMM(i, 0xfffful, s2);			\
	XOR_MEM_IMM(i, 0x7ffful, s2);			\
	XOR_MEM_IMM(i, 0x8000ul, s2);			\
	XOR_MEM_IMM(i, 0xfffffffful, s2);		\
	XOR_MEM_IMM(i, 0x80000000ul, s2);		\
	XOR_MEM_IMM(i, 0x7ffffffful, s2);		\
	XOR_MEM_IMM(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_MEM_IMM(i, 0x8000000000000000ul, s2);	\
	XOR_MEM_IMM(i, 0xfffffffffffffffful, s2);	\
	XOR_MEM_IMM(i, 0x5555555555555555ul, s2);	\
})

#define XOR_XY(s1, s2)					\
({							\
	register unsigned long tmp asm("1") = s1;	\
	register unsigned long *addr asm("2") = &s2;	\
	int cc;						\
	asm volatile(	XY(1,0,2,000,00)		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "d"(addr)		\
			: "cc");		\
	printf("xy %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, s2, tmp, cc); \
})

#define XOR_XIY(s1, i2)					\
({							\
	unsigned long tmp = s1;				\
	register unsigned long *addr asm("2") = &tmp;	\
	int cc;						\
	asm volatile(	XIY(i2,2,000,00)		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+Q" (tmp), "=d" (cc)		\
			: "Q" (tmp), "d" (addr)		\
			: "cc");		\
	printf("xiy %16.16lX ^ %16.16lX = %16.16lX (cc=%d)\n", s1, (unsigned long) 0x##i2, tmp, cc); \
})

#define xysweep(s2)				\
({						\
	XOR_XY(0ul, s2);			\
	XOR_XY(1ul, s2);			\
	XOR_XY(0xfffful, s2);			\
	XOR_XY(0x7ffful, s2);			\
	XOR_XY(0x8000ul, s2);			\
	XOR_XY(0xfffffffful, s2);		\
	XOR_XY(0x80000000ul, s2);		\
	XOR_XY(0x7ffffffful, s2);		\
	XOR_XY(0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_XY(0x8000000000000000ul, s2);	\
	XOR_XY(0xfffffffffffffffful, s2);	\
	XOR_XY(0x5555555555555555ul, s2);	\
})

#define xiysweep(s2)				\
({						\
	XOR_XIY(0ul, s2);			\
	XOR_XIY(1ul, s2);			\
	XOR_XIY(0xfffful, s2);			\
	XOR_XIY(0x7ffful, s2);			\
	XOR_XIY(0x8000ul, s2);			\
	XOR_XIY(0xfffffffful, s2);		\
	XOR_XIY(0x80000000ul, s2);		\
	XOR_XIY(0x7ffffffful, s2);		\
	XOR_XIY(0xaaaaaaaaaaaaaaaaul, s2);	\
	XOR_XIY(0x8000000000000000ul, s2);	\
	XOR_XIY(0xfffffffffffffffful, s2);	\
	XOR_XIY(0x5555555555555555ul, s2);	\
})
