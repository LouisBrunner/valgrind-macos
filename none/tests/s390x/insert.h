#include <stdio.h>

/* Dummy variable. Needed to work around GCC code generation bugs */
volatile long v;

#define INSERT_REG_MEM(insn, s1, s2)			\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "Q" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX <- %16.16lX = %16.16lX\n", s1, s2, tmp); \
})

#define INSERT_REG_IMM(insn, s1, s2)			\
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
	printf(#insn " %16.16lX <- %16.16lX = %16.16lX\n", s1, (unsigned long) 0x##s2, v); \
})


#define memsweep(i, s2)					\
({							\
	INSERT_REG_MEM(i, 0ul, s2);			\
	INSERT_REG_MEM(i, 1ul, s2);			\
	INSERT_REG_MEM(i, 0xfffful, s2);		\
	INSERT_REG_MEM(i, 0x7ffful, s2);		\
	INSERT_REG_MEM(i, 0x8000ul, s2);		\
	INSERT_REG_MEM(i, 0xfffffffful, s2);		\
	INSERT_REG_MEM(i, 0x80000000ul, s2);		\
	INSERT_REG_MEM(i, 0x7ffffffful, s2);		\
	INSERT_REG_MEM(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	INSERT_REG_MEM(i, 0x8000000000000000ul, s2);	\
	INSERT_REG_MEM(i, 0xfffffffffffffffful, s2);	\
	INSERT_REG_MEM(i, 0x5555555555555555ul, s2);	\
})

#define immsweep(i, s2)					\
({							\
	INSERT_REG_IMM(i, 0ul, s2);			\
	INSERT_REG_IMM(i, 1ul, s2);			\
	INSERT_REG_IMM(i, 0xfffful, s2);		\
	INSERT_REG_IMM(i, 0x7ffful, s2);		\
	INSERT_REG_IMM(i, 0x8000ul, s2);		\
	INSERT_REG_IMM(i, 0xfffffffful, s2);		\
	INSERT_REG_IMM(i, 0x80000000ul, s2);		\
	INSERT_REG_IMM(i, 0x7ffffffful, s2);		\
	INSERT_REG_IMM(i, 0xaaaaaaaaaaaaaaaaul, s2);	\
	INSERT_REG_IMM(i, 0x8000000000000000ul, s2);	\
	INSERT_REG_IMM(i, 0xfffffffffffffffful, s2);	\
	INSERT_REG_IMM(i, 0x5555555555555555ul, s2);	\
})

#define INSERT_ICY(s1, s2)			       		\
({								\
	register unsigned long tmp asm("1") = s1;		\
	register unsigned long *addr asm("2") = &s2;		\
	int cc;							\
	asm volatile(	ICY(1,0,2,000,00)			\
			"ipm %1\n"				\
			"srl %1,28\n"				\
			: "+d" (tmp), "=d" (cc)			\
			: "d" (tmp), "Q" (s2), "d" (addr)	\
			: "cc");				\
	printf("icy %16.16lX <- %16.16lX = %16.16lX\n", s1, s2, tmp); \
})

#define icysweep(s2)				\
({						\
	INSERT_ICY(0ul, s2);			\
	INSERT_ICY(1ul, s2);			\
	INSERT_ICY(0xfffful, s2);		\
	INSERT_ICY(0x7ffful, s2);		\
	INSERT_ICY(0x8000ul, s2);		\
	INSERT_ICY(0xfffffffful, s2);		\
	INSERT_ICY(0x80000000ul, s2);		\
	INSERT_ICY(0x7ffffffful, s2);		\
	INSERT_ICY(0xaaaaaaaaaaaaaaaaul, s2);	\
	INSERT_ICY(0x8000000000000000ul, s2);	\
	INSERT_ICY(0xfffffffffffffffful, s2);	\
	INSERT_ICY(0x5555555555555555ul, s2);	\
})
