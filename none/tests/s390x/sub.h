#include <stdio.h>

/* Dummy variable. Needed to work around GCC code generation bugs */
volatile long v;

#define SUB_REG_MEM(insn, s1, s2, NOBORROW)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #NOBORROW "\n"	\
			"aghi 0, 0\n"			\
			insn("%0", "%3")		\
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
	asm volatile(	"lghi 0," #NOBORROW "\n"	\
			"aghi 0, 0\n"			\
			insn("%0", "%3")		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "d" (s2)		\
			: "0", "cc");			\
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, s2, !NOBORROW, tmp, cc); \
})

#define SUB_REG_IMM(insn, s1, s2, NOBORROW)		\
({							\
	register unsigned long tmp asm("2") = s1;	\
	int cc;						\
	asm volatile(	"lghi 0," #NOBORROW "\n"	\
			"aghi 0, 0\n"			\
                        insn(2,s2)			\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp)			\
			: "0", "cc");			\
	v = tmp;					\
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, (unsigned long) 0x00000000##s2, !NOBORROW, v, cc); \
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

#define SUB_REG_LDISP(insn, s1, s2, NOBORROW)			\
({								\
	register unsigned long tmp asm("2") = s1;		\
	register unsigned long *addr asm("5") = &s2;		\
	int cc;							\
	asm volatile(	"lghi 0," #NOBORROW "\n"		\
			"aghi 0, 0\n"				\
			insn(2,0,5,000,00)			\
			"ipm %1\n"				\
			"srl %1,28\n"				\
			: "+d" (tmp), "=d" (cc)			\
			: "d" (tmp), "Q" (s2), "d"(addr)	\
			: "cc");				\
	v = tmp; /* work around GCC code gen bug */     \
	printf(#insn " %16.16lX - %16.16lX - %d = %16.16lX (cc=%d)\n", s1, s2, !NOBORROW, v, cc); \
})

#define ldispsweep(i, s2, carryset)				\
({								\
	SUB_REG_LDISP(i, 0ul, s2, carryset);			\
	SUB_REG_LDISP(i, 1ul, s2, carryset);			\
	SUB_REG_LDISP(i, 0xfffful, s2, carryset);		\
	SUB_REG_LDISP(i, 0x7ffful, s2, carryset);		\
	SUB_REG_LDISP(i, 0x8000ul, s2, carryset);		\
	SUB_REG_LDISP(i, 0xfffffffful, s2, carryset);		\
	SUB_REG_LDISP(i, 0x80000000ul, s2, carryset);		\
	SUB_REG_LDISP(i, 0x7ffffffful, s2, carryset);		\
	SUB_REG_LDISP(i, 0xfffffffffffffffful, s2, carryset);	\
	SUB_REG_LDISP(i, 0x8000000000000000ul, s2, carryset);	\
	SUB_REG_LDISP(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define for_each_m2(f)				\
({						\
	f(0x0ul);				\
	f(0x7ffffffffffffffful);		\
	f(0x8000000000000000ul);		\
	f(0xfffffffffffffffful);		\
	f(0x7fffffff00000000ul);		\
	f(0x8000000000000000ul);		\
	f(0xffffffff00000000ul);		\
	f(0x000000007ffffffful);		\
	f(0x0000000080000000ul);		\
	f(0x00000000fffffffful);		\
	f(0x000000000000fffful);		\
	f(0x0000000000007ffful);		\
	f(0x0000000000008000ul);		\
	f(0x000000000000fffful);		\
})
