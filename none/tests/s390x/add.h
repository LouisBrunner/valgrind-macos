#include <stdio.h>

/* Dummy variable. Needed to work around GCC code generation bugs */
volatile long v;

#define ADD_REG_MEM(insn, s1, s2, CARRY)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #CARRY "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "Q" (s2)		\
			: "0", "cc");			\
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, s2, tmp, cc); \
})

#define ADD_REG_REG(insn, s1, s2, CARRY)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #CARRY "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0, %3\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp), "d" (s2)		\
			: "0", "cc");			\
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, s2, tmp, cc); \
})

#define ADD_REG_IMM(insn, s1, s2, CARRY)		\
({							\
	unsigned long tmp = s1;				\
	int cc;						\
	asm volatile(	"lghi 0," #CARRY "\n"		\
			"aghi 0, 0\n"			\
			#insn " %0," #s2 "\n"		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp)			\
			: "0", "cc");			\
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, (unsigned long) s2, tmp, cc); \
})

#define ADD_MEM_IMM(insn, s1, s2, CARRY)		\
({							\
	unsigned long tmp = s1, v2;			\
	register unsigned long *addr asm("5") = &tmp;	\
	int cc;						\
	asm volatile(	"lghi 0," #CARRY "\n"		\
			"aghi 0, 0\n"			\
			insn(s2,5,000,00)		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+Q" (tmp), "=d" (cc)		\
			: "Q" (tmp), "d" (addr)		\
			: "0", "cc");			\
        v2 =  (((signed long)((unsigned long)0x##s2 << 56)) >> 56); \
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, v2, tmp, cc); \
})


#define memsweep(i, s2, carryset)				\
({								\
	ADD_REG_MEM(i, 0ul, s2, carryset);			\
	ADD_REG_MEM(i, 1ul, s2, carryset);			\
	ADD_REG_MEM(i, 0xfffful, s2, carryset);			\
	ADD_REG_MEM(i, 0x7ffful, s2, carryset);			\
	ADD_REG_MEM(i, 0x8000ul, s2, carryset);			\
	ADD_REG_MEM(i, 0xfffffffful, s2, carryset);		\
	ADD_REG_MEM(i, 0x80000000ul, s2, carryset);		\
	ADD_REG_MEM(i, 0x7ffffffful, s2, carryset);		\
	ADD_REG_MEM(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_REG_MEM(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_REG_MEM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define regsweep(i, s2, carryset)				\
({								\
	ADD_REG_REG(i, 0ul, s2, carryset);			\
	ADD_REG_REG(i, 1ul, s2, carryset);			\
	ADD_REG_REG(i, 0xfffful, s2, carryset);			\
	ADD_REG_REG(i, 0x7ffful, s2, carryset);			\
	ADD_REG_REG(i, 0x8000ul, s2, carryset);			\
	ADD_REG_REG(i, 0xfffffffful, s2, carryset);		\
	ADD_REG_REG(i, 0x80000000ul, s2, carryset);		\
	ADD_REG_REG(i, 0x7ffffffful, s2, carryset);		\
	ADD_REG_REG(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_REG_REG(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_REG_REG(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define immsweep(i, s2, carryset)				\
({								\
	ADD_REG_IMM(i, 0ul, s2, carryset);			\
	ADD_REG_IMM(i, 1ul, s2, carryset);			\
	ADD_REG_IMM(i, 0xfffful, s2, carryset);			\
	ADD_REG_IMM(i, 0x7ffful, s2, carryset);			\
	ADD_REG_IMM(i, 0x8000ul, s2, carryset);			\
	ADD_REG_IMM(i, 0xfffffffful, s2, carryset);		\
	ADD_REG_IMM(i, 0x80000000ul, s2, carryset);		\
	ADD_REG_IMM(i, 0x7ffffffful, s2, carryset);		\
	ADD_REG_IMM(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_REG_IMM(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_REG_IMM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define memimmsweep(i, s2, carryset)				\
({								\
	ADD_MEM_IMM(i, 0ul, s2, carryset);			\
	ADD_MEM_IMM(i, 1ul, s2, carryset);			\
	ADD_MEM_IMM(i, 0xfffful, s2, carryset);			\
	ADD_MEM_IMM(i, 0x7ffful, s2, carryset);			\
	ADD_MEM_IMM(i, 0x8000ul, s2, carryset);			\
	ADD_MEM_IMM(i, 0xfffffffful, s2, carryset);		\
	ADD_MEM_IMM(i, 0x80000000ul, s2, carryset);		\
	ADD_MEM_IMM(i, 0x7ffffffful, s2, carryset);		\
	ADD_MEM_IMM(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_MEM_IMM(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_MEM_IMM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define ahysweep(i, s2, carryset)				\
({								\
	ADD_REG_MEM(i, 0ul, s2, carryset);			\
	ADD_REG_MEM(i, 1ul, s2, carryset);			\
	ADD_REG_MEM(i, 0xfffful, s2, carryset);			\
	ADD_REG_MEM(i, 0x7ffful, s2, carryset);			\
	ADD_REG_MEM(i, 0x8000ul, s2, carryset);			\
	ADD_REG_MEM(i, 0xfffffffful, s2, carryset);		\
	ADD_REG_MEM(i, 0x80000000ul, s2, carryset);		\
	ADD_REG_MEM(i, 0x7ffffffful, s2, carryset);		\
	ADD_REG_MEM(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_REG_MEM(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_REG_MEM(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define ADD_REG_LDISP(insn, s1, s2, CARRY)			\
({								\
	register unsigned long tmp asm("2") = s1;		\
	register unsigned long *addr asm("5") = &s2;		\
	int cc;							\
	asm volatile(	"lghi 0," #CARRY "\n"			\
			"aghi 0, 0\n"				\
			insn(2,0,5,000,00)			\
			"ipm %1\n"				\
			"srl %1,28\n"				\
			: "+d" (tmp), "=d" (cc)			\
			: "d" (tmp), "Q" (s2), "d"(addr)	\
			: "cc");				\
	v = tmp; /* work around GCC code gen bug */     \
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, s2, v, cc); \
})

#define ldispsweep(i, s2, carryset)				\
({								\
	ADD_REG_LDISP(i, 0ul, s2, carryset);			\
	ADD_REG_LDISP(i, 1ul, s2, carryset);			\
	ADD_REG_LDISP(i, 0xfffful, s2, carryset);		\
	ADD_REG_LDISP(i, 0x7ffful, s2, carryset);		\
	ADD_REG_LDISP(i, 0x8000ul, s2, carryset);		\
	ADD_REG_LDISP(i, 0xfffffffful, s2, carryset);		\
	ADD_REG_LDISP(i, 0x80000000ul, s2, carryset);		\
	ADD_REG_LDISP(i, 0x7ffffffful, s2, carryset);		\
	ADD_REG_LDISP(i, 0xfffffffffffffffful, s2, carryset);	\
	ADD_REG_LDISP(i, 0x8000000000000000ul, s2, carryset);	\
	ADD_REG_LDISP(i, 0x7ffffffffffffffful, s2, carryset);	\
})

#define ADD_REG_XIMM(insn, s1, us2,s2, CARRY)		\
({							\
	register unsigned long tmp asm("2") = s1;	\
	int cc;						\
	asm volatile(	"lghi 0," #CARRY "\n"		\
			"aghi 0, 0\n"			\
			insn(2,s2)			\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (tmp), "=d" (cc)		\
			: "d" (tmp)			\
			: "0", "cc");			\
	v = tmp; /* work around GCC code gen bug */     \
	printf(#insn " " #CARRY " + %16.16lX + %16.16lX = %16.16lX (cc=%d)\n", s1, (unsigned long) 0x##us2##s2, v, cc); \
})

#define ximmsweep(i, us2, s2, carryset)					\
({									\
	ADD_REG_XIMM(i, 0ul, us2, s2, carryset);			\
	ADD_REG_XIMM(i, 1ul, us2, s2, carryset);			\
	ADD_REG_XIMM(i, 0xfffful, us2, s2, carryset);			\
	ADD_REG_XIMM(i, 0x7ffful, us2, s2, carryset);			\
	ADD_REG_XIMM(i, 0x8000ul, us2, s2, carryset);			\
	ADD_REG_XIMM(i, 0xfffffffful, us2, s2, carryset);		\
	ADD_REG_XIMM(i, 0x80000000ul, us2, s2, carryset);		\
	ADD_REG_XIMM(i, 0x7ffffffful, us2, s2, carryset);		\
	ADD_REG_XIMM(i, 0xfffffffffffffffful, us2, s2, carryset);	\
	ADD_REG_XIMM(i, 0x8000000000000000ul, us2, s2, carryset);	\
	ADD_REG_XIMM(i, 0x7ffffffffffffffful, us2, s2, carryset);	\
})

