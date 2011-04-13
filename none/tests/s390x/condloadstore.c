#include <stdio.h>
#include <stdlib.h>
#include "opcodes.h"

#define LOAD_REG_MEM(insn, s, ccset, initial, mask)	\
({							\
	register unsigned long target asm("1") = initial;	\
	unsigned long source = s;			\
	register unsigned long *addr asm("5") = &source;	\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			insn(1,mask,5,000,00)		\
			: "+d" (target), "+d" (a)	\
			: "Q" (source), "d" (b), "d"(addr)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 0x##mask, ccset, target);		\
})


#define LOAD_REG_REG(insn, s, ccset, initial, mask)	\
({							\
	register unsigned long target asm("1") = initial;	\
	register unsigned long source asm("2")= s;		\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			insn(mask,1,2)			\
			: "+d" (target), "+d" (a)	\
			: "d" (source), "d" (b)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 0x##mask, ccset, target);		\
})

#define STORE_REG_REG(insn, s, ccset, initial, mask)	\
({							\
	unsigned long target = initial;			\
	register unsigned long source asm("1") = s;	\
	register unsigned long *addr asm("5") = &target;	\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			insn(1,mask,5,000,00)		\
			: "+Q" (target), "+d" (a)	\
			: "d" (source), "d" (b), "d"(addr)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 0x##mask, ccset, target);		\
})


#define INSNVALCCINIT(insn, value, ccset, INIT, FUNC)	\
({							\
	FUNC(insn, value, ccset, INIT, 0);		\
	FUNC(insn, value, ccset, INIT, 1);		\
	FUNC(insn, value, ccset, INIT, 2);		\
	FUNC(insn, value, ccset, INIT, 3);		\
	FUNC(insn, value, ccset, INIT, 4);		\
	FUNC(insn, value, ccset, INIT, 5);		\
	FUNC(insn, value, ccset, INIT, 6);		\
	FUNC(insn, value, ccset, INIT, 7);		\
	FUNC(insn, value, ccset, INIT, 8);		\
	FUNC(insn, value, ccset, INIT, 9);		\
	FUNC(insn, value, ccset, INIT, A);		\
	FUNC(insn, value, ccset, INIT, B);		\
	FUNC(insn, value, ccset, INIT, C);		\
	FUNC(insn, value, ccset, INIT, D);		\
	FUNC(insn, value, ccset, INIT, E);		\
	FUNC(insn, value, ccset, INIT, F);		\
})




#define INSNVALCC(insn, value, ccset, FUNC)		\
({							\
	INSNVALCCINIT(insn, value, ccset, 0UL, FUNC);	\
	INSNVALCCINIT(insn, value, ccset, 0xffffffffffffffffUL, FUNC);	\
})

#define INSNVAL(insn, value, FUNC)			\
({							\
	INSNVALCC(insn, value, 0, FUNC);		\
	INSNVALCC(insn, value, 1, FUNC);		\
	INSNVALCC(insn, value, 2, FUNC);		\
	INSNVALCC(insn, value, 3, FUNC);		\
})
	
#define DO_INSN(insn, FUNC)				\
({							\
	INSNVAL(insn, 0UL, FUNC);			\
	INSNVAL(insn, 0xffffffffUL, FUNC);		\
	INSNVAL(insn, 0xffffffffffffffffUL, FUNC);	\
	INSNVAL(insn, 0xffffffff00000000UL, FUNC);	\
})

int main()
{
  	DO_INSN(LOC, LOAD_REG_MEM);
  	DO_INSN(LOCG, LOAD_REG_MEM);
	DO_INSN(LOCR, LOAD_REG_REG);
	DO_INSN(LOCGR, LOAD_REG_REG);
	DO_INSN(STOC, STORE_REG_REG);
	DO_INSN(STOCG, STORE_REG_REG);
	return 0;
}
