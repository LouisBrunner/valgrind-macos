#include <stdio.h>
#include <stdlib.h>

#define LOAD_REG_MEM(insn, s, ccset, initial, mask)	\
({							\
	unsigned long target = initial;			\
	unsigned long source = s;			\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			#insn " %0, %2," #mask "\n"	\
			: "+d" (target), "+d" (a)	\
			: "Q" (source), "d" (b)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 mask, ccset, target);			\
})


#define LOAD_REG_REG(insn, s, ccset, initial, mask)	\
({							\
	unsigned long target = initial;			\
	unsigned long source = s;			\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			#insn " %0, %2," #mask "\n"	\
			: "+d" (target), "+d" (a)	\
			: "d" (source), "d" (b)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 mask, ccset, target);			\
})

#define STORE_REG_REG(insn, s, ccset, initial, mask)	\
({							\
	unsigned long target = initial;			\
	unsigned long source = s;			\
	unsigned int a,b;				\
	switch(ccset) {					\
	case 0: a = 0; b = 0; break;			\
	case 1: a = 1; b = 0; break;			\
	case 2: a = 0xffffffff; b = 1; break;		\
	case 3: a = 0xffffffff; b = 2; break;		\
	default: abort();				\
	}						\
	asm volatile(	"alr %1, %3\n"  /* set cc */	\
			#insn " %2, %0," #mask "\n"	\
			: "+Q" (target), "+d" (a)	\
			: "d" (source), "d" (b)		\
			: "cc");			\
	printf(#insn " %16.16lX into %16.16lX if mask"	\
		"%d for cc %d: %16.16lX\n",s, initial,	\
		 mask, ccset, target);			\
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
	FUNC(insn, value, ccset, INIT, 10);		\
	FUNC(insn, value, ccset, INIT, 11);		\
	FUNC(insn, value, ccset, INIT, 12);		\
	FUNC(insn, value, ccset, INIT, 13);		\
	FUNC(insn, value, ccset, INIT, 14);		\
	FUNC(insn, value, ccset, INIT, 15);		\
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
	DO_INSN(loc, LOAD_REG_MEM);
	DO_INSN(locg, LOAD_REG_MEM);
	DO_INSN(locr, LOAD_REG_REG);
	DO_INSN(locgr, LOAD_REG_REG);
	DO_INSN(stoc, STORE_REG_REG);
	DO_INSN(stocg, STORE_REG_REG);
	return 0;
}
