#include <stdio.h>

union reg_pair {
	struct { unsigned long a, b; };
	unsigned __int128 pair;
};

#define MUL_REG_OP(insn, m1, fmt, m2)                                   \
({									\
	union reg_pair tmp = { { m1, m1 } };				\
	unsigned cc;							\
	asm ("xr %[cc],%[cc]\n"						\
	     insn("%[pair]", "%[op]")					\
	     "ipm %[cc]\n"						\
	     : [pair] "+d" (tmp.pair), [cc] "=&d" (cc)			\
	     : [op] fmt (m2)						\
	     : "cc");							\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%u)\n", \
	       m1, m2, tmp.a, tmp.b, cc >> 28);				\
})

#define MUL_REG_REG_REG(insn, m1, none, m2)                             \
({									\
	union reg_pair tmp = { { 0, 0 } };				\
	unsigned cc;							\
	asm ("xr %[cc],%[cc]\n"						\
	     insn("%[pair]", "%[a]", "%[b]")				\
	     "ipm %[cc]\n"						\
	     : [pair] "+d" (tmp.pair), [cc] "=&d" (cc)			\
	     : [a] "d" (m1), [b] "d" (m2)				\
	     : "cc");							\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", \
	       m1, m2, tmp.a, tmp.b, cc >> 28);				\
})

#define MUL_REG_XIMM(insn, m1, um2, m2)                                 \
({									\
	union reg_pair tmp = { { m1, m1 } };				\
	unsigned cc;							\
	asm ("xr %[cc],%[cc]\n"						\
	     insn("%[pair]", "0x" #m2)					\
	     "ipm %[cc]\n"						\
	     : [pair] "+d" (tmp.pair), [cc] "=&d" (cc)			\
	     :: "cc");							\
	printf(#insn " %16.16lX * %16.16lX = %16.16lX%16.16lX (cc=%d)\n", \
	       m1, (unsigned long) 0x##um2##m2, tmp.a, tmp.b, cc >> 28); \
})

#define sweep(f, i, x, m2)				\
({							\
	f(i, 0ul, x, m2);				\
	f(i, 1ul, x, m2);				\
	f(i, 0xfffful, x, m2);				\
	f(i, 0x7ffful, x, m2);				\
	f(i, 0x8000ul, x, m2);				\
	f(i, 0xfffffffful, x, m2);			\
	f(i, 0x80000000ul, x, m2);			\
	f(i, 0x7ffffffful, x, m2);			\
	f(i, 0xfffffffffffffffful, x, m2);		\
	f(i, 0x8000000000000000ul, x, m2);		\
	f(i, 0x7ffffffffffffffful, x, m2);		\
})

#define singlesweep(i, fmt, m2) sweep(MUL_REG_OP, i, fmt, m2)
#define regregsweep(i, m2) sweep(MUL_REG_REG_REG, i, , m2)
#define ximmsweep(i, um2, m2) sweep(MUL_REG_XIMM, i, um2, m2)

#define regsweep(i, m2) singlesweep(i, "d", m2)
#define memsweep(i, m2) singlesweep(i, "T", m2)
#define rmemsweep(i, m2) singlesweep(i, "R", m2)
#define immsweep(i, m2) singlesweep(i, "n", (unsigned long)m2)

#define for_each_m2(do_regmem_insns)            \
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
