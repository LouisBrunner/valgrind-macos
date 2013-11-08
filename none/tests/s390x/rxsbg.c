#include <stdio.h>
#include "opcodes.h"

#define DO_RXSBG(insn, _r1, _r2, i3, i4, i5)		\
({							\
	register unsigned long r1 asm ("1") = _r1;	\
	register unsigned long r2 asm ("2") = _r2;	\
	int cc;						\
	asm volatile(	insn(1,2, i3, i4, i5)		\
			"ipm %1\n"			\
			"srl %1,28\n"			\
			: "+d" (r1), "=d" (cc)		\
			: "d" (r1), "d" (r2)			\
			: "cc");			\
	printf(#insn " r1(==%16.16lX),r2(==%16.16lX),0x" #i3 ",0x" #i4 ",0x" #i5 " = %16.16lX (cc=%d)\n", _r1, _r2, r1, cc); \
})

#define r1sweep(i, r2, i3, i4, i5)				\
({								\
	DO_RXSBG(i, 000000000000000000ul, r2, i3, i4, i5);	\
	DO_RXSBG(i, 0x0000ffffccccaaaaul, r2, i3, i4, i5);	\
	DO_RXSBG(i, 0xfffffffffffffffful, r2, i3, i4, i5);	\
})

#define r2sweep(i, i3, i4, i5)				\
({							\
	r1sweep(i, 0x0000000000000000ul, i3, i4, i5);	\
	r1sweep(i, 0x5555ccccffff0000ul, i3, i4, i5);	\
	r1sweep(i, 0xfffffffffffffffful, i3, i4, i5);	\
})


/* min/max z=0/1 and some random number in the middle */
#define i3sweep(i, i4, i5)		\
({					\
	r2sweep(i, 00, i4, i5);		\
	r2sweep(i, 14, i4, i5);		\
	r2sweep(i, 3f, i4, i5);		\
	r2sweep(i, 80, i4, i5);		\
	r2sweep(i, a1, i4, i5);		\
	r2sweep(i, bf, i4, i5);		\
})

/* min/max t=0/1 and some random number in the middle */
#define i4sweep(i, i5)			\
({					\
	i3sweep(i, 00, i5);		\
	i3sweep(i, 2a, i5);		\
	i3sweep(i, 3f, i5);		\
	i3sweep(i, 80, i5);		\
	i3sweep(i, 9e, i5);		\
	i3sweep(i, bf, i5);		\
})

/* min/max and other shifts */
#define i5sweep(i)		\
({				\
	i4sweep(i, 00);		\
	i4sweep(i, 01);		\
	i4sweep(i, 13);		\
	i4sweep(i, 3e);		\
	i4sweep(i, 3f);		\
})






int main()
{
	i5sweep(RISBG);
	i5sweep(RNSBG);
	i5sweep(ROSBG);
	i5sweep(RXSBG);

	return 0;
}
