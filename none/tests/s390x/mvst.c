#include "test.h"

char buffer[23] ="0123456789abcdef\0XXXXX";
char bigbuf[512]=
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde\0";

char target[512];

int mvst(void *targetp, void *source)
{
	register int eos asm("0") = '\0';
	asm volatile(
		"	mvst %0, %1\n"
		: "+a"(targetp), "+a"(source) : "d"(eos) : "memory", "cc");
	return get_cc();
}

int mvst_full(void *targetp, void *source)
{
	register int eos asm("0") = '\0';
	asm volatile(
		"0:	mvst %0, %1\n"
		"	jo 0b\n"
		: "+a"(targetp), "+a"(source) : "d"(eos) : "memory", "cc");
	return get_cc();
}


int main()
{
	short t;
	char s;
	printf("CC:%d\n", mvst(target, buffer));
	printf("%s\n", target);
	printf("CC:%d\n",mvst_full(target, bigbuf));
	printf("%s\n", target);
	t = 0x6161;
	s = 0;
	printf("%s\n", (char *) &t);
	printf("CC:%d\n",mvst(&t,&s));
	printf("%s\n", (char *) &t);
	return 0;
}

