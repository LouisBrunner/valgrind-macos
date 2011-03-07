#include "test.h"
char buffer[24] ="0123456789abcdefghijklmn";
char *buflong = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.,}[]"
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRS%UVWXYZ0123456789_.,}[]";

static char * srst3(char *__next, char *__start, char __what, int *__cc)
{
	register unsigned long what asm ("0") = __what;
	register char *next asm ("2") = __next;
	register char *start asm ("4") = __start;
	int cc;

	asm volatile(	"0: srst 2,4\n"
			"jo 0b\n"
			"ipm %2\n"
			"srl %2,28\n"
			:"+d" (start), "+d" (next), "=d" (cc) :"d" (what): "cc");
	*__cc = cc;
	return next;
}

static char * srst2(char *__start, char __what,  int *__cc)
{
	register unsigned long what asm ("0") = __what;
	register char *start asm ("4") = __start;
	int cc;

	asm volatile(	"0: srst 0,4\n"
			"jo 0b\n"
			"ipm %2\n"
			"srl %2,28\n"
			:"+d" (start), "+d" (what), "=d" (cc) :: "cc");
	*__cc = cc;
	return (char *) what;
}

int main()
{
	char *buf;
	int cc;


	/* len=0 and start== next should not fault */
	srst3((char *)0x12345678,(char *)0x12345678,'0', &cc);
	printf("(cc=%d)\n", cc);

	buf = srst3(&buffer[23], &buffer[23], '0', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst3(&buffer[23], &buffer[0], '0', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst3(&buffer[23], &buffer[0], 'a', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst3(&buffer[23], &buffer[0], 'm', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst3(&buffer[23], &buffer[0], 'n', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst2(&buffer[0], '0', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst2(&buffer[0], 'a', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst2(&buffer[0], 'm', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst2(&buffer[0], 'n', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);

	buf = srst3(buflong + 469, buflong, '%', &cc);
	dump_field(buf, 1);
	printf("(cc=%d)\n", cc);
	return 0;
}

