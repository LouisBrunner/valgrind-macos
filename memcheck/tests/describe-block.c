#include "../memcheck.h"

int main()
{
	VALGRIND_CREATE_BLOCK(0x1000, 0x1000, "magic foople zone");
	*(char *)0x1000 = 'x';
	return 0;
}
