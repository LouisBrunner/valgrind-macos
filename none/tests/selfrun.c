#include <stdio.h>
#include "valgrind.h"

int main()
{
	printf("RUNNING_ON_VALGRIND=%d\n", RUNNING_ON_VALGRIND);
	return 0;
}
