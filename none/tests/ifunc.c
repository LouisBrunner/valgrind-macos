/* This test made valgrind run in an infinite loop. See bugzilla #301204 */
#include <stdio.h>

static void mytest(int d)
{
    printf("%d\n", d);
}

static void (*resolve_test(void))(int)
{
    return mytest;
}

void test(int d)
    __attribute__((ifunc("resolve_test")));

int main()
{
    test(5);
    return 0;
}
