#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    const char *s = strdup("hello, world");
    int result;

    asm("movdqa (%1),%%xmm0\n"
        "pcmpistri $0x3a,%%xmm0,%%xmm0\n"
        : "=c" (result) : "r" (s) : "xmm0");

    free((void *)s);

    return result;
}
