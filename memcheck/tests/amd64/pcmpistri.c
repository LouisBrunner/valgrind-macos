#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    __attribute__((aligned(16))) char s[] = "hello, world";
    int result;

    asm("movdqa (%1),%%xmm0\n"
        "pcmpistri $0x3a,%%xmm0,%%xmm0\n"
        : "=c" (result) : "r" (s) : "xmm0");

    return result;
}
