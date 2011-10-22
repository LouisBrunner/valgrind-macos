/* A very trivial test for undefinedness propagation through
   saturating narrowing.  Obviously need a much more thorough test.
*/
#include <stdio.h>
#include <assert.h>
#include "../../memcheck.h"
int main()
{
    unsigned char data[32], vbits[32];
    __asm__ __volatile__
       ("movdqu     (%0), %%xmm0 \n"
        "packuswb %%xmm0, %%xmm0 \n"
        "movdqu   %%xmm0, 16(%0) \n"
        ::"r"(data)
        :"memory","xmm0"
    );
    unsigned int res =
       VALGRIND_GET_VBITS( data, vbits, 32 );
    assert(res == 1); /* 1 == success */
    int i, j;
    for(i=0; i<2; i++) {
        for(j=0; j<16; j++)
            printf("%02x ", vbits[i*16+j]);
        printf("\n");
    }
    return 0;
}
