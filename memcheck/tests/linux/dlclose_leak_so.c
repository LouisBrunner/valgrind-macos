/* dlclose_leak_so.c */

#include <stdlib.h>

/** Makes a jump based on an uninitialized variable in order to make sure
 * errors reported while the dlopen'ed object is loaded work. */
int jmp_on_uninit(void) {
    int uninit[27];
    __asm__ __volatile("":::"cc","memory");
    if(uninit[13]) {
        return 1;
    } else {
        return 0;
    }
}

/** Leak 1 byte of memory. This is to test the stack check reported after the
 *  object has been dlclose'd. */
char* alloc_1_byte(void) {
    return (char*)malloc(1);
}
