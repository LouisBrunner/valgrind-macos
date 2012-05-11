#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "valgrind.h"
#include "../memcheck.h"

struct super { int x; };
static struct super superblock = { 12345 };

/* run with `valgrind -q --malloc-fill=0xaf --free-fill=0xdb` */
int main(int argc, char **argv)
{
    unsigned char *s;
    VALGRIND_CREATE_MEMPOOL(&superblock, /*rzB=*/0, /*is_zeroed=*/0);
    s = malloc(40);
    assert(s);
    assert(*s == 0xaf);
    *s = 0x05;
    VALGRIND_MEMPOOL_ALLOC(&superblock, s, 40);
    printf("*s=%#hhx after MEMPOOL_ALLOC\n", *s);
    VALGRIND_MEMPOOL_FREE(&superblock, s);
    printf("*s=%#hhx after MEMPOOL_FREE\n", *s);
    VALGRIND_MEMPOOL_ALLOC(&superblock, s, 40);
    printf("*s=%#hhx after second MEMPOOL_ALLOC\n", *s);
    free(s);
    VALGRIND_DESTROY_MEMPOOL(&superblock);

    s = malloc(40);
    assert(s);
    assert(*s == 0xaf);
    *s = 0x05;
    VALGRIND_MALLOCLIKE_BLOCK(s, 40, 0/*rzB*/, 0/*is_zeroed*/);
    printf("*s=%#hhx after MALLOCLIKE_BLOCK\n", *s);
    VALGRIND_FREELIKE_BLOCK(s, 0/*rzB*/);
    printf("*s=%#hhx after FREELIKE_BLOCK\n", *s);
    VALGRIND_MALLOCLIKE_BLOCK(s, 40, 0/*rzB*/, 0/*is_zeroed*/);
    printf("*s=%#hhx after second MALLOCLIKE_BLOCK\n", *s);

    return 0;
}

