#include "tests/malloc.h"
#include "pub_core_basics.h"
#include <stdio.h>
#include <assert.h>

__attribute__((noinline))
static int my_ffsll ( ULong x )
{
   int i;
   for (i = 0; i < 64; i++) {
      if ((x & 1ULL) == 1ULL)
         break;
      x >>= 1;
   }
   return i+1;
}

/* Find length of string, assuming it is aligned and shorter than 8
   characters.  Little-endian only. */
__attribute__((noinline))
static int aligned_strlen(char *s)
{
    /* This is for 64-bit platforms */
    assert(sizeof(ULong) == 8);
    /* ..and only works for aligned input */
    assert(((unsigned long)s & 0x7) == 0);

    /* read 8 bytes */
    ULong val = *(ULong*)s;
    /* Subtract one from each byte */
    ULong val2 = val - 0x0101010101010101ULL;
    /* Find lowest byte whose high bit changed */
    val2 ^= val;
    val2 &= 0x8080808080808080ULL;

    return (my_ffsll(val2) / 8) - 1;
}

__attribute__((noinline)) void foo ( int x )
{
   __asm__ __volatile__("":::"memory");
}

int
main(int argc, char *argv[])
{
    char *buf = memalign16(5);
    buf[0] = 'a';
    buf[1] = 'b';
    buf[2] = 'c';
    buf[3] = 'd';
    buf[4] = '\0';

    /* --partial-loads-ok=no:  expect addr error (here) */
    /* --partial-loads-ok=yes: expect no error */
    if (aligned_strlen(buf) == 4)
        foo(44);

    /* --partial-loads-ok=no:  expect addr error (here) */
    /* --partial-loads-ok=yes: expect value error (in my_ffsll) */
    buf[4] = 'x';
    if (aligned_strlen(buf) == 0)
        foo(37);

    free(buf);

    /* Also, we need to check that a completely out-of-range,
       word-sized load gives an addressing error regardless of the
       start of --partial-loads-ok=.  *And* that the resulting
       value is completely defined. */
    RegWord* words = malloc(3 * sizeof(RegWord));
    free(words);

    /* Should ALWAYS give an addr error. */
    RegWord  w     = words[1];

    /* Should NEVER give an error (you might expect a value one, but no.) */
    if (w == 0x31415927) {
       fprintf(stderr,
               "Elvis is alive and well and living in Milton Keynes.\n");
    }

    return 0;
}
