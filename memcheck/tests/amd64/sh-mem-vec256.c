
// Set up the 256-bit shadow memory test, by defining the
// required vector-copy function, and then including the
// template.

#define VECTOR_BYTES 32

static __attribute__((noinline))
void vector_copy ( void* dst, void* src )
{
  /* Note: Verions of GCC through 4.8.1 do not allow "ymm7" in the
     clobber list. (See http://stackoverflow.com/a/15767111/768469).
     Simulate it with "xmm7". */
  __asm__ __volatile__(
     "vmovupd (%1), %%ymm7 ; vmovupd %%ymm7, (%0)"
     : /*OUT*/ : /*IN*/ "r"(dst), "r"(src) : "memory","xmm7"
  );
}

// Include the test body, which refers to the above function
#include "../common/sh-mem-vec128.tmpl.c"
