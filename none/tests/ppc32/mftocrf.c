
#include <stdio.h>
#include <config.h>

static
int try_mtocrf ( int x )
{
  int base = 0x31415927;
  int res;
#ifdef HAVE_AS_PPC_MFTOCRF
  /* pre-set CR */
  __asm__ __volatile__(
     "mtcr %0"
     : /*w*/ : /*r*/ "b"(base) : /*trash*/"cc" );

  /* do it */
  __asm__ __volatile__(
     "mtocrf 4, %0"
     : /*w*/ : /*r*/ "b"(x) : /*trash*/"cc" );

  /* get CR */
  __asm__ __volatile__(
     "mfcr %0"
     : /*w*/"=b"(res) : /*r*/ );
#else
  res = 42;
#endif
  return res;
}

static
int try_mfocrf ( int x ) 
{
   int res;
#ifdef HAVE_AS_PPC_MFTOCRF
   /* CR = x */
   __asm__ __volatile__(
     "mtcr %0"
     : /*w*/ : /*r*/ "b"(x) : /*trash*/"cc" );

  /* do it */
  __asm__ __volatile__(
     "li %0,0\n\t"
     "mfocrf %0,64"
     : /*w*/"=b"(res) : /*r*/ );
#else
  res = 42;
#endif
  return res;
}

/* This is a bit of a kludge since mfocrf reads the spec'd CR field,
   but the remaining returned bits are undefined.  It seems like on
   MPC7447A (Apple Mac Mini) mfocrf just reads the entire CR, which is
   an acceptable implementation, but is not necessarily what other
   implementations are going to do. */

int main ( void )
{
  int i, j;
  for (i = 0; i < 32; i++) {
    printf("0x%08x\n", try_mtocrf( 1<<i ));
  }
  printf("\n");
  j = 1;
  for (i = 0; i < 32; i++) {
    printf("0x%08x\n", try_mfocrf( j ));
    j *= 3;
  }

  return 0;
}
