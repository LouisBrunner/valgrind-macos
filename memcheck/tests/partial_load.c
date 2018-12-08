#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main ( void )
{
  long  w; int   i; char* p;
  assert(sizeof(long) == sizeof(void*));
#if defined(__powerpc64__)
  fprintf (stderr, "powerpc64\n"); /* Used to select correct .exp file.  */
#endif

  /* partial load, which --partial-loads-ok=yes should suppress */
  p = calloc( sizeof(long)-1, 1 );
  assert(p);
  w = *(long*)p;
  free(p);

  /* partial but misaligned, ppc64[le] ok, but otherwise cannot be suppressed */
  p = calloc( sizeof(long), 1 );
  assert(p);
  p++;
  w += *(long*)p;
  p--;
  free(p);

  /* partial but not word sized, cannot be suppressed */
  p = calloc( sizeof(short)-1, 1 );
  assert(p);
  w += (long)( *(short*)p );
  free(p);

  /* aligned, word sized, but completely invalid - cannot be suppressed */
  p = calloc( sizeof(long), 1 );
  assert(p);
  free(p);
  w += *(long*)p;

  /* dump result in a way gcc can't understand */
  for (i = 0; i < 64; i++)
     w <<= 1;

  return (int)w;
}

