
/* Test malloc, calloc, realloc and memalign set errno to ENOMEM */

#include <errno.h>
#include <limits.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>

int main ( void )
{
  char* small = malloc (16);
  char* p;

  errno = 0;
  p = malloc(SSIZE_MAX);
  if (!p && errno == ENOMEM) puts("malloc: Cannot allocate memory");

  errno = 0;
  p = calloc(1, SSIZE_MAX);
  if (!p && errno == ENOMEM) puts("calloc: Cannot allocate memory");

  errno = 0;
  p = realloc(small, SSIZE_MAX);
  if (!p && errno == ENOMEM) puts("realloc: Cannot allocate memory");

  errno = 0;
  p = memalign(64, SSIZE_MAX);
  if (!p && errno == ENOMEM) puts("memalign: Cannot allocate memory");

  free(small);

  return 0;
}
