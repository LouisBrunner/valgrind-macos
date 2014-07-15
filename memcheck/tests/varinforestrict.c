// Simple program that uses C99 restrict qualifier.
// Once GCC is fixed to output DW_TAG_restrict_type in the debuginfo
// valgrind --read-var-info=yes would get a serious error reading the
// debuginfo. This tests makes sure that a fixed GCC and a fixed valgrind
// work well together.
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=59051
// https://bugs.kde.org/show_bug.cgi?id=336619

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "memcheck/memcheck.h"

/* Cause memcheck to complain about the address "a" and so to print
   its best guess as to what "a" actually is.  a must be addressible. */
void croak (void *aV )
{
  char* a = (char*)aV;
  char* undefp = malloc(1);
  char saved = *a;
  assert(undefp);
  *a = *undefp;
  (void) VALGRIND_CHECK_MEM_IS_DEFINED(a, 1);
  *a = saved;
  free(undefp);
}

void
bad_restrict_ptr (void * restrict bad_ptr)
{
  croak ((void *) &bad_ptr);
}

char *
cpy (char * restrict s1, const char * restrict s2, size_t n)
{
  char *t1 = s1;
  const char *t2 = s2;
  while(n-- > 0)
    *t1++ = *t2++;
  return s1;
}

int
main (int argc, char **argv)
{
  const char *hello = "World";
  size_t l = strlen (hello) + 1;
  char *earth = malloc (l);
  fprintf (stderr, "Hello %s\n", cpy (earth, hello, l));
  free (earth);

  void *bad = malloc (16);
  bad_restrict_ptr (bad);
  free (bad);
  return 0;
}
