#include <stdio.h>
#include <stdlib.h>

#define _GNU_SOURCE
#include <string.h>

int main(int argc, char **argv)
{
  char a[] = "The spazzy orange tiger jumped over the tawny jaguar.";
  char *b, *c;
  char *d, *e;

  size_t l = strlen (a);
  fprintf (stderr, "strlen: %zd\n", l); // strlen: 53

  b = (char *) malloc((l + 3)); // Extra space for some zeros.
  b[l] = 'X';
  b[l + 1] = 'X';
  b[l + 2] = 'X';
  c = stpncpy (b, a, l + 3);

  fprintf (stderr, "equal: %d\n", strcmp (a, b)); // equal: 0
  fprintf (stderr, "retlen: %zd\n", c - b); // retlen: 53
  fprintf (stderr, "last: '%c'\n", *(c - 1)); // last: '.'
  fprintf (stderr, "zero0: %d\n", *c); // zero0: 0
  fprintf (stderr, "zero1: %d\n", *(c + 1)); // zero1: 0
  fprintf (stderr, "zero2: %d\n", *(c + 2)); // zero2: 0

  d = (char *) malloc (l - 1); // No room for zero termination or dot.
  e = stpncpy (d, b, l - 1);

  fprintf (stderr, "equal: %d\n", strncmp (b, d, l - 1)); // equal: 0
  fprintf (stderr, "retlen: %zd\n", e - d); // retlen: 52
  fprintf (stderr, "last: '%c'\n", *(e - 1)); // last: 'r'

  free (b);
  free (d);
  return 0;
}
