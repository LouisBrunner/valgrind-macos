#include <stdlib.h>

int
main ()
{
  char *p = malloc (1024);
  for (int i = 3; i >= 0; i--)
    for (int j = 0; j <= 3; j++)
      {
	char *q = realloc (p, i * j * 512);
	p = q;
      }

  free (p);
}
