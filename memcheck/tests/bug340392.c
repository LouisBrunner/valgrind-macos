#include <stdlib.h>

typedef struct {
  unsigned char c;
  int i;
  void *foo;
} S;

S *make_s (void);

int
main (int argc, char **argv)
{
  S *s = make_s ();
  if (s->c == 0 && s->i == 1 && s->foo == getenv ("BLAH"))
    abort();
  return 0;
}

S *
make_s (void)
{
  S *res = malloc (sizeof (S));
  res->c = 1;
  return res;
}
