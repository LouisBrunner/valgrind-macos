#include <stdlib.h>

static void *p;

int main ()
{
  p = malloc (1024);
  p = NULL;
}

