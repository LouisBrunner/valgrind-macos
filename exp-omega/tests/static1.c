#include <stdlib.h>

static char *pointer1 = NULL;

int main(int argc, char *argv[])
{
  pointer1 = malloc(64);

  return 0;
}                              /* No leak. */
