#include <stdlib.h>

typedef struct
{
  int tracking;
  unsigned long alignment;
} structure1;

void *fake_malloc(size_t length)
{
  structure1 *pointer1 = 0;

  pointer1 = malloc(sizeof(structure1) + length);
  if(pointer1)
  {
    pointer1->tracking = 3;
    pointer1++;
  }
  
  return pointer1;
}

void fake_free(void *pointer2)
{
  structure1 *pointer3 = (structure1 *)pointer2;

  if(pointer3)
  {
    pointer3--;
    free(pointer3);
  }

  return;
}

int main(int argc, char *argv[])
{
  void *pointer4 = 0;

  pointer4 = fake_malloc(50);
  fake_free(pointer4);

  return 0;
}                           /* No leaks. */
