
#include <stdlib.h>
#include <stdio.h>

char * touch_malloc (int size) 
{
  char * result;
  int i;
  result = malloc (size);
  for (i = 0; i < size; i++)
    *(result + i) = 'a';

  return result;
}
char * touch_realloc (char * ptr, int size) 
{
  char * result;
  int i;
  result = realloc (ptr, size);
  for (i = 0; i < size; i++)
    *(result + i) = 'a';

  return result;
}

int main ( void )
{
  char *a1, *b1, *a2 __attribute__((unused)), *b2 __attribute__((unused));
  printf("started\n");
  a1 = touch_malloc(1600000) ;
  b1 = touch_malloc(200000) ;
  a2 = touch_realloc(a1, 1601600) ;
  b2 = touch_realloc(b1, 200000) ;
  printf("success\n");
  return 0;
};
