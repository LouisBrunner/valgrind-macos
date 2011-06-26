#include <stdio.h>

static void another_func(char *msg)
{
   printf ("another func called msg %s\n", msg);
}

int main (int argc, char *argv[])
{
   printf("address of main %p\n", &main);
   printf("address of another_func %p\n", &another_func);
   another_func("called from main");
   return 0;
}
