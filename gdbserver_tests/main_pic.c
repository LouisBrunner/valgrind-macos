#include <inttypes.h>
#include <stdio.h>

static void another_func(char *msg)
{
   printf ("another func called msg %s\n", msg);
}

int main (int argc, char *argv[])
{
   printf("address of main %#" PRIxPTR "\n", (uintptr_t) main);
   printf("address of another_func %#" PRIxPTR "\n", (uintptr_t) another_func);
   another_func("called from main");
   return 0;
}
