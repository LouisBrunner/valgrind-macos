#include <stdio.h>

static char buf[10000];
static int bufi = 0;
void* malloc(size_t i) {
   bufi += i;
   return buf + bufi - i;
}

void free(void*ptr) {
}

int main (void)
{
   char *p;
   p = malloc(10);
   p = malloc(123);
   free(p);
   return 0;
}

