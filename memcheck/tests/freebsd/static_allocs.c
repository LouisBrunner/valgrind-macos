#include <stdio.h>

static char buf[10000];
static int bufi = 0;
void *realloc(void *ptr, size_t size) {
   if (!ptr) {
      bufi += size;
      return buf + bufi - size;
   } else {
      return NULL;
   }
}

void *calloc(size_t number, size_t size) {
   bufi += number*size;

   return buf + bufi - (number*size);
}


void free(void*ptr) {
}

void *reallocf(void *ptr, size_t size) {
   return ptr;
}

volatile char* nullptr = NULL;


int main (void)
{
   char *p;
   p = realloc((void*)nullptr, 10);
   p = calloc(10, 16);
   (void)reallocf(p, 0);
   return 0;
}

