#include <stdio.h>
#include <unistd.h>

#define MAX 3000

// At one time, this was causing a seg fault within Valgrind -- it was when
// extending the brk segment onto a new page.  Fixed in vg_syscalls.c 1.129.

int main () {
  char* ptr;
  int i;

  for (i=0; i<MAX; i++) {
    ptr = sbrk(1);

    if (ptr == (void*)-1) {
      printf ("sbrk() failed!\n");
      return 0;
    }

    *ptr = 0;
  }

  return 0;
} 
