#include <stdio.h>
#include <unistd.h>

#define MAX 20000

int main () {
  int i;
  int inc_dec;
  int delta;
  int brk_stat;

  // loop to first increase, then decrease
  for (inc_dec = 1; inc_dec >= -1; inc_dec-=2) {
     // loop to increase(decrease) with small then big delta
     for (delta = 1; delta <= 400; delta+=399) {
        if (0) printf("initial brk value for inc_dec %d delta %d: %p\n",
               inc_dec, delta, sbrk(0));
        for (i=0; i<MAX; i++) {
           brk_stat = brk(sbrk(0) + inc_dec * delta);
           if (brk_stat == -1) {
              printf("brk value at failure: %p\n", sbrk(0));
              perror ("brk() failed!\n");
              return 0;
           }
        }
        if (0) printf("resulting brk value for inc_dec %d delta %d: %p\n",
               inc_dec, delta, sbrk(0));
     }
  }

  return 0;
} 
