/*
 * Tests for various context functions
 *
 * getcontext
 * setcontext
 * swapcontext
 */

#include <ucontext.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <assert.h>

int main()
{
   ucontext_t uc;
   volatile int flag = 0;
   if (-1 == getcontext(&uc)) {
      perror("getcontext failed: ");
   }

   flag++;

   if (flag == 1) {
      if (-1 == setcontext(&uc)) {
         perror("setcontext failed: ");
      }
   }

   flag++;

   if (flag == 3) {
      ucontext_t uc2;
      if (-1 == swapcontext(&uc2, &uc)) {
         perror("swapcontext failed: ");
      }
   }

   assert(flag == 5);
 
   // error section
   ucontext_t* ucp = malloc(sizeof(ucontext_t));
   free(ucp);
   setcontext(ucp);
   swapcontext(ucp, ucp);
   getcontext(ucp);
}
