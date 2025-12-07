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
#include "../../memcheck.h"

int main(void)
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
   ucontext_t* ucp = malloc(sizeof(*ucp));
   ucontext_t* ucp2 = malloc(sizeof(*ucp2));
   (void)VALGRIND_MAKE_MEM_NOACCESS(ucp, sizeof(*ucp));
   (void)VALGRIND_MAKE_MEM_NOACCESS(ucp2, sizeof(*ucp2));
   flag = 0;
   if (-1 == getcontext(ucp)) {
      perror("getcontext failed: ");
   }

   flag++;

   if (flag == 1) {
      (void)VALGRIND_MAKE_MEM_NOACCESS(ucp, sizeof(*ucp));
      if (-1 == setcontext(ucp)) {
         perror("setcontext failed: ");
      }
      fprintf(stderr, "should never see setcontext return\n");
   }

   flag++;

   if (flag == 3) {
      (void)VALGRIND_MAKE_MEM_NOACCESS(ucp, sizeof(*ucp));
      if (-1 == swapcontext(ucp2, ucp)) {
         perror("swapcontext failed: ");
      }
      fprintf(stderr, "should never see swapcontext return\n");
   }

   assert(flag == 5);
   free(ucp);
   free(ucp2);
}
