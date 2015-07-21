/* Test of correct simulation for uc->uc_link. */

#include <assert.h>
#include <stdio.h>
#include <ucontext.h>

static void print_value(int value)
{
   printf("Value is %d.\n", value);
}

int main(void)
{
   ucontext_t uc;
   char stack[8096];
   volatile int done = 0;

   /* Get current context. */
   getcontext(&uc);
   if (done) {
      /* Execution resumes here when print_value() returns. */
      return 0;
   }
   done = 1;

   /* Setup the stack. */
   uc.uc_stack.ss_sp = stack;
   uc.uc_stack.ss_size = sizeof(stack);

   /* Call print_value(). */
   makecontext(&uc, print_value, 1, 42);
   setcontext(&uc);

   /* This code should not be reached. */
   assert(0);
   return 0;
}

