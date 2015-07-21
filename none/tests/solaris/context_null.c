/* Test that setting NULL context causes the thread to exit. */

#include <assert.h>
#include <sys/syscall.h>
#include <ucontext.h>

int main(void)
{
   syscall(SYS_context, SETCONTEXT, NULL);

   /* This code should not be reached. */
   assert(0);
   return 0;
}

