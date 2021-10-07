// obtained from https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/rswctx.htm

#include <stdlib.h>
#include <stdio.h>
#include <ucontext.h>
#include <errno.h>

#define STACK_SIZE 2097152+16384 /* large enough value for AMODE 64 */

ucontext_t fcontext,mcontext;
int x = 0;

void func(int arg)
{
   printf("function called with value %d\n",arg);
   x++;
   printf("function returning to main\n");
   setcontext(&mcontext);
}

int main(void)
{
   int  value = 1;

   getcontext(&fcontext);
   if ((fcontext.uc_stack.ss_sp = (char *) malloc(STACK_SIZE)) != NULL)
   {
      fcontext.uc_stack.ss_size = STACK_SIZE;
      fcontext.uc_stack.ss_flags = 0;
      fcontext.uc_sigmask.__bits[3] = -1;
      errno = 0;
      makecontext(&fcontext,(void (*)(void))func,1,value);
      if (errno != 0)
      {
         perror("Error reported by makecontext()");
         return -1;    /* Error occurred exit */
      }
   }
   else
   {
      perror("not enough storage for stack");
      abort();
   }

   printf("context has been built\n");
   swapcontext(&mcontext,&fcontext);

   if (!x)
   {
     perror("incorrect return from swapcontext");
     abort();
   }
   else
   {
      printf("returned from function\n");
   }
   free(fcontext.uc_stack.ss_sp);
}


