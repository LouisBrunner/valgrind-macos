#include <stdio.h>
#include <assert.h>
#include <string.h>

char buffer[] = "0123456789abcdefXXXXX";
char target[] = "---------------------";
char overlap[]= "012345678901234567890";
char full[300];

int main(void)
{
   int i;

   /* Normal copy */
   printf("------- Copy 17 bytes from BUFFER to TARGET\n");
   printf("before: buffer = |%s|\n", buffer);
   printf("before: target = |%s|\n", target);
   asm volatile( "mvc 0(17,%0),0(%1)\n"
                 ::"a" (target),"a" (buffer): "memory");
   printf("after:  buffer = |%s|\n", buffer);
   printf("after:  target = |%s|\n", target);
   printf("\n");

   /* Destructive overlap #1 */
   printf("------- Destructive overlap #1\n");
   printf("before: |%s|\n", overlap);
   asm volatile( "mvc 1(17,%0),0(%1)\n"
                 ::"a" (overlap),"a" (overlap): "memory");
   printf("after:  |%s|\n", overlap);

   /* Destructive overlap #2 */
   printf("------- Destructive overlap #2\n");
   memset(target, '-', sizeof target - 1);  // restore initial state
   printf("before: target = |%s|\n", target);
   asm volatile( "mvi 0(%0),'x'\n\t"        // target[1] = 'x'
                 "mvc 1(2,%0),0(%0)\n\t"    // target[2:3] = target[1]
                 :: "a" (target+1));
   printf("after:  target = |%s|\n", target);

   /* Destructive overlap #3 */
   printf("------- Destructive overlap #3 (max length)\n");
   memset(full, '-', sizeof full);
   full[0] = 'x';
   asm volatile( "mvc 1(256,%0),0(%0)\n\t"     // full[1:256] = full[0]
                 :: "a" (full));
   /* Verify: the first 256+1 characters should be 'x' followed by '-' */
   for (i = 0; i <= 256; ++i)
      assert(full[i] == 'x');
   for ( ; i < sizeof full; ++i)
      assert(full[i] == '-');
   printf("\n");

   /* Non-destructive overlap */
   printf("------- Non-destructive overlap  buf[0:4] = buf[10:14]\n");
   char buf[] = "0123456789abcde";
   printf("before: buf = |%s|\n", buf);
   asm volatile( "mvc 0(5,%0),10(%1)\n"
                 ::"a" (buf),"a" (buf): "memory");
   printf("after:  buf = |%s|\n", buf);
   
   return 0;
}
