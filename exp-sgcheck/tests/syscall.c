#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>

// For some reason, the stack frame below __GI_write is disappearing.
// Therefore, if I don't want the write errors to be merged, I have to
// ensure they have a different stack trace.  I do this by using this
// function.  Weird.
__attribute__((noinline))
void mywrite(char* buf, int len)
{
   write(-1, buf, len);
}

__attribute__((noinline))
void mygetitimer(long arg1, struct itimerval* itval)
{
   getitimer(arg1, itval);
}

__attribute__((noinline))
void myopen(char* name, long flags)
{
   open(name, flags);
}

int main(void)
{
   char *buf = malloc(sizeof(char)*6), *buf2 = malloc(sizeof(char)*6);
   struct itimerval* itval = malloc(sizeof(struct itimerval) - 1);
   int diff = buf2 - buf;
   buf[0] = 'h';
   buf[1] = 'e';
   buf[2] = 'l';
   buf[3] = 'l';
   buf[4] = 'o';
   buf[5] = 'x';

   // error (read)  (will fail due to -1, as we want -- don't want any
   // unpredictable output to foul up the test)
   mywrite(buf+3, 5);      // error (read)
   mywrite(buf-1, 5);      // error (read)
   mywrite(buf+1, diff);   // error (read)
   myopen(buf+3, 0x0);     // error (read_asciiz)

   mygetitimer(0, itval);    // error (write)

   //----
   free(buf);
   mywrite(buf,   5);      // error
   mywrite(buf+3, 5);      // error
   mywrite(buf+1, diff);   // error (read)

   return 0;
}
