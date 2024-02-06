#include <assert.h>
#include <aio.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
int x;

int main(void)
{
   #define LEN 10
   char buf[LEN];

   struct aiocb a;
   struct sigevent s;

   memset(&a, 0, sizeof(struct aiocb));
   // Not sure if the sigevent is even looked at by aio_*... just zero it.
   memset(&s, 0, sizeof(struct sigevent));

   a.aio_fildes     = -1;
   a.aio_offset     = 0;
   a.aio_buf        = NULL;
   a.aio_nbytes     = LEN;
   a.aio_reqprio    = 0;
   //a.aio_sigevent   = s;
   a.aio_lio_opcode = 0;   // ignored

   //------------------------------------------------------------------------
   // The cases where aiocbp itself points to bogus memory is handled in
   // memcheck/tests/darwin/scalar.c, so we don't check that here.

   //------------------------------------------------------------------------
   // XXX: This causes an unexpected undef value error later, at the XXX mark.
   //      Not sure why, it shouldn't.
   // assert( aio_return(&a) < 0);  // (iocb hasn't been inited)

   //------------------------------------------------------------------------
   assert( aio_read(&a) < 0);       // invalid fd

   //------------------------------------------------------------------------
   a.aio_fildes = open("aio.c", O_RDONLY);
   assert(a.aio_fildes >= 0);

   // unaddressable aio_buf
   //assert(aio_read(&a) == 0);
   //assert(aio_return(&a) == -1);

   //------------------------------------------------------------------------
   a.aio_buf = buf;

   assert( aio_read(&a) == 0 );

   // also failed on macOS
   // (don't crash on the repeated &a)
   // assert( aio_read(&a) < 0 );

   // undefined -- aio_return() not called yet
   if (buf[0] == buf[9]) x++;

   int try_count = 0;
   int res = aio_error(&a);
   while (0 != res && try_count < 1000) {
      ++try_count;
      struct timespec rq = { 0, 1000 };
      nanosleep(&rq, NULL);
      res = aio_error(&a);
   }

   assert(try_count < 1000);

   assert( aio_return(&a) > 0 );    // XXX: (undefined value error here)

   if (buf[0] == buf[9]) x++;

#if 0
   assert( aio_return(&a) < 0 );    // (repeated aio_return();  fails because 
                                    // Valgrind can't find &a in the table)
#endif

   //------------------------------------------------------------------------
   a.aio_buf    = 0;
   a.aio_fildes = creat("mytmpfile", S_IRUSR|S_IWUSR);
   assert(a.aio_fildes >= 0);

   // unaddressable aio_buf
   //assert( aio_write(&a) == 0);
   //assert(aio_return(&a) == -1);

   //------------------------------------------------------------------------
   a.aio_buf = buf;

   assert( aio_write(&a) == 0 );

   // (don't crash on the repeated &a)
   //assert( aio_write(&a) < 0 );

   try_count = 0;
   res = aio_error(&a);
   while (0 != res && try_count < 1000) {
      ++try_count;
      struct timespec rq = { 0, 1000 };
      nanosleep(&rq, NULL);
      res = aio_error(&a);
   }

   assert(try_count < 1000);

   assert( aio_return(&a) > 0 );

#if 0
   assert( aio_return(&a) < 0 );    // (repeated aio_return();  fails because 
                                    // Valgrind can't find &a in the table)
#endif

   unlink("mytmpfile");

   return x;
}
