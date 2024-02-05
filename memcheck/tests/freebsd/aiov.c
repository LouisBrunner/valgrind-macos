#include <assert.h>
#include <aio.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
int x;

int main(void)
{
   #define LEN 10
   char buf1[LEN];
   char buf2[LEN];
   char buf3[LEN];
   struct iovec vec_array[] = {{buf1, LEN}, {buf2, LEN}, {buf3, LEN}};
   //struct iovec bad_array[] = {{NULL, LEN}, {buf2, LEN}, {buf3, LEN}};

   struct aiocb a;

   memset(&a, 0, sizeof(struct aiocb));

   a.aio_fildes     = -1;
   a.aio_offset     = 0;
   a.aio_iov        = NULL;
   a.aio_iovcnt     = 3;
   a.aio_reqprio    = 0;
   a.aio_lio_opcode = 0;   // ignored

   //------------------------------------------------------------------------
   // The cases where aiocbp itself points to bogus memory is handled in
   // memcheck/tests/darwin/scalar.c, so we don't check that here.

   //------------------------------------------------------------------------
   // XXX: This causes an unexpected undef value error later, at the XXX mark.
   //      Not sure why, it shouldn't.
   // assert( aio_return(&a) < 0);  // (iocb hasn't been inited)

   //------------------------------------------------------------------------
   assert( aio_readv(&a) < 0);       // invalid fd

   //------------------------------------------------------------------------
   a.aio_fildes = open("aio.c", O_RDONLY);
   assert(a.aio_fildes >= 0);

   // unaddressable aio_iov
   assert( aio_readv(&a) < 0);

   //------------------------------------------------------------------------
   //a.aio_iov = bad_array;
   // unaddressable first element in aio_iov
   // but it doesn't fail!
   //assert( aio_readv(&a) < 0 );
   
   a.aio_iov = vec_array;
   assert( aio_readv(&a) == 0 );

   // undefined -- aio_return() not called yet
   if (((char*)(vec_array[0].iov_base))[0] == ((char*)(vec_array[0].iov_base))[9]) x++;

   // also failed on macOS
   // (don't crash on the repeated &a)
   //assert( aio_readv(&a) == 0 );

   while (0 != aio_error(&a)) { }

   assert( aio_return(&a) > 0 );    // XXX: (undefined value error here)

   if (((char*)(vec_array[0].iov_base))[0] == ((char*)(vec_array[0].iov_base))[9]) x++;

   assert( aio_return(&a) < 0 );    // (repeated aio_return();  fails because 
                                    // Valgrind can't find &a in the table)

   //------------------------------------------------------------------------
   a.aio_iov    = 0;
   a.aio_fildes = creat("mytmpfile", S_IRUSR|S_IWUSR);
   assert(a.aio_fildes >= 0);

   // unaddressable aio_buf
   assert( aio_writev(&a) < 0);

   //------------------------------------------------------------------------
   a.aio_iov = vec_array;

   assert( aio_writev(&a) == 0 );

   // (don't crash on the repeated &a)
   // this failed on macOS
   //assert( aio_writev(&a)  < 0 );

   while (0 != aio_error(&a)) { };

   assert( aio_return(&a) > 0 );

   assert( aio_return(&a) < 0 );    // (repeated aio_return();  fails because 
                                    // Valgrind can't find &a in the table)

   unlink("mytmpfile");

   return x;
}
