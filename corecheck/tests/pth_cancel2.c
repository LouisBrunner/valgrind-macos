/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * async_safe --
 *
 * Example showing macro wrappers for calling non-async
 * safe routines when the caller has asynchronous 
 * cancellation turned on
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <pthread.h>


#define async_cancel_safe_read(fd,buf,amt) \
   { \
      int oldtype; \
      pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &oldtype); \
      if (read(fd,buf,amt) < 0) \
         perror("read"),exit(1); \
      pthread_setcanceltype(oldtype,NULL); \
      pthread_testcancel(); \
   } 
   

#define async_cancel_safe_write(fd,buf,amt) \
   { \
      int oldtype; \
      pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &oldtype); \
      if (write(fd,buf,amt) < 0) \
         perror("write"), exit(1); \
      pthread_setcanceltype(oldtype,NULL); \
      pthread_testcancel(); \
   }


static int fd;
   
void *io(void *arg)
{
   int *fd2=(int *)arg; 
   char buf[20]="String";
   int amt=20;

   for (;;) {
      async_cancel_safe_write(*fd2,buf,amt);
      async_cancel_safe_read(*fd2,buf,amt);
   }
   return(NULL);
}

void *killer(void *arg)
{ 
   pthread_t * target = (pthread_t *)arg;
   sleep(1);
   pthread_cancel(*target);
   return(NULL);
}

extern int
main(void)
{
   pthread_t io_thread, killer_thread;   

   //   extern void *io(void *);
   // extern void *killer(void  *);

   if ((fd = open(".ktemp",O_CREAT | O_RDWR, 0666)) < 0)
      perror("open"), exit(1);

   pthread_create(&io_thread, 
		  NULL,
		  io,
		  (void *)&fd);
   pthread_create(&killer_thread,
		  NULL,
		  killer,
		  (void *)&io_thread);

   pthread_join(io_thread, NULL);

   pthread_join(killer_thread,NULL);

   if ((close(fd)) < 0)
     perror("close"),exit(1);
   if ((unlink(".ktemp")) < 0)
     perror("unlink"),exit(1);

   return 0;
}
