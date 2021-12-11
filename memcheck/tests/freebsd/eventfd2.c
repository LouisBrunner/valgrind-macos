/* EFD_SEMAPHORE */

#include <sys/eventfd.h>
#include <sys/wait.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static void xsem_wait(int fd)
{
   eventfd_t cntr;

   if (eventfd_read(fd, &cntr) != 0) {
      perror("reading eventfd");
      exit(1);
   }

   fprintf(stdout, "wait completed on %d: count=%" PRIu64 "\n",
           fd, cntr);
}

static void xsem_post(int fd, int count)
{
   eventfd_t cntr = count;

   if (eventfd_write(fd, cntr) != 0) {
      perror("writing eventfd");
      exit(1);
   }
}

static void sem_player(int fd1, int fd2)
{
   /* these printfs did contain the pid
    * so "[%u] ... ", getpid()
    * not good for regresson tests
    * (also xsem_wait above)
    */
   fprintf(stdout, "posting 1 on %d\n", fd1);
   xsem_post(fd1, 1);

   fprintf(stdout, "waiting on %d\n", fd2);
   xsem_wait(fd2);

   fprintf(stdout, "posting 1 on %d\n", fd1);
   xsem_post(fd1, 1);

   fprintf(stdout, "waiting on %d\n", fd2);
   xsem_wait(fd2);

   fprintf(stdout, "posting 5 on %d\n", fd1);
   xsem_post(fd1, 5);

   fprintf(stdout, "waiting 5 times on %d\n", fd2);
   xsem_wait(fd2);
   xsem_wait(fd2);
   xsem_wait(fd2);
   xsem_wait(fd2);
   xsem_wait(fd2);
}

static void usage(char const *prg)
{
   fprintf(stderr, "use: %s [-h]\n", prg);
}

int main(int argc, char **argv)
{
   int c, fd1, fd2, status;
   pid_t cpid_poster, cpid_waiter;

   while ((c = getopt(argc, argv, "h")) != -1) {

      switch (c) {
      default:
         usage(argv[0]);
         return 1;
      }
   }

   if ((fd1 = eventfd(0, EFD_SEMAPHORE)) == -1 ||
       (fd2 = eventfd(0, EFD_SEMAPHORE)) == -1) {
      perror("eventfd");
      return 1;
   }
   if ((cpid_poster = fork()) == 0) {
      sem_player(fd1, fd2);
      exit(0);
   }
   if ((cpid_waiter = fork()) == 0) {
      sem_player(fd2, fd1);
      exit(0);
   }
   waitpid(cpid_poster, &status, 0);
   waitpid(cpid_waiter, &status, 0);

   exit(0);
}
