#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>

#include <sys/wait.h>
void *
slavethread(void *arg)
{
   char c;
   while (1)
      (void)read(0, &c, 1);
}

int main(int argc, char **argv)
{
    pthread_t slave;
    int i = 0;
    int pid = getpid();

    for (i = 0; i < 10; i++)
       if (pthread_create(&slave, 0, &slavethread, 0))
          {
             perror("slave2");
             exit(2);
          }
    switch (fork())
       {
       case 0: // child
          sleep(2); // Should be enough to ensure (some) threads are created
          for (i = 0; i < 20 && kill(pid, 2) == 0; i++)
             ;
          exit(0);
       case -1:
          perror("fork");
          exit(4);
       }

    while (rand() >= 0)
       i++;
    fprintf(stderr, "strange, this program is supposed to be killed !!!!\n");
    return 1;
}
