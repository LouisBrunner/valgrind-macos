#define _GNU_SOURCE

#include <config.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#ifndef HAVE_SEMTIMEDOP

#include <signal.h>
#include <sys/time.h>

static int semtimedop(int  semid, struct sembuf *sops, unsigned nsops,
                      struct timespec *timeout)
{
   struct sigaction act;
   struct sigaction oldact;
   struct itimerval itv;
   int rv;

   act.sa_handler = SIG_IGN;
   sigemptyset( &act.sa_mask );
   act.sa_flags = 0;
   
   if (sigaction(SIGALRM, &act, &oldact) < 0)
   {
      perror("sigaction");
      exit(1);
   }
   
   itv.it_interval.tv_sec = 0;
   itv.it_interval.tv_usec = 0;
   itv.it_value.tv_sec = timeout->tv_sec;
   itv.it_value.tv_usec = timeout->tv_nsec / 1000;
   
   if (setitimer(ITIMER_REAL, &itv, NULL) < 0)
   {
      perror("setitimer");
      exit(1);
   }
   
   if ((rv = semop(semid, sops, nsops)) < 0 && errno == EINTR)
   {
      errno = EAGAIN;
   }

   if (sigaction(SIGALRM, &oldact, NULL) < 0)
   {
      perror("sigaction");
      exit(1);
   }
   
   return rv;
}

#endif

int main(int argc, char **argv)
{
   int semid;
   struct sembuf sop;
   struct timespec ts;
   
   if ((semid = semget(IPC_PRIVATE, 1, 0600)) < 0)
   {
      perror("semget");
      exit(1);
   }

   sop.sem_num = 0;
   sop.sem_op = 1;
   sop.sem_flg = 0;
   
   if (semop(semid, &sop, 1) < 0)
   {
      perror("semop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   sop.sem_num = 0;
   sop.sem_op = 0;
   sop.sem_flg = 0;

   ts.tv_sec = 0;
   ts.tv_nsec = 1000;
   
   if (semtimedop(semid, &sop, 1, &ts) < 0 && errno != EAGAIN)
   {
      perror("semtimedop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   sop.sem_num = 0;
   sop.sem_op = -1;
   sop.sem_flg = 0;
   
   if (semop(semid, &sop, 1) < 0)
   {
      perror("semop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   sop.sem_num = 0;
   sop.sem_op = 0;
   sop.sem_flg = 0;

   ts.tv_sec = 0;
   ts.tv_nsec = 1000;
   
   if (semtimedop(semid, &sop, 1, &ts) < 0)
   {
      perror("semtimedop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   if (semctl(semid, 0, IPC_RMID) < 0)
   {
      perror("semctl(IPC_RMID)");
      exit(1);
   }
         
   exit(0);
}
