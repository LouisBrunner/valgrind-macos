#define _GNU_SOURCE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>

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
