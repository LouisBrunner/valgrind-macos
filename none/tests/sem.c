#define _GNU_SOURCE

#include <config.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <time.h>
#include <unistd.h>

void semctl_test (int trace, const char *fname)
{
  key_t key;
  int semid;
  int nr_of_readers;
  int ret;

  union semun {
     int              val;    /* Value for SETVAL */
     struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
     unsigned short  *array;  /* Array for GETALL, SETALL */
     struct seminfo  *__buf;  /* Buffer for IPC_INFO
                                 (Linux-specific) */
  } u;

  struct semid_ds ds;

  key = ftok (fname, 1);
  if (key == -1)
    perror ("ftok");
  nr_of_readers = 4;

  semid = semget (key, 2 * nr_of_readers, IPC_CREAT + 0660);
  if (semid == -1) {
    perror ("semget");
  }
  if (trace)
     printf("semid %d\n", semid);

  u.buf = &ds;
  ret = semctl (semid, 0, IPC_STAT, u);
  if (ret == -1)
     perror("semctl IPC_STAT");
  if (trace)
     printf("semid %d sem_nsems %d\n", semid, (int) ds.sem_nsems);

  {
    unsigned short semarray[2 * nr_of_readers];
    for (int count = 0; count < nr_of_readers; count++) {
        semarray[2 * count] = 0;
        semarray[2 * count + 1] = 1000;
      }
    ret = semctl (semid, 0, SETALL, semarray);
    if (ret == -1)
      perror ("semctl SETALL");
  }

  ret = semctl (semid, 0, IPC_RMID);
  if (ret == -1)
    perror ("semctl IPC_RMID");
}

int main(int argc, char **argv)
{
   int semid;
   struct sembuf sop;
#ifdef HAVE_SEMTIMEDOP
   struct timespec ts;
#endif
   
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

   if (semctl(semid, 0, GETVAL) != 1)
   {
      perror("semctl GETVAL");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   if (semctl(semid, 0, GETPID) != getpid())
   {
      perror("semctl GETPID");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

   /* The next call to semtimedop causes the program to hang on
      ppc32-linux (Yellow Dog 4.0).  I don't know why.  Hence the
      extended ifdef. */
#if defined(HAVE_SEMTIMEDOP) && !defined(__powerpc__)
   sop.sem_num = 0;
   sop.sem_op = 0;
   sop.sem_flg = 0;

   ts.tv_sec = 0;
   ts.tv_nsec = 1000000;
   
   if (semtimedop(semid, &sop, 1, &ts) < 0 && errno != EAGAIN)
   {
      perror("semtimedop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }
#endif

   sop.sem_num = 0;
   sop.sem_op = -1;
   sop.sem_flg = 0;
   
   if (semop(semid, &sop, 1) < 0)
   {
      perror("semop");
      semctl(semid, 0, IPC_RMID);
      exit(1);
   }

#ifdef HAVE_SEMTIMEDOP
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
#endif

   if (semctl(semid, 0, IPC_RMID) < 0)
   {
      perror("semctl(IPC_RMID)");
      exit(1);
   }

   semctl_test(argc > 1, argv[0]);
   exit(0);
}
