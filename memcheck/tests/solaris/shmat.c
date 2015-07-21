/* Tests simple manipulation with a shared memory segment. */

#include <stdio.h>
#include <sys/shm.h>

#define SEGMENT_SIZE 167

int main(int argc, const char *argv[])
{
   int shmid = shmget(IPC_PRIVATE, SEGMENT_SIZE, IPC_CREAT | SHM_R | SHM_W);
   if (shmid < 0) {
      perror("shmget()");
      return 1;
   }

   void *addr = shmat(shmid, NULL, 0);
   if (addr == (void *) -1) {
      perror("shmat()");
      return 2;
   }

   struct shmid_ds stats;
   int ret = shmctl(shmid, IPC_STAT, &stats);
   if (ret != 0) {
      perror("shmctl(IPC_STAT)");
      return 3;
   }   

   printf("segment size: %zu\n", stats.shm_segsz);

   ret = shmdt(addr);
   if (ret != 0) {
      perror("shmdt()");
      return 4;
   }

   ret = shmctl(shmid, IPC_RMID, NULL);
   if (ret != 0) {
      perror("shmctl(IPC_RMID)");
      return 5;
   }

   return 0;
}
