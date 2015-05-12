#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/mman.h>

int main()
{
  int shmid = shmget(IPC_PRIVATE, 100 * 4096, 
                     IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR);
  assert(shmid != -1);

  void *addr = shmat(shmid, NULL, 0);
  assert(addr != (void *)-1);

  addr = mremap(addr, 100 * 4096, 400 * 4096, MREMAP_MAYMOVE);
  assert(addr != (void *)-1);

  return 0;
}
