#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/mman.h>

static void *mkmap(unsigned sz)
{
  int shmid = shmget(IPC_PRIVATE, sz, 
                     IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR);
  assert(shmid != -1);

  void *addr = shmat(shmid, NULL, 0);
  assert(addr != (void *)-1);

  return addr;
}

int main()
{
  void *np, *p;
	
  p  = mkmap(1024*1024);
  np = mremap(p, 1024*1024, 2048*1024, MREMAP_MAYMOVE); /* grow, maymove */
  assert(np != (void *)-1);

  return 0;
}
