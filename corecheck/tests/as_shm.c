#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>

int main()
{
	int shmid = shmget(IPC_PRIVATE, 0x10000, IPC_CREAT|IPC_EXCL|0777);
	void *addr;
	char local;
	char *top = (char *)(((unsigned long)&local + 0x0fffffff) & ~0x0fffffff);

	if (shmid == -1)
		perror("shmget");

	addr = shmat(shmid, 0, 0);

	if (addr == (void *)-1)
		perror("shmat @ 0");
	else
		printf("shmat 0: addr=...\n");

	addr = shmat(shmid, top, 0);

	if (addr == (void *)-1)
		perror("shmat @ top");
	else
		printf("shmat 2: addr=...\n");

	shmctl(shmid, IPC_RMID, NULL);

	return 0;
}
