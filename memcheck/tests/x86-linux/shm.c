#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include "../../memcheck.h"
#include "scalar.h"

int main(void)
{
    // uninitialised, but we know px[0] is 0x0
    long* px  = malloc(sizeof(long));
    long  x0  = px[0];
    long  res;

    GO(__NR_shmget, "3s 0m");
    SY(__NR_shmget, x0+IPC_PRIVATE, x0+1024, x0 | IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR); SUCC;

    long id = res;

    GO(__NR_shmat, "3s 0m");
    SY(__NR_shmat, x0+id, x0, x0); SUCC;

    void* mem = (void*)res;
    struct shmid_ds buf;
    VALGRIND_MAKE_MEM_NOACCESS(&buf, sizeof(buf));
    GO(__NR_shmctl, "3s 0m");
    SY(__NR_shmctl, x0+id, x0 | IPC_INFO, x0+&buf); SUCC;

    GO(__NR_shmdt, "1s 0m");
    SY(__NR_shmdt, x0+mem); SUCC;

    SY(__NR_shmctl, id, IPC_RMID, NULL);
}
