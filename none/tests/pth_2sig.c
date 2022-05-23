#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

void *slavethread(void *arg)
{
    while (1)
        pause();
}

int main(int argc, char **argv)
{
    const struct timespec alittle = { 0, 1000000000 / 100 };   // 0.01 seconds.
    int i;
    for (i = 0; i < 10; i++) {
        pthread_t slave;
        if (pthread_create(&slave, 0, slavethread, 0)) {
            perror("pthread_create");
            exit(2);
        }
    }

    pid_t pid = getpid();
    switch (fork()) {
        case 0: // child
            sleep(2); // Should be enough to ensure (some) threads are created
            for (i = 0; i < 20 && kill(pid, SIGTERM) == 0; i++)
                nanosleep(&alittle, NULL);
            exit(0);
        case -1:
            perror("fork");
            exit(4);
    }

    while (1)
        pause(); 
    fprintf(stderr, "strange, this program is supposed to be killed!\n");
    return 1;
}
