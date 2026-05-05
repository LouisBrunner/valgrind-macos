#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

static void handler(int sig) { (void)sig; }

int main(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = handler;
    sigaction(SIGUSR1, &sa, NULL);   /* no SA_RESTART */

    /* Child will interrupt us */
    if (fork() == 0) {
        usleep(100000);  /* 100ms */
        kill(getppid(), SIGUSR1);
        _exit(0);
    }

    struct timespec req = { .tv_sec = 2, .tv_nsec = 0 };
    struct timespec rem;

    int ret = clock_nanosleep(CLOCK_MONOTONIC, 0, &req, &rem);

    if (ret != EINTR)
    {
        printf("Unexpected return: %d\n", ret);
        return 1;
    }

    /* Force full struct read */
    volatile long sec  = rem.tv_sec;
    volatile long nsec = rem.tv_nsec;

    /* Trigger uninit read error if rem was not marked as initialised */
    if (sec || nsec)
    {
        return 1;
    }

    return 0;
}

