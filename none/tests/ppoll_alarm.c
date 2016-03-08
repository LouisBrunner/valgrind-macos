/* Tries to exploit bug in ppoll mask handling:
   https://bugs.kde.org/show_bug.cgi?id=359871
   where client program was able to successfully block VG_SIGVGKILL. */

#define _GNU_SOURCE /* for ppoll */
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

static int ready = 0;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

static void *
mythr(void *ignore)
{
    pthread_mutex_lock(&mutex);
    ready = 1;
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);

    sigset_t ss;
    sigfillset(&ss);
    while (1) {
        struct timespec ts = {10000, 0};
        ppoll(NULL, 0, &ts, &ss);
    }

    return NULL;
}

int
main()
{
    pthread_t thr;
    int ret = pthread_create(&thr, NULL, mythr, NULL);
    if (ret != 0) {
        fprintf(stderr, "pthread_create failed\n");
        return 1;
    }

    pthread_mutex_lock(&mutex);
    while (ready == 0) {
        pthread_cond_wait(&cond, &mutex);
    }
    pthread_mutex_unlock(&mutex);

    alarm(1); /* Unhandled SIGALRM should cause exit. */
    while (1)
        sleep(1);

    return 0;
}
