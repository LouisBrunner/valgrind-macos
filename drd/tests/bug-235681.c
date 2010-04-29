/*
 * pthread_cond_wait() test program.
 * See also https://bugs.kde.org/show_bug.cgi?id=235681.
 */

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>

pthread_mutex_t mutex;
pthread_cond_t cond_var;
int status;
int silent;

static void *run_fn(void *v)
{
    int rc;

    if (!silent)
        fprintf(stderr, "run_fn starting\n");

    rc = pthread_mutex_lock(&mutex);
    assert(!rc);

    while (!status) {
        if (!silent)
            fprintf(stderr, "run_fn(): status==0\n");
        rc = pthread_cond_wait(&cond_var, &mutex);
        assert(!rc);
        if (!silent)
            fprintf(stderr, "run_fn(): woke up\n");
    }
    if (!silent)
        fprintf(stderr, "run_fn(): status==1\n");

    rc = pthread_mutex_unlock(&mutex);
    assert(!rc);

    if (!silent)
        fprintf(stderr, "run_fn done\n");

    return NULL;
}

int main(int argc, char **argv)
{
    int rc;
    pthread_t other_thread;

    if (argc > 1)
        silent = 1;

    rc = pthread_mutex_init(&mutex, NULL);
    assert(!rc);
    rc = pthread_cond_init(&cond_var, NULL);
    assert(!rc);

    status = 0;

    rc = pthread_create(&other_thread, NULL, run_fn, NULL);
    assert(!rc);

    /* yield the processor, and give the other thread a chance to get into the while loop */
    if (!silent)
        fprintf(stderr, "main(): sleeping...\n");
    sleep(1);

    rc = pthread_mutex_lock(&mutex);
    assert(!rc);
    /**** BEGIN CS *****/

    if (!silent)
        fprintf(stderr, "main(): status=1\n");
    status = 1;
    rc = pthread_cond_broadcast(&cond_var);
    assert(!rc);

    /**** END CS *****/
    rc = pthread_mutex_unlock(&mutex);
    assert(!rc);

    if (!silent)
        fprintf(stderr, "joining...\n");

    rc = pthread_join(other_thread, NULL);
    assert(!rc);

    fprintf(stderr, "Done.\n");

    return 0;
}
