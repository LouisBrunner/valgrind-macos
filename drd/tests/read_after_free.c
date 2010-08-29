#define _GNU_SOURCE 1

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

static char* s_mem;
static volatile int s_freed;

static void* thread_func(void* arg)
{
    // Busy-wait until pthread_create() has finished.
    while (s_freed == 0)
        pthread_yield();
    free(s_mem);
    __sync_add_and_fetch(&s_freed, 1);
    return NULL;
}

int main(int argc, char** argv)
{
    pthread_t tid;
    int quiet;
    char result;

    quiet = argc > 1;

    s_mem = malloc(10);
    if (!quiet)
        fprintf(stderr, "Pointer to allocated memory: %p\n", s_mem);
    assert(s_mem);
    pthread_create(&tid, NULL, thread_func, NULL);
    __sync_add_and_fetch(&s_freed, 1);
    // Busy-wait until the memory has been freed.
    while (s_freed == 1)
        pthread_yield();
    // Read-after-free.
    result = s_mem[0];
    if (!quiet)
        fprintf(stderr, "Read-after-free result: %d\n", result);
    pthread_join(tid, NULL);
    fprintf(stderr, "Done.\n");
    return 0;
}
