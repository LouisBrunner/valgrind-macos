/* Reproduces bug 321960 (based on test from Daniel Stodden).
   At least on Ubuntu 12 and 13, causes invalid write errors
   in __yell or the memset call (due to some part of the main
   stack being marked as not addressable in memcheck).
   Bug seems extremely sensitive to initial conditions:
   Depending on the size of the env, bug is triggered or not.
   Also, a high nr of threads in thr[] is needed to get
   the problem. */
#include <pthread.h>
#if !defined(__FreeBSD__)
#include <alloca.h>
#endif
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h> 

void *
nop(void *nil)
{
    return NULL;
}

void
__yell(void)
{
    char buf[256];
    memset(buf, 0, sizeof(buf));
}

/* Without argument, executes once.
   Otherwise first arg indicates nr of times the process will exec
   itself, each time increasing the size of the environment
   by about 50 characters. */
int main(int argc, char **argv, char** envp)
{
    pthread_t thr[50];
    int i, err;

    for (i = 0; i < sizeof(thr) / sizeof(*thr); i++) {
        err = pthread_create(&thr[i], NULL, nop, NULL);
        assert(!err);
    }

    alloca(4096);
    __yell();

    for (i = 0; i < sizeof(thr) / sizeof(*thr); i++)
        pthread_join(thr[i], NULL);

    if ( argc == 2 && atoi(argv[1]) > 0) {
       /* exec ourselves with some more env */
       char** new_env;
       char more_env[100];
       char n[10];
       int j;

       sprintf(more_env, "N%d=ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ",  atoi(argv[1]));
       for (j = 0; envp[j]; j++)
          ;
       new_env = malloc((j+2) * sizeof(char*));
       assert (new_env != NULL);
       for (i = 0; i < j; i++)
          new_env[i] = envp[i];
       new_env[i++] = more_env;
       new_env[i++] = NULL;
       assert(i == j+2);
       sprintf (n, "%d",  atoi(argv[1]) - 1);
       // system ("env | wc");
       execle(argv[0], argv[0], n, (char *) NULL, new_env);
       assert(0);
    } else
       return 0;
}
