/* Test program to verify whether DRD only complains about uninitialized
 * condition variables for dynamically allocated memory.
 */


#include <pthread.h>
#include <stdio.h>


static pthread_cond_t s_cond1 = PTHREAD_COND_INITIALIZER;
static pthread_cond_t s_cond2 = PTHREAD_COND_INITIALIZER;


int main(int argc, char** argv)
{
    fprintf(stderr, "Statically initialized condition variable.\n");

    pthread_cond_signal(&s_cond1);

    fprintf(stderr, "Uninitialized condition variable.\n");

    *((char*)&s_cond2 + sizeof(s_cond2) - 1) ^= 1;
    pthread_cond_signal(&s_cond2);

    fprintf(stderr, "Done.\n");

    return 0;
}
