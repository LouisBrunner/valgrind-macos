/* Make the value of PTHREAD_COND_INITIALIZER available to DRD. */

#include "drd_cond.h"
#include <pthread.h>

static pthread_cond_t pthread_cond_initializer = PTHREAD_COND_INITIALIZER;
Addr DRD_(pthread_cond_initializer) = (Addr)&pthread_cond_initializer;
int DRD_(pthread_cond_initializer_size) = sizeof(pthread_cond_initializer);
