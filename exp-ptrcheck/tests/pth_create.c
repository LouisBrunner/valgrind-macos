
#include <pthread.h>
#include <stdlib.h>



// This demonstrates an error for a pre_mem_{read,write} event that comes
// from the core, rather than a syscall (ie. part == Vg_CorePart instead of
// part == Vg_CoreSyscall).


int main(void)
{
   pthread_key_t* key  = malloc(sizeof(pthread_key_t));
   pthread_key_t* key2 = malloc(sizeof(pthread_key_t));

   pthread_key_create ( (pthread_key_t*)((long)key + 1), NULL );
   free(key2);
   pthread_key_create (                        key2    , NULL );

   return 0;
}
