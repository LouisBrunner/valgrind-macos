#include <pthread.h>
int main(int argc, char *argv[])
{
   pthread_cond_t c;
   pthread_cond_init(& c, NULL);
   pthread_cond_destroy(& c);
   return 0;
} 
