/** Trigger barrier reinitialization, which is not allowed by the POSIX
 *  threads standard. See also http://www.opengroup.org/onlinepubs/000095399/functions/pthread_barrier_init.html.
 */


#define _GNU_SOURCE

#include <pthread.h>


int main(int argc, char** argv)
{
  pthread_barrier_t b;
  pthread_barrier_init(&b, 0, 1);
  pthread_barrier_init(&b, 0, 1);
  return 0;
}
