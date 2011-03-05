/* Check that Helgrind does not complain about semaphores with a
   nonzero initial value, when said semaphores are correctly used.
   Also useful for generating VCG of simple semaphore activity, for
   inspection. */

#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

#define N_THREADS 3

static sem_t* my_sem_init(char*, int, unsigned);
static int my_sem_destroy(sem_t*);
static int my_sem_wait(sem_t*); //static int my_sem_post(sem_t*);

void* child_fn ( void* semV ) {
   int r;
   sem_t* sem = (sem_t*)semV;
   r= my_sem_wait(sem); assert(!r);
   return NULL;
}

int main ( void )
{
   int r, i;
   sem_t* sem;
   pthread_t child[N_THREADS];

   sem= my_sem_init("sem1", 0, N_THREADS); assert(sem);

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_create( &child[i], NULL, child_fn, sem );
      assert(!r);
   }

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_join( child[i], NULL );
      assert(!r);
   }

   r= my_sem_destroy(sem); assert(!r);
   return 0;
}


static sem_t* my_sem_init (char* identity, int pshared, unsigned count)
{
   sem_t* s;

#if defined(VGO_linux)
   s = malloc(sizeof(*s));
   if (s) {
      if (sem_init(s, pshared, count) < 0) {
	 perror("sem_init");
	 free(s);
	 s = NULL;
      }
   }
#elif defined(VGO_darwin)
   char name[100];
   sprintf(name, "anonsem_%s_pid%d", identity, (int)getpid());
   name[ sizeof(name)-1 ] = 0;
   if (0) printf("name = %s\n", name);
   s = sem_open(name, O_CREAT | O_EXCL, 0600, count);
   if (s == SEM_FAILED) {
      perror("sem_open");
      s = NULL;
   }
#else
#  error "Unsupported OS"
#endif

   return s;
}

static int my_sem_destroy ( sem_t* s )
{
   return sem_destroy(s);
}

static int my_sem_wait(sem_t* s)
{
  return sem_wait(s);
}

#if 0
static int my_sem_post(sem_t* s)
{
  return sem_post(s);
}
#endif
