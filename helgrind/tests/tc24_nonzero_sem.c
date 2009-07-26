/* Check that Helgrind does not complain about semaphores with a
   nonzero initial value, when said semaphores are correctly used.
   Also useful for generating VCG of simple semaphore activity, for
   inspection. */
#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <assert.h>
#include <unistd.h>
#define N_THREADS 3
static int my_sem_init(sem_t*, char*, int, unsigned);
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
   sem_t sem;
   pthread_t child[N_THREADS];

   r= my_sem_init(&sem, "sem1", 0, N_THREADS); assert(!r);

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_create( &child[i], NULL, child_fn, (void*)&sem );
      assert(!r);
   }

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_join( child[i], NULL );
      assert(!r);
   }

   r= my_sem_destroy(&sem); assert(!r);
   return 0;
}


static int my_sem_init (sem_t* s, char* identity, int pshared, unsigned count)
{
#if defined(VGO_linux)
   return sem_init(s, pshared, count);
#elif defined(VGO_darwin)
   char name[100];
   sem_t** fakeptr = (sem_t**)s;
   assert(sizeof(sem_t) >= sizeof(sem_t*));
   { int i; for (i = 0; i < sizeof(name); i++) name[i] = 0; }
   sprintf(name, "anonsem_%s_pid%d", identity, (int)getpid());
   name[ sizeof(name)-1 ] = 0;
   if (0) printf("name = %s\n", name);
   *fakeptr = sem_open(name, O_CREAT, 0600, count);
   if (*fakeptr == (sem_t*)SEM_FAILED)
      return -1;
   else
      return 0;
#else
#  error "Unsupported OS"
#endif
}

static int my_sem_destroy ( sem_t* s )
{
#if defined(VGO_linux)
   return sem_destroy(s);
#elif defined(VGO_darwin)
   sem_t** fakeptr = (sem_t**)s;
   return sem_close(*fakeptr);
#else
#  error "Unsupported OS"
#endif
}

static int my_sem_wait(sem_t* s)
{
#if defined(VGO_linux)
  return sem_wait(s);
#elif defined(VGO_darwin)
  return sem_wait( *(sem_t**)s );
#else
#  error "Unsupported OS"
#endif
}

//static int my_sem_post(sem_t* s)
//{
//#if defined(VGO_linux)
//  return sem_post(s);
//#elif defined(VGO_darwin)
//  return sem_post( *(sem_t**)s );
//#else
//#  error "Unsupported OS"
//#endif
//}
