// TO DO: replace the constants below by macro's #define'd during the configure
// phase.

#if defined(VGP_x86_linux)
# define PTHREAD_MUTEX_SIZE    24
# define PTHREAD_COND_SIZE     48
#elif defined(VGP_amd64_linux)
# define PTHREAD_MUTEX_SIZE    40
# define PTHREAD_COND_SIZE     48
#else
  /* FIXME: fill these fields in correctly.  32 is arbitrary. */
# define PTHREAD_MUTEX_SIZE    32
# define PTHREAD_COND_SIZE     32
#endif
#define PTHREAD_SPINLOCK_SIZE  4
