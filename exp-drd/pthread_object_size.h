// TO DO: replace the constants below by macro's #define'd during the configure
// phase.

#if defined(VGA_x86)
#define PTHREAD_MUTEX_SIZE    24
#define PTHREAD_COND_SIZE     48
#elif defined(VGA_amd64)
#define PTHREAD_MUTEX_SIZE    40
#define PTHREAD_COND_SIZE     48
#else
#error Unknown platform
#endif
#define PTHREAD_SPINLOCK_SIZE  4
