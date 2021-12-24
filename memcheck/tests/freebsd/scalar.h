#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>

#define GO(syscall_num, param) \
   fprintf(stderr, "---------------------------------------------------------\n"  \
                   "%3d:%24s %s\n"                                                \
                   "---------------------------------------------------------\n", \
                   syscall_num, #syscall_num, param);

#define SY res = syscall

#define FAIL assert(res == -1);
#define SUCC assert(res != -1);
#define FAILx(E) \
   do { \
      int myerrno = errno; \
      if (res == -1) { \
         if (myerrno == E) { \
            /* as expected */ \
         } \
         else { \
            fprintf(stderr, "Expected error %s (%d), got %d\n", #E, E, myerrno); \
            exit(1); \
         } \
      } \
      else { \
         fprintf(stderr, "Expected error %s (%d), got success\n", #E, E); \
         exit(1); \
      } \
   } while (0);

/* Module variables. */
static long x0;
static long res;

