#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#include "pub_tool_basics.h"
#include "vki/vki-scnums-darwin.h"
#include "pub_tool_vkiscnums.h"

// Since we use vki_unistd.h, we can't include <unistd.h>.  So we have to
// declare this ourselves.
extern int syscall (int __sysno, ...);

// Thorough syscall scalar arg checking.  Also serves as thorough checking
// for (very) basic syscall use.  Generally not trying to do anything
// meaningful with the syscalls.

#define GO(__NR_xxx, N, s) \
   fprintf(stderr, "-----------------------------------------------------\n"  \
                   "x%lx(%d):%20s %s\n"                                       \
                   "-----------------------------------------------------\n", \
                   (unsigned long)__NR_xxx, N, #__NR_xxx, s);

#define GO_UNIMP(n, s) \
   fprintf(stderr, "-----------------------------------------------------\n"  \
                   "%-17s%s\n"                                                \
                   "-----------------------------------------------------\n", \
                   "("#n"):     ", s);

#define SY(__NR_xxx, args...)    res = syscall(__NR_xxx, ##args);

#define FAIL  assert(-1 == res);
#define SUCC  assert(-1 != res);
#define SUCC_OR_FAIL    /* no test */

#define FAILx(E) \
   do { \
      int myerrno = errno; \
      if (-1 == res) { \
         if (E == myerrno) { \
            /* as expected */ \
         } else { \
         fprintf(stderr, "Expected error %s (%d), got %d\n", #E, E, myerrno); \
         exit(1); \
         } \
      } else { \
         fprintf(stderr, "Expected error %s (%d), got success\n", #E, E); \
         exit(1); \
      } \
   } while (0);

#define SUCC_OR_FAILx(E) \
   do { \
      int myerrno = errno; \
      if (-1 == res) { \
         if (E == myerrno) { \
            /* as expected */ \
         } else { \
         fprintf(stderr, "Expected error %s (%d), got %d\n", #E, E, myerrno); \
         exit(1); \
         } \
      } \
   } while (0);
