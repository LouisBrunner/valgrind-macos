#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>

// Thorough syscall scalar arg checking.  Also serves as thorough checking
// for (very) basic syscall use.  Generally not trying to do anything
// meaningful with the syscalls.

#define GO(__NR_xxx, s) \
   fprintf(stderr, "-----------------------------------------------------\n"  \
                   "%3d:%20s %s\n"                                            \
                   "-----------------------------------------------------\n", \
                   __NR_xxx, #__NR_xxx, s);

#define SY  syscall


