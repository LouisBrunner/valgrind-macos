#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include "../../config.h"

int main(int argc, char** argv)
{
   char buf[PATH_MAX];
   memset(buf, 0, PATH_MAX);
#if defined(VGO_solaris)
   int ret = readlink("/proc/self/path/a.out", buf, PATH_MAX);
#else
   // Linux, and maybe one day NetBSD
   // other platforms excluded by .vgtest prereq
   int ret = readlink("/proc/self/exe", buf, PATH_MAX);
#endif
   if (argc > 1) {
      printf("ret = %d, buf = %.64s\n", ret, buf);
   }
   char resolved[PATH_MAX];
   realpath(argv[0], resolved);
   assert(strcmp(resolved, buf) == 0);

   const size_t small_buf_size = 11;
   char small_buf[small_buf_size];
   memset(small_buf, '#', small_buf_size);
#if defined(VGO_solaris)
   ret = readlink("/proc/self/path/a.out", small_buf, 10);
#else
   ret = readlink("/proc/self/exe", small_buf, 10);
#endif
   assert(strncmp(resolved, small_buf, 10) == 0);
   assert(small_buf[10] == '#');

#if defined(VGO_solaris)
   ret = readlink("/proc/self/path/a.out", (char*)1, 100);
#else
   ret = readlink("/proc/self/exe", (char*)1, 100);
#endif
   assert(ret == -1);
   assert(errno == EFAULT);
}

