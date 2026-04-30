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
   int ret = readlinkat(99, "/proc/self/exe", buf, PATH_MAX);
   if (argc > 1) {
      printf("ret = %d, buf = %.64s\n", ret, buf);
   }
   char resolved[PATH_MAX];
   realpath(argv[0], resolved);
   assert(strcmp(resolved, buf) == 0);

   const size_t small_buf_size = 11;
   char small_buf[small_buf_size];
   memset(small_buf, '#', small_buf_size);
   ret = readlinkat(100, "/proc/self/exe", small_buf, 10);
   assert(strncmp(resolved, small_buf, 10) == 0);
   assert(small_buf[10] == '#');

   ret = readlinkat(101, "/proc/self/exe", (char*)1, 100);
   assert(ret == -1);
   assert(errno == EFAULT);
}

