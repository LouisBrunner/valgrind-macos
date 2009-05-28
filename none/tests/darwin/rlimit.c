// Small test program to demonstrate Valgrind bug.
// https://bugs.kde.org/show_bug.cgi?id=191761
// Author: rohitrao@google.com
//
// Before the fix, it was printing 266.  Now it prints 256.

#include <stdio.h>
#include <sys/resource.h>

int main(void)
{
   struct rlimit rlp;
   getrlimit(RLIMIT_NOFILE, &rlp);
   fprintf(stderr, "RLIMIT_NOFILE is %lld\n", (long long)rlp.rlim_cur);
   return 0;
}

