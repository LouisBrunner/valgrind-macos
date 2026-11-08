/* Copyright (c) 2026, Oracle and/or its affiliates. */
/* Check the scalar arguments of both Solaris socketpair syscall ABIs. */

#include "scalar.h"
#include "config.h"
#include "../../memcheck.h"
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   if (argc > 1) {
#if defined(SOLARIS_SOCKETPAIR_TAKES_FDS)
      return strcmp(argv[1], "--fds") != 0;
#else
      return strcmp(argv[1], "--legacy") != 0;
#endif
   }

   /* Keep the native value deterministic while testing undefined arguments. */
   x0 = 0;
   VALGRIND_MAKE_MEM_UNDEFINED(&x0, sizeof(x0));
#if defined(SOLARIS_SOCKETPAIR_TAKES_FDS)
   GO(SYS_so_socketpair, "2s 0m");
   SY(SYS_so_socketpair, x0 - 1, x0 - 1); FAILx(EBADF);
#else
   GO(SYS_so_socketpair, "1s 1m");
   SY(SYS_so_socketpair, x0); FAILx(EFAULT);
#endif
   return 0;
}
