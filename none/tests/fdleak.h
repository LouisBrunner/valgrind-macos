#ifndef _FDLEAK_H_
#define _FDLEAK_H_

#include <stdlib.h>
#include <stdio.h>

#define DO(op) \
   ({ \
      long res = op; \
      if (res < 0) { \
         perror(#op); \
         exit(1); \
      }; \
      res; \
   })

/*
 * The macro below closes file descriptors inherited from the process
 * that forked the current process. Close these file descriptors right
 * after the start of main() in order to get consistent results across
 * different releases. Known behavior:
 * - Fedora Core 1's Perl opens /dev/pts/2 as fd 10.
 * - For Ubuntu 8.04, see also
 *   https://bugs.launchpad.net/ubuntu/+source/seahorse/+bug/235184
 */
#define CLOSE_INHERITED_FDS { int i; for (i = 3; i < 64; i++) close(i); }

#endif /* _FDLEAK_H_ */
