#ifndef _FDLEAK_H_
#define _FDLEAK_H_

#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>

#define DO(op) \
   ({ \
      long res = op; \
      if (res < 0) { \
         perror(#op); \
         exit(1); \
      }; \
      res; \
   })

#endif /* _FDLEAK_H_ */
