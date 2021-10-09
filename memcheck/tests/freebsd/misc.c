/*
 * Tests miscellaneous syscalls
 *
 * uuidgen
 * genrandom
 */

#include <sys/types.h>
#include <sys/uuid.h>
#include <sys/random.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../../config.h"
#include "../../memcheck.h"

int main(void)
{
   struct uuid s;
   if (-1 == uuidgen(&s, 1))
   {
      perror("uuidgen failed:");
   }

   struct uuid v[10];
   if (-1 == uuidgen(v, 10))
   {
      perror("uuidgen failed:");
   }
 
#if (FREEBSD_VERS >= FREEBSD_12)

   char buf[100];
   if (-1 == getrandom(buf, 100, GRND_NONBLOCK))
   {
      perror("getrandom failed:");
   }
 
#endif   
 
   // error section
   struct uuid* ps = malloc(2*sizeof(struct uuid));
   free(ps);
   uuidgen(ps, 2);

   int badint = 1;;
   VALGRIND_MAKE_MEM_UNDEFINED(&badint, sizeof(int));
   uuidgen(&s, badint);
 
#if (FREEBSD_VERS >= FREEBSD_12)

   badint = 100;
   VALGRIND_MAKE_MEM_UNDEFINED(&badint, sizeof(int));
   getrandom(buf, badint, badint);

   char* buf2 = malloc(100);
   free(buf2);
 
   getrandom(buf2, 100, 0);
 
#endif   
 
}

