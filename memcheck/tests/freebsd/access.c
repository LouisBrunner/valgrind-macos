/*
 * Tests for various access functions
 *
 * access
 * eaccess
 * accessat
 */


#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

int main()
{
   if (-1 == access("access.c", R_OK))
   {
      perror("access failed:");
   }
 
   if (-1 == eaccess("access.c", F_OK))
   {
      perror("eaccess failed:");
   }
 
   if (-1 == faccessat(AT_FDCWD, "access.c", R_OK, AT_EACCESS))
   {
      perror("accessat failed:");
   }

   // error section
   int badint;
   char* badstring = strdup("foo");
   free(badstring);
   access(badstring, badint);
   eaccess(badstring, badint);
   faccessat(badint, badstring, badint, badint);

   exit(badint);
}

