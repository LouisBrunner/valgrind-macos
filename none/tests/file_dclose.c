#define _GNU_SOURCE
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "fdleak.h"

static int
openfile (const char *f)
{
   return creat (f, O_RDWR);
}

static void
closefile (const char *f, int fd)
{
   close (fd);
   unlink (f);
}

int main ()
{
   CLOSE_INHERITED_FDS;

   const char *TMPFILE = "file_dclose.tmp";
   int fd;

   fd = openfile (TMPFILE);
   if (fd != -1) {
      fprintf(stderr, "close %d\n", fd);
      close (fd);
   }

   fprintf (stderr, "time passes and we close %d again\n", fd);
   closefile (TMPFILE, fd);

   return 0;
}
