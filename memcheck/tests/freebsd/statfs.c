#include <sys/param.h>
#include <sys/mount.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
   struct statfs fs;
   int fd = open("/bin/sh", O_RDONLY);

   // OK
   if (-1 == statfs("/bin/sh", &fs)) {
      perror("statfs failed:");
   }
   
   if (-1 == fstatfs(fd, &fs)) {
      perror("statfs failed:");
   }

   struct statfs* pfs;

   pfs = malloc(sizeof(struct statfs));
   free(pfs);

   // invalid write
   statfs("/bin/sh", pfs);
   pfs = malloc(sizeof(struct statfs));
   free(pfs);
   fstatfs(fd, pfs);

   pfs = malloc(sizeof(struct statfs) - 3);
   statfs("/bin/sh", pfs);

   char* badstring = strdup("/bin/sh");
   free(badstring);

   statfs(badstring, &fs);

   int badfd;
   fstatfs(badfd, &fs);

   free(pfs);
}

