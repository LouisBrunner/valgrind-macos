/*
 * Tests for various stat functions
 *
 * stat - tests fstatat with 1st arg AT_FDCWD
 * fstatat
 */


#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>

int main()
{
   struct stat sb;

   if (-1 == stat("stat.c", &sb))
   {
      perror("stat failed:");
   }

   assert(sb.st_size == 1161);

   int fd = openat(AT_FDCWD, "stat.c", O_RDONLY);
   if (-1 == fd)
   {
       perror("openat failed:");
   }

   if (-1 == fstat(fd, &sb))
   {
       perror("fstat failed:");
   }

    DIR* cwd = opendir(".");
    if (cwd)
    {
       int cwdfd = dirfd(cwd);
       fstatat(cwdfd, "stat.c", &sb, 0);
       closedir(cwd);
    }

   // error section
   char *badfilename = strdup("stat.c");
   free(badfilename);

   stat(badfilename, &sb);

   struct stat* badpsb = malloc(sizeof(struct stat));
   free(badpsb);

   stat("stat.c", badpsb);

   badpsb = malloc(sizeof(struct stat));
   free(badpsb);
   fstat(fd, badpsb);

   int badfd;
   fstat(badfd, &sb);

   fstatat(badfd, "stat.c", &sb, 0);
   fstatat(fd, "stat.c", &sb, badfd);

   close(fd);
}

