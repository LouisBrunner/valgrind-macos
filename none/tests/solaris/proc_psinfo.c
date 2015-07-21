/*
 * Reads /proc/self/psinfo such that it can be tested whether Valgrind
 * intercepts the system calls that access this pseudo-file.
 */

#include <fcntl.h>
#include <limits.h>
#include <procfs.h>
#include <stdio.h>
#include <unistd.h>

static void test_psinfo(int op, const char *label,
                        const char *path)
{
   int fd;
   if (op == 0) {
      printf("open for %s:\n", label);

      fd = open(path, O_RDONLY); 
      if (fd < 0) {
         perror("open");
         return;
      }
   } else {
      printf("openat for %s:\n", label);

      fd = openat(AT_FDCWD, path, O_RDONLY);
      if (fd < 0) {
         perror("openat");
         return;
      }
   }

   psinfo_t psinfo;
   ssize_t bytes = read(fd, &psinfo, sizeof(psinfo));
   if (bytes != sizeof(psinfo)) {
      perror("read");
      return;
   }

   printf("fname: %s\n", psinfo.pr_fname);
   printf("psargs: %s\n", psinfo.pr_psargs);

   printf("argc: %d\n", psinfo.pr_argc);
   unsigned int i;
   char **argv = (char **) psinfo.pr_argv;
   for (i = 0; i < psinfo.pr_argc; i++) {
      printf("argv[%u]: %s\n", i, argv[i]);
   }

   close(fd);
}

int main(int argc, const char *argv[])
{
   char path[PATH_MAX];
   snprintf(path, sizeof(path), "/proc/%ld/psinfo", (long int) getpid());

   test_psinfo(0, "/proc/self/psinfo", "/proc/self/psinfo");
   printf("\n");
   test_psinfo(0, "/proc/<pid>/psinfo", path);
   printf("\n");

   test_psinfo(1, "/proc/self/psinfo", "/proc/self/psinfo");
   printf("\n");
   test_psinfo(1, "/proc/<pid>/psinfo", path);

   return 0;
}
