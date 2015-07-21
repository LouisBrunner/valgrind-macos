/* Test whether /proc/{self,$PID}/path/a.out is correctly simulated. */

#include <limits.h>
#include <stdio.h>
#include <strings.h>
#include <unistd.h>
#include <sys/fcntl.h>

static void test_readlink(const char *cwd, const char *label,
      const char *path)
{
   char buf[PATH_MAX];
   int n;

   if ((n = readlink(path, buf, sizeof(buf) - 1)) >= 0) {
      const char *p;
      size_t len = strlen(cwd);

      buf[n] = '\0';

      p = buf;
      if (!strncmp(buf, cwd, len))
         p += len;
      printf("Result of readlink(\"%s\"): %s\n", label, p);
   }
   else
      perror("readlink");
}

static void test_readlinkat(const char *cwd, const char *label,
      const char *path)
{
   char buf[PATH_MAX];
   int n;

   if ((n = readlinkat(AT_FDCWD, path, buf, sizeof(buf) - 1)) >= 0) {
      const char *p;
      size_t len = strlen(cwd);

      buf[n] = '\0';

      p = buf;
      if (!strncmp(buf, cwd, len))
         p += len;
      printf("Result of readlinkat(\"%s\"): %s\n", label, p);
   }
   else
      perror("readlinkat");
}

int main(void)
{
   char cwd[PATH_MAX];
   char path[PATH_MAX];

   cwd[0] = '\0';
   if (!getcwd(cwd, sizeof(cwd) - 1)) /* '-1' to make room for '/' */
      perror("getcwd");
   strcat(cwd, "/");

   snprintf(path, sizeof(path), "/proc/%ld/path/a.out", (long)getpid());

   test_readlink(cwd, "/proc/self/path/a.out", "/proc/self/path/a.out");
   test_readlink(cwd, "/proc/<pid>/path/a.out", path);

   test_readlinkat(cwd, "/proc/self/path/a.out", "/proc/self/path/a.out");
   test_readlinkat(cwd, "/proc/<pid>/path/a.out", path);

   return 0;
}

