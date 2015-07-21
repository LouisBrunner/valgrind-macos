/* Maps several pages with or without MAP_NORESEVE.
   Mappings with MAP_NORESEVE do not show in /proc/self/xmap
   (only in /proc/self/rmap) until they actually materialize.
   Very nice from Solaris kernel :-(
 */

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wait.h>
#include <sys/mman.h>
#include <sys/param.h>

static void *do_map(int flags)
{
   flags |= MAP_PRIVATE | MAP_ANON;
   void *addr = mmap(0, PAGESIZE, PROT_READ | PROT_WRITE, flags, -1, 0);
   if (addr == NULL) {
      perror("mmap");
      exit(1);
   } else {
      return addr;
   }
}

static void *do_dlopen(const char *pathname)
{
   int mode = RTLD_LAZY | RTLD_LOCAL;
   void *handle = dlopen(pathname, mode);
   if (handle == NULL) {
      fprintf(stderr, "dlopen failed for %s: %s",
              pathname, dlerror());
      exit(1);
   } else {
      return handle;
   }
}

int main(int argc, const char *argv[])
{
   do_map(MAP_NORESERVE);
   do_dlopen("libm.so");
   do_map(0);
   do_map(0);
   do_map(MAP_NORESERVE);
   do_dlopen("liby.so");
   do_map(MAP_NORESERVE);
   do_map(0);
   do_map(0);
   do_map(MAP_NORESERVE);
   do_map(MAP_NORESERVE);
   do_dlopen("libz.so");
   do_map(MAP_NORESERVE);
   do_map(MAP_NORESERVE);
   do_map(0);

   pid_t pid = fork();
   if (pid == -1) {
      perror("fork");
      exit(1);
   }

   if (pid == 0) {
      do_map(MAP_NORESERVE);
      do_map(0);
      do_map(0);
      do_dlopen("libw.so");
      do_map(0);
      do_map(MAP_NORESERVE);
      do_map(MAP_NORESERVE);
      do_map(0);
      printf("CHILD: PASSED\n");
      fflush(stdout);
      return 0;
   }

   int status;
   if (waitpid(pid, &status, 0) != pid) {
      perror("waitpid");
   } else if ((WIFEXITED(status) != 0) && (WEXITSTATUS(status) == 0)) {
      printf("PASSED\n");
   } else {
      fprintf(stderr, "FAILED: child exited with unexpected status %s %d\n",
              WIFEXITED(status) ? "exit" : "signal", 
              WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
   }

   return 0;
}
