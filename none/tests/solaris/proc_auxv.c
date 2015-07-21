/* Test if /proc/{self,$PID}/auxv is correctly simulated and that the aux
   vector contains plausible values. */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/auxv.h>
#include <sys/fcntl.h>

static int check_file(const char *path, auxv_t *auxv)
{
   auxv_t rauxv;
   int res = 1;
   FILE *fi;

   if (!(fi = fopen(path, "r"))) {
      perror("fopen");
      return 1;
   }
   while (1) {
      if (fread(&rauxv, sizeof(rauxv), 1, fi) != 1) {
         if (ferror(fi)) {
            perror("fread");
            goto out;
         }
         fprintf(stderr, "unexpected EOF\n");
         goto out;
      }
      if (memcmp(auxv, &rauxv, sizeof(rauxv))) {
         fprintf(stderr, "incorrect auxv in %s\n", path);
         fprintf(stderr, "expected: type=%d, val=%ld\n", auxv->a_type,
                 auxv->a_un.a_val);
         fprintf(stderr, "got: type=%d, val=%ld\n", rauxv.a_type,
                 rauxv.a_un.a_val);
         goto out;
      }

      if (auxv->a_type == AT_NULL)
         break;

      auxv++;
   }

   res = 0;

out:
   fclose(fi);
   return res;
}

int main(int argc, char *argv[], char *envp[])
{
   auxv_t *auxv;
   char buf[128];

   /* Find aux vector. */
   while (*envp)
      envp++;
   auxv = (auxv_t*)(envp + 1);

   /* /proc/self/auxv check */
   if (check_file("/proc/self/auxv", auxv))
      return 1;

   /* /proc/$PID/auxv check */
   snprintf(buf, sizeof(buf), "/proc/%ld/auxv", (long)getpid());
   if (check_file(buf, auxv))
      return 1;

   /* AT_SUN_EXECNAME check */
   while (auxv->a_type != AT_NULL) {
      if (auxv->a_type == AT_SUN_EXECNAME) {
         const char *execname = auxv->a_un.a_ptr;
         if (!execname) {
            fprintf(stderr, "AT_SUN_EXECNAME is null\n");
            return 1;
         }
         if (access(execname, R_OK | X_OK)) {
            fprintf(stderr, "AT_SUN_EXECNAME (%s) is invalid: %s\n",
                    execname, strerror(errno));
            return 1;
         }
         break;
      }
      auxv++;
   }

   return 0;
}

