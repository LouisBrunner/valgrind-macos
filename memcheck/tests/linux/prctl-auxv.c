#include <elf.h>
#include <errno.h>
#include <fcntl.h>
#include <link.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <unistd.h>

#ifndef PR_GET_AUXV
#define PR_GET_AUXV 0x41555856
#endif

static size_t auxv_size(const ElfW(auxv_t) *auxv)
{
   size_t n = 1;
   while (auxv[n - 1].a_type != AT_NULL)
      n++;
   return n * sizeof(*auxv);
}

static const char *result(long res)
{
   if (res != -1)
      return "success";
   return errno == EINVAL ? "EINVAL" : errno == EFAULT ? "EFAULT" : "other";
}

static const char *base_name(const char *path)
{
   const char *slash = strrchr(path, '/');
   return slash ? slash + 1 : path;
}

int main(int argc, char **argv, char **envp)
{
   long size = prctl(PR_GET_AUXV, NULL, 0UL, 0UL, 0UL);
   if (argc > 1)
      return size == -1;
   if (size <= 0) {
      perror("prctl(PR_GET_AUXV)");
      return 1;
   }

   ElfW(auxv_t) *auxv = malloc(size);
   printf("full copy returns the size: %s\n",
          prctl(PR_GET_AUXV, auxv, (unsigned long)size, 0UL, 0UL) == size
          ? "yes" : "no");
   size_t len = auxv_size(auxv);

   while (*envp++ != NULL)
      ;
   const ElfW(auxv_t) *stack_auxv = (const ElfW(auxv_t) *)envp;
   printf("matches the initial stack: %s\n",
          len == auxv_size(stack_auxv) && memcmp(auxv, stack_auxv, len) == 0
          ? "yes" : "no");

   static char proc_auxv[4096];
   ssize_t n, proc_len = 0;
   int fd = open("/proc/self/auxv", O_RDONLY);
   while (fd >= 0 && (n = read(fd, proc_auxv + proc_len,
                               sizeof(proc_auxv) - proc_len)) > 0)
      proc_len += n;
   close(fd);
   printf("matches /proc/self/auxv: %s\n",
          proc_len == len && memcmp(auxv, proc_auxv, len) == 0 ? "yes" : "no");

   const char *execfn = NULL;
   for (const ElfW(auxv_t) *p = auxv; p->a_type != AT_NULL; p++)
      if (p->a_type == AT_EXECFN)
         execfn = (const char *)p->a_un.a_val;
   printf("AT_EXECFN names this program: %s\n",
          execfn && strcmp(base_name(execfn), base_name(argv[0])) == 0
          ? "yes" : "no");

   size_t part_len = sizeof(*auxv) + 3;
   unsigned char *part = malloc(size);
   memset(part, 0xa5, size);
   printf("truncated copy: %s\n",
          prctl(PR_GET_AUXV, part, (unsigned long)part_len, 0UL, 0UL) == size
          && memcmp(part, auxv, part_len) == 0 && part[part_len] == 0xa5
          ? "ok" : "wrong");

   printf("arg4 != 0: %s\n",
          result(prctl(PR_GET_AUXV, auxv, (unsigned long)size, 1UL, 0UL)));
   printf("arg5 != 0: %s\n",
          result(prctl(PR_GET_AUXV, auxv, (unsigned long)size, 0UL, 1UL)));
   printf("NULL buffer: %s\n",
          result(prctl(PR_GET_AUXV, NULL, (unsigned long)size, 0UL, 0UL)));
   void *ro = mmap(NULL, size, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
   printf("read-only buffer: %s\n",
          result(prctl(PR_GET_AUXV, ro, (unsigned long)size, 0UL, 0UL)));

   munmap(ro, size);
   free(part);
   free(auxv);
   return 0;
}
