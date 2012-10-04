#include <elf.h>
#include <link.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

int
main (int argc, char **argv, char **envp)
{
  ElfW(auxv_t) auxv;
  ElfW(auxv_t) *auxv_p;

  void *entry0 = NULL;
  void *entry1 = NULL;

  char *platform0 = NULL;
  char *platform1 = NULL;

  // First try the "traditional" way.
  while (*envp++ != NULL)
    ; /* Skip, skip, skip... and after finding a NULL we have the auxv. */

  for (auxv_p = (ElfW(auxv_t) *) envp;
       auxv_p->a_type != AT_NULL;
       auxv_p++)
    {
      if (auxv_p->a_type == AT_ENTRY)
	entry0 = (void *) auxv_p->a_un.a_val;
      if (auxv_p->a_type == AT_PLATFORM)
	platform0 = strdup((char *) auxv_p->a_un.a_val);
    }

  // Now the /proc way as often used in libraries.
  int fd = open("/proc/self/auxv", O_RDONLY);
  if (fd == -1)
    return -1;

  while (read(fd, &auxv, sizeof(auxv)) == sizeof(auxv))
    {
      if (auxv.a_type == AT_ENTRY)
	entry1 = (void *) auxv.a_un.a_val;
      if (auxv.a_type == AT_PLATFORM)
	platform1 = strdup((char *) auxv.a_un.a_val);
    }
  close(fd);

  if (entry0 == entry1 && entry0 != NULL)
    fprintf(stderr, "entries OK\n");

  if (strcmp (platform0, platform1) == 0)
    fprintf(stderr, "platform OK\n");

  free (platform0);
  free (platform1);

  return 0;
}
