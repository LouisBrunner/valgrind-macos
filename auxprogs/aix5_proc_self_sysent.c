
/* A program which prints its own syscall name-to-number bindings.
   Used to generate the basis of include/vki/vki-scnums-aix5.h and
   coregrind/m_vkiscnums.c. */

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>

#include <sys/procfs.h>

#define NN 100000
char buf[NN];
int nbuf = 0;

int main ( void )
{
  int i;
  char name[50];
  sprintf(name, "/proc/%d/sysent", getpid());
  FILE* f = fopen(name, "r");
  assert(f);
  nbuf = fread(buf, 1, NN, f);
  assert(nbuf > 0 && nbuf <= NN);

  prsysent_t* header = (prsysent_t*)&buf[0];
  if (0) printf("Found %u syscalls\n\n", header->pr_nsyscalls);

  for (i = 0; i < header->pr_nsyscalls; i++) {
    printf("%3u  %s\n", header->pr_syscall[i].pr_number,
	   &buf[ header->pr_syscall[i].pr_nameoff ]);
  }

  fclose(f);
  return 0;
}
