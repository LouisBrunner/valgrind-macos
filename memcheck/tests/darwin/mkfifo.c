#include <stdio.h>
#include <stdlib.h>

#include <sys/syscall.h>
#include <unistd.h>
#include <fcntl.h>

#ifndef SYS_mkfifo
# define SYS_mkfifo 132
#endif

static char f_name[]="mkfifo_data_file";

int mkfifo(const char *path)
{
   return syscall(SYS_mkfifo, path);
}

int main(void)
{
   int fd;

   fd = mkfifo(f_name);

   if (fd == -1)
      perror("mkfifo"), exit(1);

   unlink(f_name);

   return 0;
}
