#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <unistd.h>

int main(int argc, char **argv)
{
  struct rlimit lim;
  int fd;
  
  getrlimit(RLIMIT_NOFILE, &lim);

  for ( fd = 3; fd < lim.rlim_cur; fd++ )
    close( fd );

  exit( 0 );
}
