#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

int main(int argc, char **argv)
{
  void *p1;
  void *p2;

  if ( ( p1 = mmap( 0, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 ) ) == MAP_FAILED )
    {
      perror( "mmap" );
      exit( 1 );
    }

  if ( munmap( p1, 4096 ) != 0 )
    {
      perror( "munmap" );
      exit( 1 );
    }

  if ( ( p2 = mmap( p1 + 1, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 ) ) == MAP_FAILED )
    {
      perror( "mmap" );
      exit( 1 );
    }

  if ( munmap( p2, 4096 ) != 0 )
    {
      perror( "munmap" );
      exit( 1 );
    }

  if ( ( p2 = mmap( p1 + 1, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0 ) ) == MAP_FAILED )
    {
      perror( "mmap" );
      exit( 1 );
    }

  if ( munmap( p2, 4096 ) != 0 )
    {
      perror( "munmap" );
      exit( 1 );
    }

  exit( 0 );
}
