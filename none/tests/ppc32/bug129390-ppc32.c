
#include <stdio.h>

int main ( void )
{
  printf("vmxcache: start\n");
  __asm__ __volatile__(
     "dss 0\n\t"
     "dss 1\n\t"
     "dss 2\n\t"
     "dss 3\n\t"
     "dssall\n\t"

     "dst 0,0,0\n\t"
     "dst 0,0,3\n\t"
     "dst 0,31,0\n\t"
     "dst 0,31,3\n\t"
     "dst 31,0,0\n\t"
     "dst 31,0,3\n\t"
     "dst 31,31,0\n\t"
     "dst 31,31,3\n\t"

     "dstt 0,0,0\n\t"
     "dstt 0,0,3\n\t"
     "dstt 0,31,0\n\t"
     "dstt 0,31,3\n\t"
     "dstt 31,0,0\n\t"
     "dstt 31,0,3\n\t"
     "dstt 31,31,0\n\t"
     "dstt 31,31,3\n\t"

     "dstst 0,0,0\n\t"
     "dstst 0,0,3\n\t"
     "dstst 0,31,0\n\t"
     "dstst 0,31,3\n\t"
     "dstst 31,0,0\n\t"
     "dstst 31,0,3\n\t"
     "dstst 31,31,0\n\t"
     "dstst 31,31,3\n\t"

     "dststt 0,0,0\n\t"
     "dststt 0,0,3\n\t"
     "dststt 0,31,0\n\t"
     "dststt 0,31,3\n\t"
     "dststt 31,0,0\n\t"
     "dststt 31,0,3\n\t"
     "dststt 31,31,0\n\t"
     "dststt 31,31,3\n\t"
  );
  printf("vmxcache: done\n");
  return 0;
}
