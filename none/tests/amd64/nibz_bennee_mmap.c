
/* Test for aspacem bug reported by Alex Bennee, reported on users
   list around 9 Aug 06, resulting in

   > > --1515:0:aspacem  Valgrind: FATAL: aspacem assertion failed:
   > > --1515:0:aspacem    holeEnd <= aspacem_maxAddr
   > > --1515:0:aspacem    at m_aspacemgr/aspacemgr.c:1998
   > > (vgPlain_am_get_advisory)
   > > --1515:0:aspacem  Exiting now.

   TomH writes:

   > I think the problem here is that you've done an mmap (either fixed or
   > hinted) above aspacem_maxAddr and then munmaped it which leaves you
   > with a free segment above aspacem_maxAddr.

   The sequence seems to be that you have to allocate memory somewhere
   above aspacem_maxAddr, then free it, then do another hinted
   allocation that is above aspacem_maxAddr but which can't be done
   due to the memory already being in use. This program will reproduce
   it.

   On investigation: the problem was the munmap returns its space in
   the form of SkFree even though the space above aspacem_maxAddr is
   supposed to be SkResvn.  This is now fixed.
*/

#include <stdio.h>
#include <stdlib.h>
#include "tests/sys_mman.h"

int main(int argc, char **argv)
{
   void *p;
   if ((p = mmap((void*)0x1F7F100000, 4096, PROT_READ|PROT_WRITE,
                 MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0)) == (void *)-1)
   {
      perror("mmap");
      exit(1);
   }
   if (munmap(p, 4096) < 0)
   {
      perror("munmap");
      exit(1);
   }
   if ((p = mmap((void*)0x1F7F101000, 4096, PROT_READ,
                 MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)) == (void *)-1)
   {
      perror("mmap");
      exit(1);
   }
   if ((p = mmap((void*)0x1F7F101000, 4096, PROT_READ,
                 MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)) == (void *)-1)
   {
      perror("mmap");
      exit(1);
   }

   printf("made it through alive!\n");
   exit(0);
}
