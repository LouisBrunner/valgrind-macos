// This test-case exposes a bug that was present in the compressed V bit
// handling for a while.  The problem was that when
// copy_address_range_state() copied a VA_BITS2_OTHER value, it failed to
// also copy the corresponding entry in the sec-V-bits table.  Then later on
// when we searched for the sec-V-bits entry for the copied-to location, it
// failed to find it:
//
//   Memcheck: mc_main.c:766 (get_sec_vbits8): Assertion 'n' failed.
//   Memcheck: get_sec_vbits8: no node for address 0x4017440 (0x4017441)

#include <stdlib.h>

int main(void)
{
   int i, t = 0;
   char* x = malloc(1000);

   // Write some PDBs (partially defined bytes)
   for (i = 0; i < 1000; i++)
      x[i] &= (i & 0xff);

   // realloc them, invoking copy_address_range_state()
   x = realloc(x, 10000);

   // Read the PDBs -- this caused a sec-V-bits lookup failure.
   for (i = 0; i < 1000; i++)
      t += x[i];
   
   __asm__ __volatile__ ("" :: "r"(t));

   return 0;
}

