#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <inttypes.h>
#include "opcodes.h"

uint64_t
ecag(int ai, int li, int ti)
{
   register uint64_t result asm("2") = 0;
   register uint64_t input  asm("3") = (ai << 4) | (li << 1) | ti;

   asm volatile( ECAG(2,0,3,000,00)
                 : "=d" (result) : "d" (input));
   return result;
}

static unsigned
get_level_info(uint64_t topology, unsigned level)
{
   return (topology >> (56 - level * 8)) & 0xff;
}

int 
main(void)
{
   unsigned level;
   uint64_t topology;

   topology = ecag(0, 0, 0);   // get summary

   /* ECAG supports at most 8 levels of cache. Iterate over all of them
      ignoring those not present. */
   for (level = 0; level < 8; level++) {
      unsigned info = get_level_info(topology, level);

      if ((info & 0xc) == 0) continue;  // cache does not exist at this level

      unsigned cache_type  =  info & 0x3;
      unsigned cache_scope = (info & 0xc) >> 2;
      char *type, *scope;

      switch (cache_type) {
      case 0: type = "separate data and instruction"; break;
      case 1: type = "instruction"; break;
      case 2: type = "data"; break;
      case 3: type = "unified data and instruction"; break;
      }

      switch (cache_scope) {
      case 0: assert(0);  // should never occur because cache exists
      case 1: scope = "private";  break;
      case 2: scope = "shared";   break;
      case 3: scope = "reserved"; break;
      }

      printf("L%u topology: %s; %s\n", level+1, type, scope);
      printf("L%u cache line size data: %"PRId64"\n", level+1,
             ecag(1, level, 0));
      printf("L%u cache line size insn: %"PRId64"\n", level+1,
             ecag(1, level, 1));
      printf("L%u total cachesize data: %"PRId64"\n", level+1,
             ecag(2, level, 0));
      printf("L%u total cachesize insn: %"PRId64"\n", level+1,
             ecag(2, level, 1));
      printf("L%u set. assoc.     data: %"PRId64"\n", level+1,
             ecag(3, level, 0));
      printf("L%u set. assoc.     insn: %"PRId64"\n", level+1,
             ecag(3, level, 1));
   }
   
   return 0;
}
