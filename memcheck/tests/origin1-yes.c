
/* This test case was originally written by Nicholas Nethercote. */

// This test covers all the different sources of values, both defined and
// undefined.  It only involves undefined condition errors.
//
// Nb: a stack frame is allocated when a signal is delivered.  But it
// immediately get written with stuff, so there's no significant possibility
// of undefined values originating there.  So we ignore it.  (On platforms
// like AMD64 that have a redzone just beyond the stack pointer there is a
// possibility, but it's so slim we ignore it.)

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "tests/sys_mman.h"
#include <unistd.h>
#include "../memcheck.h"

int x = 0;

int main(void)
{
   assert(1 == sizeof(char));
   assert(2 == sizeof(short));
   assert(4 == sizeof(int));
   assert(8 == sizeof(long long));

   //------------------------------------------------------------------------
   // Sources of undefined values
   //------------------------------------------------------------------------

   // Stack, 32-bit
   {
      volatile int undef_stack_int;
      fprintf(stderr, "\nUndef 1 of 8 (stack, 32 bit)\n");
      x += (undef_stack_int == 0x12345678 ? 10 : 21);
   }
   
   // Stack, 32-bit, recently modified.  Nb: we have to do the register
   // mucking about to make sure that the modification isn't fenced by a
   // store/load pair and thus not seen (see origin-not-quite.c).
   {
      volatile int undef_stack_int;
      register int modified_undef_stack_int;
      fprintf(stderr, "\nUndef 2 of 8 (stack, 32 bit)\n");
      modified_undef_stack_int = undef_stack_int;
      modified_undef_stack_int++;
      x += (modified_undef_stack_int == 0x1234 ? 11 : 22);
   }
   
   // Stack, 64-bit.  XXX: gets reported with two identical origins.
   {
      volatile long long undef_stack_longlong;
      fprintf(stderr, "\nUndef 3 of 8 (stack, 64 bit)\n");
      x += (undef_stack_longlong == 0x1234567812345678LL ? 11 : 22);
   }
   
   // Malloc block, uninitialised, 32-bit
   {
      int* ptr_to_undef_malloc_int = malloc(sizeof(int));
      int  undef_malloc_int = *ptr_to_undef_malloc_int;
      fprintf(stderr, "\nUndef 4 of 8 (mallocd, 32-bit)\n");
      x += (undef_malloc_int == 0x12345678 ? 12 : 23);
   }

   // Realloc block, uninitialised
   {
      int* ptr_to_undef_malloc_int2 = malloc(sizeof(int));
         // Allocate a big chunk to ensure that a new block is allocated.
      int* ptr_to_undef_realloc_int = realloc(ptr_to_undef_malloc_int2, 4096);
         // Have to move past the first 4 bytes, which were copied from the
         // malloc'd block.
      int  undef_realloc_int = *(ptr_to_undef_realloc_int+1);
      fprintf(stderr, "\nUndef 5 of 8 (realloc)\n");
      x += (undef_realloc_int == 0x12345678 ? 13 : 24);
   }

   // Custom-allocated block, non-zeroed
   {
      int  undef_custom_alloc_int;
      VALGRIND_MALLOCLIKE_BLOCK(&undef_custom_alloc_int, sizeof(int),
                                /*rzB*/0, /*is_zeroed*/0);
      fprintf(stderr, "\nUndef 6 of 8 (MALLOCLIKE_BLOCK)\n");
      x += (undef_custom_alloc_int == 0x12345678 ? 14 : 25);
   }

   // Heap segment (brk), uninitialised
   // CURRENTLY DISABLED.  Why?
   // - On Darwin, sbrk() is implemented via vm_allocate() which always zeroes
   //   its allocated memory.  For a while we used use a separate .exp file
   //   for Darwin, but we add an extra printf on Darwin only so that it
   //   cannot be successfully matched on non-Darwin platforms.
   // - On Ubuntu 9.04 configured with --enable-only32bit, the brk symbol
   //   shows up as "???"
   // - Our current treatment of brk is suspect;  whole new pages allocated
   //   with brk should arguably be marked defined -- see the big comment
   //   above track_new_mem_brk() in memcheck/mc_main.c.
//#if defined(VGO_darwin)
      fprintf(stderr, "\nUndef 7 of 8 (brk)\n");
//      fprintf(stderr, "\n(no complaint; sbrk initialises memory on Darwin)\n");
      fprintf(stderr, "\n(currently disabled)\n");
//#else
//   {
//      int* ptr_to_new_brk_limit = sbrk(4096);
//      int  undef_brk_int = *ptr_to_new_brk_limit;
//      fprintf(stderr, "\nUndef 7 of 8 (brk)\n");
//      x += (undef_brk_int == 0x12345678 ? 15 : 26);
//   }
//#endif

   // User block, marked as undefined
   {
      int  undef_user_int = 0;
      VALGRIND_MAKE_MEM_UNDEFINED(&undef_user_int, sizeof(int));
      fprintf(stderr, "\nUndef 8 of 8 (MAKE_MEM_UNDEFINED)\n");
      x += (undef_user_int == 0x12345678 ? 16 : 27);
   }

   //------------------------------------------------------------------------
   // Sources of defined values
   //------------------------------------------------------------------------

   // Heap block (calloc), initialised
   {
      int* ptr_to_def_calloc_int = calloc(1, sizeof(int));
      int  def_calloc_int = *ptr_to_def_calloc_int;
      fprintf(stderr, "\nDef 1 of 3\n");
      x += (def_calloc_int == 0x12345678 ? 17 : 28);
   }

   // Custom-allocated block, non-zeroed
   {
      int  def_custom_alloc_int = 0;
      fprintf(stderr, "\nDef 2 of 3\n");
      VALGRIND_MALLOCLIKE_BLOCK(&def_custom_alloc_int, sizeof(int),
                                /*rzB*/0, /*is_zeroed*/1);
      x += (def_custom_alloc_int == 0x12345678 ? 18 : 29);
   }

   // mmap block, initialised
   {
      int* ptr_to_def_mmap_int =
               mmap(0, 4096, PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
      int def_mmap_int = *ptr_to_def_mmap_int;
      fprintf(stderr, "\nDef 3 of 3\n");
      x += (def_mmap_int == 0x12345678 ? 19 : 30);
   }

   return x;
}
