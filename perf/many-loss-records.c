// Performance test for the leak checker from bug #191182.
// Nb: it must be run with --leak-resolution=high to show the quadratic
// behaviour caused by the large number of loss records.  
// By Philippe Waroquiers.
//
// On my machine, before being fixed, building the loss record list took about
// 36 seconds, and sorting + printing it took about 20 seconds.  After being
// fixed it took about 2 seconds, and the leak checking was only a small
// fraction of that. --njn

#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>

/* parameters */

/* we will create stack_fan_out ^ stack_depth different call stacks */
int stack_fan_out = 15;
int stack_depth = 4; 

/* for each call stack, allocate malloc_fan blocks */
int malloc_fan = 4;

/* for each call stack, allocate data structures having malloc_depth
   indirection at each malloc-ed level */
int malloc_depth = 2; 

/* in addition to the pointer needed to maintain the levels; some more
   bytes are allocated simulating the data stored in the data structure */
int malloc_data = 5;

/* every n top blocks, 1 block and all its children will be freed instead of
   being kept */
int free_every_n = 2;

/* every n release block operation, 1 block and its children will be leaked */
int leak_every_n = 250;



struct Chunk {
   struct Chunk* child;
   char   s[];
};

struct Chunk** topblocks;
int freetop = 0;

/* statistics */
long total_malloced = 0;
int blocknr = 0;
int blockfreed = 0;
int blockleaked = 0;
int total_stacks = 0;
int releaseop = 0;

void free_chunks (struct Chunk ** mem)
{
   if (*mem == 0)
      return;

   free_chunks ((&(*mem)->child));

   blockfreed++;
   free (*mem);
   *mem = 0; 
}

void release (struct Chunk ** mem)
{
   releaseop++;

   if (releaseop % leak_every_n == 0) {
      blockleaked++;
      *mem = 0; // lose the pointer without free-ing the blocks
   } else {
      free_chunks (mem);
   }
}

void call_stack (int level)
{
   int call_fan_out = 1;

   if (level == stack_depth) {  
      int sz = sizeof(struct Chunk*) + malloc_data;
      int d;
      int f;

      for (f = 0; f < malloc_fan; f++) {
         struct Chunk *new  = NULL;    // shut gcc up
         struct Chunk *prev = NULL;

         for (d = 0; d < malloc_depth; d++) {
            new = malloc (sz);
            total_malloced += sz;
            blocknr++;
            new->child = prev;
            prev = new;
         }
         topblocks[freetop] = new;

         if (freetop % free_every_n == 0) {
               release (&topblocks[freetop]);
         }
         freetop++;
      }

      total_stacks++;

   } else {
      /* Nb: don't common these up into a loop!  We need different code
         locations to exercise the problem. */
      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      call_stack (level + 1);
      if (call_fan_out == stack_fan_out) return;
      call_fan_out++;

      printf ("maximum stack_fan_out exceeded\n");
   }
}

int main()
{
   int d;
   int stacks = 1;
   for (d = 0; d < stack_depth; d++)
      stacks *= stack_fan_out;
   printf ("will generate %d different stacks\n", stacks);
   topblocks = malloc(sizeof(struct Chunk*) * stacks * malloc_fan);
   call_stack (0);
   printf ("total stacks %d\n", total_stacks);
   printf ("total bytes malloc-ed: %ld\n", total_malloced);
   printf ("total blocks malloc-ed: %d\n", blocknr);
   printf ("total blocks free-ed: %d\n", blockfreed);
   printf ("total blocks leak-ed: %d\n", blockleaked);
   return 0;
}
