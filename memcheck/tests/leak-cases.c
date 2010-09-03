#include <stdio.h>
#include <stdlib.h>
#include "leak.h"
#include "../memcheck.h"

// Pointer chain          AAA Category/output BBB Category/output
// -------------          ------------------- ------------
// p1 ---> AAA            DR / R
// p2 ---> AAA ---> BBB   DR / R              IR / R
// p3      AAA            DL / L
// p4      AAA ---> BBB   DL / I              IL / L
// p5 -?-> AAA            (y)DR, (n)DL / P
// p6 ---> AAA -?-> BBB   DR / R              (y)IR, (n)DL / P
// p7 -?-> AAA ---> BBB   (y)DR, (n)DL / P    (y)IR, (n)IL / P
// p8 -?-> AAA -?-> BBB   (y)DR, (n)DL / P    (y,y)IR, (n,y)IL, (_,n)DL / P
// p9      AAA -?-> BBB   DL / L              (y)IL, (n)DL / I 
//
// Pointer chain legend:
// - pN: a root set pointer
// - AAA, BBB: heap blocks
// - --->: a start-pointer
// - -?->: an interior-pointer
//
// Category legend:
// - DR: Directly reachable
// - IR: Indirectly reachable
// - DL: Directly lost
// - IL: Indirectly lost
// - (y)XY: it's XY if the interior-pointer is a real pointer
// - (n)XY: it's XY if the interior-pointer is not a real pointer
// - (_)XY: it's XY in either case
//
// How we handle the 9 cases:
// - "directly lost":    case 3
// - "indirectly lost":  cases 4, 9
// - "possibly lost":    cases 5..8
// - "still reachable":  cases 1, 2


typedef
   struct _Node {
      struct _Node* next;
      // Padding ensures the structu is the same size on 32-bit and 64-bit
      // machines.
      char padding[8 - sizeof(struct _Node*)];
   } Node;

Node* mk(Node* next)
{
   // We allocate two nodes, so we can do p+1 and still point within the
   // block.
   Node* x = malloc(2 * sizeof(Node));
   x->next = next;
   return x;
}

// These are definite roots.
Node* p1;
Node* p2;
Node* p3;
Node* p4;
Node* p5;
Node* p6;
Node* p7;
Node* p8;
Node* p9;

void f(void)
{
   p1 = mk(NULL);       // Case 1: 16/1 still reachable

   p2 = mk(mk(NULL));   // Case 2: 16/1 still reachable
                                // 16/1 still reachable
   (void)mk(NULL);      // Case 3: 16/1 definitely lost

   (void)mk(mk(NULL));  // Case 4: 16/1 indirectly lost (counted again below!)
                                // 32(16d,16i)/1 definitely lost (double count!)
   p5 = mk(NULL);       // Case 5: 16/1 possibly lost (ok)
   p5++;

   p6 = mk(mk(NULL));   // Case 6: 16/1 still reachable
   (p6->next)++;                // 16/1 possibly lost

   p7 = mk(mk(NULL));   // Case 7: 16/1 possibly lost
   p7++;                        // 16/1 possibly lost

   p8 = mk(mk(NULL));   // Case 8: 16/1 possibly lost
   (p8->next)++;                // 16/1 possibly lost
   p8++;

   p9 = mk(mk(NULL));   // Case 9: 16/1 indirectly lost (counted again below!)
   (p9->next)++;                // 32(16d,16i)/1 definitely lost (double count!)
   p9 = NULL;
}

int main(void)
{
   DECLARE_LEAK_COUNTERS;

   GET_INITIAL_LEAK_COUNTS;

   // Originally, this program did all the work in main(), but on some
   // platforms (x86/Darwin and AMD64/Linux with --enable-only32bit) stray
   // pointers to supposedly-lost heap blocks were being left on the stack,
   // thus making them reachable.  Doing the allocations in f() and the leak
   // counting in main() avoids the problem.
   f();

   CLEAR_CALLER_SAVED_REGS;
   GET_FINAL_LEAK_COUNTS;

   PRINT_LEAK_COUNTS(stderr);

   return 0;
}
