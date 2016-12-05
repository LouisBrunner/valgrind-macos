#include <stdio.h>
#include <stdlib.h>
#include "leak.h"
#include "../memcheck.h"

/* We build this tree:
  
           A
         /   \
        B     C
       / \   / \ 
      D   E F   G
  
   Then we leak D and C-F-G.
*/

typedef
   struct _Node {
      struct _Node *l;
      struct _Node *r;
      // Padding ensures the structu is the same size on 32-bit and 64-bit
      // machines.
      char padding[16 - 2*sizeof(struct _Node*)];
   } Node;

Node* mk(void)
{
   Node *x = malloc(sizeof(Node));
   x->l = NULL;
   x->r = NULL;
   return x;
}

// This is a definite root.
Node* t;

void f(void)
{
   // Building like this rather than "t = mk(mk(mk(NULL, NULL), ...)" seems to
   // help avoid leaving pointers on the stack to supposedly-leaked blocks.
   t       = mk();   // A
   t->l    = mk();   // B
   t->r    = mk();   // C  (48(16d,32i)/1 definitely leaked from here)
   t->l->l = mk();   // D  (16/1 definitely leaked from here)
   t->l->r = mk();   // E
   t->r->l = mk();   // F
   t->r->r = mk();   // G

   // Sever B->D, leaking D
   t->l->l = NULL;
 
   // Sever A->C, leaking C-F-G
   t->r = NULL;
}

int main(void)
{
   DECLARE_LEAK_COUNTERS;

   GET_INITIAL_LEAK_COUNTS;

   // See leak-cases.c for why we do the work in f().
   f();

   CLEAR_CALLER_SAVED_REGS;
   GET_FINAL_LEAK_COUNTS;

   PRINT_LEAK_COUNTS(stderr);

   return 0;
}

