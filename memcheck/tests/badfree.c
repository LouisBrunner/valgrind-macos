

#include <stdio.h>
#include <stdlib.h>
static void* return_arg(void* q);
int main ( void )
{
   void* p = (void*)0x87654321;
   int q[] = { 1, 2, 3 };
   
   /* Free a pointer to Never-Never Land */
   free(p);

   /* Free a pointer to a stack block */
   free(return_arg(q));

   return 0;
}

/*
 * The only purpose of the function below is to make sure that gcc 4.4.x does
 * not print the following warning during the compilation of this test program:
 * warning: attempt to free a non-heap object
 */
static void* return_arg(void* q)
{
   return q;
}

