
#include <math.h>
#include <stdio.h>

int xToI ( );

int main ( void )
{
   printf ( "the answer is %d\n", xToI () );
   return 0;
}


int xToI()
{
    return (int)floor(2.90) + 1;
}

