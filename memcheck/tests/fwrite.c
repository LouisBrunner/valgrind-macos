
#include <stdlib.h>
#include <unistd.h>
int main ( void )
{
   char* arr = malloc(10);
   (void) write( 1 /* stdout */, arr, 10 );
   return 0;
}
