
#include <stdlib.h>

typedef
   struct {
      int          x;
      unsigned int y:1;
      int          z;
   } 
   Fooble;

int main ( void )
{
   Fooble* f = malloc(sizeof(Fooble));
   f->x = 1;
   f->z = 1;
   f->y = (f == (Fooble*)17 ? 1 : 0);
   return 0;
}
