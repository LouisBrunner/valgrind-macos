
#include <stdlib.h>
#include <string.h>

// glibc's versions of functions like strlen() do things word-wise instead
// of byte-wise, which means they can overrun the end of strings, etc.
// Naughty, but must be safe, I guess;  Annelid copes with this in the same
// way Memcheck does, letting it happen unless the --partial-loads-ok=no
// option is used.

int main(void)
{
   char* h  = "hello, world";
   char* p = strdup(h);
   char  u[20];
   char* c __attribute__((unused));
   int   len;

   len = strlen(p);

   c = strchr (p, 'l'); 
   c = strchr (p, 'x'); 

   c = strrchr(p, 'l'); 
   c = strrchr(p, 'x'); 

   c = memchr (p, 'l', len);  // glibc version ok?
   c = memchr (p, 'x', len); 

   memcpy(u, p, len+1);       // glibc version ok?

   return 0;
}
