#include <stdlib.h>
#include <string.h>

// The issue here is the same one in memcmptest -- 'strchr' and 'index' are
// aliases, as are 'strrchr' and 'rindex'.  In each case, the shorter name
// gets preferred, ie. 'index' and 'rindex'.

int main(int argc, char* argv[])
{
   char *s, *a __attribute__((unused)), *b __attribute__((unused));
   s = malloc(sizeof(char));

   // Nb: s[0] is uninitialised, but almost certainly a zero
   
   a = strchr (s, '1');
   b = strrchr(s, '1');
   return 0;//((int)a + (int)b);
}
