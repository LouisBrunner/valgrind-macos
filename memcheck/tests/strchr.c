#include <stdlib.h>
#include <string.h>

// The issue here is the same one in memcmptest -- 'strchr' and 'index' are
// aliases, as are 'strrchr' and 'rindex'.  In each case, the shorter name
// gets preferred, ie. 'index' and 'rindex'.

int main(int argc, char* argv[])
{
   char* s = malloc(10);
   char* a = strchr(s, '\0');
   char* b = strrchr(s, '\0');
   return ((int)a + (int)b);
}
