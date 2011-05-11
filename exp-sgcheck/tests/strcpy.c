#include <stdlib.h>
#include <string.h>

// This shows the case whereby subtraction between two pointers from
// different segments can be used legitimately.

// dest: stack, src: heap
char* my_strcpy (char* dest, const char* src)
{
   char c, *s = (char *) src;
   long off = dest - s;
   off = off - 1;
   do {
      c = *s++;
      s[off] = c;          // s + off == dest
   } while (c != '\0');
   return dest;
}

int main(void)
{
   char* h  = "hello, world";
   char* p1 = strdup(h);
   char* p2 = strdup(h);
   char  u1[13];
   char  u2[13];

   // All these are legit
   p1[p2-p1] = 0;    // p-p   (must be BADSEG'd) // ea is p2[0]
   u1[p2-u1] = 0;    // p-?
   p1[u2-p1] = 0;    // ?-p   (must be BADSEG'd)
   u1[u2-u1] = 0;    // ?-?

   // All these are a 1-byte underrun
   p1[p2-p1-1] = 0;  // p-p   (must be BADSEG'd) // ea is p2[-1]
   u1[p2-u1-1] = 0;  // p-?   (undet) 
   p1[u2-p1-1] = 0;  // ?-p   (must be BADSEG'd)
   u1[u2-u1-1] = 0;  // ?-?   (undet)

   my_strcpy(u1, p1);
   my_strcpy(u2, u1);

   return 0;
}
