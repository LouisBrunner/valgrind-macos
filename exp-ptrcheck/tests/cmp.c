#include <stdlib.h>

// The comparisons use SUB instructions, and this can result in having a
// (nonptr - ptr) situation legitimately;  at one point I was flagging
// errors when that happened.

int main(void)
{
   char* buf = malloc(sizeof(char) * 6);

   // Known zero non-pointer
   char* nz = (char*)((long)buf^(long)buf);  // known non-pointer

   // Unknown zero nonptr;  make them zero but unknown
   char* unz;
   ((char*)&unz)[0] = '\0';
   ((char*)&unz)[1] = '\0';
   ((char*)&unz)[2] = '\0';
   ((char*)&unz)[3] = '\0';

   if (buf == nz)  return 1;
   if (nz  == buf) return 1;     // --> n - p, but legitimate
   if (buf == unz) return 1;
   if (unz == buf) return 1;

   return 0;
}
