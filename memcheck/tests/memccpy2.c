#include <ctype.h>
#include <stdio.h>
#include <string.h>
 
int main(void)
{
   char* astring = strdup("this is a string # with something to seek");
   size_t len = strlen(astring);
   memccpy(astring+10, astring, '#', len-10);
   sprintf(astring, "this is a string # with something to seek");
   memccpy(astring, astring+10, '#', len);
}

