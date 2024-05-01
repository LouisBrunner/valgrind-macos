#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
 
int main(void)
{
   char* astring = strdup("this is a string # with something to seek");
   size_t len = strlen(astring);
   memccpy(astring+10, astring, '#', len-10);
   sprintf(astring, "this is a string # with something to seek");
   memccpy(astring, astring+10, '#', len);
   
   sprintf(astring, "this is a string # with something to seek");
   /*
    * space is earlier than len, no overlap
    * "this " gets copied (up to and including the first ' ')
    * and it overwrites the destination starting with the 's' of "string"
    * so res will point to the 'g' of "string"
    */
   char* res = memccpy(astring+10, astring, ' ', len-10);
   assert(res && *res == 'g');
   sprintf(astring, "this is a string # with something to seek");
   /* length is 0, nothing copied, returns NULL */
   res = memccpy(astring, "abcdefhhijklmnopqrstuvwxy", 'z', 0);
   assert(NULL == res);
   /* 'z' not found so 20 bytes copied, returns NULL */
   res = memccpy(astring, "abcdefhhijklmnopqrstuvwxy", 'z', 20);
   assert(NULL == res);
   free(astring);
}

