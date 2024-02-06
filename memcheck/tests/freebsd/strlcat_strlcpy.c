#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(void)
{
   const size_t dstsize = 100U;
   char *dst = malloc(dstsize);
   // normal use
   strlcpy(dst, "test1", dstsize);
   assert(!strcmp(dst, "test1"));
   strcat(dst, "test2");
   // overlap, source starts within dst string
   strlcpy(dst+4, dst+9, dstsize-4);
   sprintf(dst, "test1test2");
   // overlap, dst starts within src string
   strlcpy(dst+9, dst+4, dstsize-9);
   sprintf(dst, "test1");
   // overlap, dst points to nul terminator of src
   strlcpy(dst+5, dst+4, dstsize-5);
   sprintf(dst, "test1");
   // as above but incorrect length (1 too long)
   // since src nul is overwritten this will
   // keep reading from src until the length limit
   // is reached
   // since the length is wrong this will result
   // in an invalid read and write 1 byte
   // beyond the end of the buffer
   strlcpy(dst+5, dst+4, dstsize-4);
   
   sprintf(dst, "test1");
   strlcat(dst, "test2", dstsize);
   assert(!strcmp(dst, "test1test2"));
   
   strlcat(dst+5, dst+7, dstsize-5);
   sprintf(dst, "test1test2");
   // we can't really control 'dst' since
   // the destination id the end of the string
   strlcat(dst+7, dst+5, dstsize-7);
   
   // again wrong dstsize
   sprintf(dst, "test1");
   strlcpy(dst+3, dst+4, dstsize-2);
   free(dst);
}

