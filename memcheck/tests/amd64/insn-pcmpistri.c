/* https://bugs.kde.org/show_bug.cgi?id=309921 */

#define _XOPEN_SOURCE 600 /* for posix_memalign() */

#include "../../memcheck.h"

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

/* Exercise pcmpistri instruction in a realistic way. */
int aligned_strlen(const char *const s)
{
   assert(((unsigned long)s & 0x0F) == 0);

   const char *p = s;

   /* volatile asm and "memory" clobber are needed here, since we
      access memory in ways we cannot describe to GCC. */
   __asm__ __volatile__ ("\n1:\n"
                         "\tmovdqa (%0),%%xmm6\n"
                         "\tpcmpistri $0x3a,%%xmm6,%%xmm6\n"
                         "\tjc 2f\n"
                         "\tadd $0x10,%0\n"
                         "\tjmp 1b\n"
                         "2:\n"
                         "\tadd %%rcx,%0\n"
                         : "=p" (p) : "0" (p) : "xmm6", "rcx", "cc", "memory");

   return p-s;
}

/* Compute strlen(s).  Arrange for result to be valid or invalid
   according to second argument. */
int test_strlen(const char *const s, int valid)
{
   /* len = length of string including trailing null */
   const size_t len = strlen(s) + 1;
   const size_t roundup = ((len+15)/16)*16;
   int result = -1;

   void *space;
   posix_memalign(&space, 16, roundup);
   memset(space, 'x', roundup);
   memcpy(space, s, len);

   const char *const s_copy = space;
   const unsigned char ff = 0xFF;
   if (valid) {
      /* Mark all bytes beyond the null as invalid. */
      size_t i;
      for (i=len ; i < roundup ; ++i)
         (void)VALGRIND_SET_VBITS(&s_copy[i], &ff, 1);
   }
   else {
      /* Mark the null byte itself as invalid. */
      assert(len > 0);
      (void)VALGRIND_SET_VBITS(&s_copy[len-1], &ff, 1);
   }

   result = aligned_strlen(s_copy);

   free(space);

   return result;
}

void doit(const char *const s)
{
   printf("strlen(\"%s\")=%d\n", s, test_strlen(s, 1));

   fprintf(stderr, "strlen(\"%s\")=%s\n", s,
           test_strlen(s, 0) ? "true" : "false");
}

int main(int argc, char *argv[])
{
   doit("");
   doit("a");
   doit("ab");
   doit("abc");
   doit("abcd");
   doit("abcde");
   doit("abcdef");
   doit("abcdefg");
   /* 8 */
   doit("abcdefgh");
   doit("abcdefghi");
   doit("abcdefghij");
   doit("abcdefghijk");
   doit("abcdefghijkl");
   doit("abcdefghijklm");
   doit("abcdefghijklmn");
   doit("abcdefghijklmno");
   /* 16 */
   doit("abcdefghijklmnop");
   doit("abcdefghijklmnopq");
   doit("abcdefghijklmnopqr");
   doit("abcdefghijklmnopqrs");
   doit("abcdefghijklmnopqrst");
   doit("abcdefghijklmnopqrstu");
   doit("abcdefghijklmnopqrstuv");
   doit("abcdefghijklmnopqrstuvw");
   doit("abcdefghijklmnopqrstuwvx");
   doit("abcdefghijklmnopqrstuwvxy");
   doit("abcdefghijklmnopqrstuwvxyz");
   /* 255 */
   doit("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   /* 256 */
   doit("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   return 0;
}
