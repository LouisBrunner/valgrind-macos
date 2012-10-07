#include <stdio.h>
#include <ctype.h>
#include "test.h"

#define LOOPBACK "jo 0b	\n\t"

typedef struct {
   const char *str1;
   const char *str2;
   int cc;
} clst_t;

static clst_t
do_clst(const char *__string1, const char *__string2, char __end)
{
   register char end asm ("0") = __end;
   register const char *string1 asm ("2") = __string1;
   register const char *string2 asm ("4") = __string2;

   asm volatile( "0: clst 2,4\n\t"
                 LOOPBACK
                 :"+d" (string1), "+d" (string2) :"d" (end): "cc");

   return (clst_t) { .str1 = string1, .str2 = string2, .cc = get_cc() };
}

void
clst(const char *str1, const char *str2, int sentinel)
{
   clst_t res;

   printf("comparing: %s with %s   sentinel = %d", str1, str2, sentinel);
   if (isprint(sentinel))
      printf(" (%c)", sentinel);
   printf("\n");
   res = do_clst(str1, str2, sentinel);
   printf("str1 = %s\nstr2 = %s\ncc = %d\n", res.str1, res.str2, res.cc);
   printf("\n");
}

int main(void)
{
   clst("lower123",  "lowerabc",  '\0');
   clst("higher234", "higher123", '\0');
   clst("equal",  "equal",  '\0');

   clst("equal",  "equallong",  '\0');
   clst("equallong",  "equal",  '\0');

   clst("lower1",  "lower2",  'w');   // will compare equal

   return 0;
}
