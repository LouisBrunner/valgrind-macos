#include <stdio.h>
#include <string.h>

#define VECTOR __attribute__ ((vector_size (16)))

typedef char VECTOR char_v;

volatile char tmp;
static const char *hex_digit = "0123456789abcdefGHIJKLMNOPQRSTUV";

static char_v to_char_vec(const char *str)
{
   char buf[17];
   char_v v;

   for (int i = 0; i < sizeof(buf); i++) {
      char ch = str[i];
      if (ch == '\0')
         break;
      else if (ch == '$')
         buf[i] = '\0';
      else if (ch != '~')
         buf[i] = ch;
   }
   v = *(char_v *) buf;
   return v;
}

static void test_vstrs_char(const char *haystack, const char *needle,
                            int expect_res, int expect_cc)
{
   int cc;
   char_v v2val = to_char_vec(haystack);
   char_v v3val = to_char_vec(needle);

   register unsigned long VECTOR v4 __asm__("v4") = { strlen(needle), 0 };
   register char_v v1 __asm__("v1");
   register char_v v2 __asm__("v2") = v2val;
   register char_v v3 __asm__("v3") = v3val;

   __asm__(
      "cr     0,0\n\t"                  /* Clear CC */
      ".short 0xe712,0x3020,0x408b\n\t" /* vstrs %v1,%v2,%v3,%v4,0,2 */
      "ipm    %[cc]\n\t"
      "srl    %[cc],28"
      : "=v" (v1), [cc] "=d" (cc)
      : "v" (v2), "v" (v3), "v" (v4)
      : "cc");

   tmp = hex_digit[v1[7] & 0x1f];
   if (expect_res >= 0  && v1[7] != expect_res)
      printf("result %u != %d\n", v1[7], expect_res);

   tmp = hex_digit[cc & 0xf];
   if (expect_cc >= 0 && cc != expect_cc)
      printf("CC %d != %d\n", cc, expect_cc);
}

int main()
{
   test_vstrs_char("haystack$needle", "needle$haystack", 16, 1);
   test_vstrs_char("haystack, needle", "needle, haystack", 10, 3);
   test_vstrs_char("ABCDEFGH", "DEFGHI", -1, -1);
   test_vstrs_char("match in UNDEF", "UN", 9, 2);
   test_vstrs_char("after ~ UNDEF", "DEF", -1, -1);
   test_vstrs_char("", "", 0, 2);
   return 0;
}
