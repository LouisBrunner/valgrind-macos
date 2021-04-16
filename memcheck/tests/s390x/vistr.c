#include <stdio.h>
#include <string.h>

#define VECTOR __attribute__ ((vector_size (16)))

typedef char VECTOR char_v;

volatile char tmp;
static const char *hex_digit = "0123456789abcdef";

static char_v to_char_vec(const char *str, char_v *maskp)
{
   char buf[17];
   char_v v;
   char_v mask = {0};

   for (int i = 0; i < sizeof(buf); i++) {
      char ch = str[i];
      if (ch == '\0')
         break;
      else if (ch == '$') {
         buf[i] = '\0';
         mask[i] = -1;
      } else if (ch != '~') {
         buf[i] = ch;
         mask[i] = -1;
      }
   }
   v = *(char_v *) buf;
   *maskp = mask;
   return v;
}

static void test_vistr_char(const char *str, const char *expect_res,
                            int expect_cc)
{
   int cc, count;
   char_v v1, mask;
   char_v v2 = to_char_vec(str, &mask);
   char_v exp_v1 = to_char_vec(expect_res, &mask);
   char equal[16];

   __asm__(
      "cr    0,0\n\t"           /* Clear CC */
      "vistr %[v1],%[v2],0,1\n\t"
      "ipm   %[cc]\n\t"
      "srl   %[cc],28"
      : [v1] "=v" (v1),
        [cc] "=d" (cc)
      : [v2] "v" (v2)
      : "cc");

   *(char_v *) equal = (v1 & mask) == (exp_v1 & mask);
   if (memchr(equal, 0, sizeof(equal)))
      printf("Result doesn't match `%s'\n", expect_res);

   count = 0;
   for (int i = 0; i < 16; i++) {
      if (v1[i] == 0) count++;
   }
   tmp = hex_digit[count];

   tmp = hex_digit[cc & 0xf];
   if (expect_cc >= 0 && cc != expect_cc)
      printf("CC %d != %d\n", cc, expect_cc);
}

int main()
{
   test_vistr_char("terminated$====~", "terminated$$$$$$", 0);
   test_vistr_char("undef~~~~~~~~~~~", "undef", -1);
   test_vistr_char("undef, 2nd half~", "undef, 2nd half", -1);
   test_vistr_char("Not. Terminated.", "Not. Terminated.", 3);
   test_vistr_char("partiallyOK~~$~~", "partiallyOK~~$$$", 0);
   return 0;
}
