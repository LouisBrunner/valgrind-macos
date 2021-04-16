#include <stdio.h>
#include <string.h>

#define VECTOR __attribute__ ((vector_size (16)))

typedef char VECTOR char_v;

volatile char tmp;
static const char *hex_digit = "0123456789abcdefGHIJKLMNOPQRSTUV";

static char_v to_char_vec(const char *str)
{
   char_v v;
   char buf[17];
   int len = strlen(str);

   memcpy(buf, str, (len && str[len - 1] == '~') ? len - 1 : len + 1);
   v = *(char_v *) buf;
   return v;
}

#define GENERATE_TEST(mnem)                                          \
static void test_ ## mnem ## _char(const char *str, const char *match, \
                                   int expect_res, int expect_cc)    \
{                                                                    \
   int cc;                                                           \
   char_v v1;                                                        \
   char_v v2 = to_char_vec(str);                                     \
   char_v v3 = to_char_vec(match);                                   \
                                                                     \
   __asm__(                                                          \
      "cr    0,0\n\t"           /* Clear CC */                       \
      #mnem "  %[v1],%[v2],%[v3],0,3\n\t"                            \
      "ipm   %[cc]\n\t"                                              \
      "srl   %[cc],28"                                               \
      : [v1] "=v" (v1),                                              \
        [cc] "=d" (cc)                                               \
      : [v2] "v" (v2),                                               \
        [v3] "v" (v3)                                                \
      : "cc");                                                       \
                                                                     \
   tmp = hex_digit[v1[7] & 0x1f];                                    \
   if (expect_res >= 0  && v1[7] != expect_res)                      \
      printf("result %u != %d\n", v1[7], expect_res);                \
                                                                     \
   tmp = hex_digit[cc & 0xf];                                        \
   if (expect_cc >= 0 && cc != expect_cc)                            \
      printf("CC %d != %d\n", cc, expect_cc);                        \
}

GENERATE_TEST(vfae)

GENERATE_TEST(vfee)

GENERATE_TEST(vfene)

int main()
{
   test_vfae_char("not found", "................", 9, 0);
   test_vfae_char("xy", "zzzzzzzzyyyyyyyy", 1, 2);
   test_vfae_char("incomplete~", "xxxxxxxxxxxxxxxx", -1, -1);

   test_vfee_char("same char here", "..........here", 10, 2);
   test_vfee_char("and here too ...", "_________t~", 9, 1);
   test_vfee_char("equality!~", "========!!~", 8, -1);

   test_vfene_char("strings equal", "strings equal", 13, 0);
   test_vfene_char(hex_digit, hex_digit, 16, 3);
   test_vfene_char("undef~", "undefined", -1, -1);
   test_vfene_char("active~", "actually ok", 3, 1);
   return 0;
}
