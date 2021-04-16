#include <stdio.h>
#include <string.h>

#define VECTOR __attribute__ ((vector_size (16)))

typedef char VECTOR char_v;

struct vstrc_char_rng {
   unsigned char range[16];
   unsigned char flags[16];
};

#define RNG_FLAG_EQ   0x80
#define RNG_FLAG_LT   0x40
#define RNG_FLAG_GT   0x20
#define RNG_FLAG_ANY  0xe0
#define RNG_FLAG_NONE 0x00

volatile char tmp;
static const char *hex_digit = "0123456789abcdefGHIJKLMNOPQRSTUV";

static void test_vstrc_char(const char *str, const struct vstrc_char_rng *rng,
                            int expect_res, int expect_cc)
{
   int cc;
   char_v v1;
   char_v v2 = *(const char_v *) str;
   char_v v3 = *(const char_v *) rng->range;
   char_v v4 = *(const char_v *) rng->flags;

   __asm__(
      "cr    0,0\n\t"           /* Clear CC */
      "vstrc %[v1],%[v2],%[v3],%[v4],0,3\n\t"
      "ipm   %[cc]\n\t"
      "srl   %[cc],28"
      : [v1] "=v" (v1),
        [cc] "=d" (cc)
      : [v2] "v" (v2),
        [v3] "v" (v3),
        [v4] "v" (v4)
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
   struct vstrc_char_rng rng;
   char buf[16];

   memset(rng.flags, RNG_FLAG_NONE, 16);

   rng.range[4] = 'z';
   rng.flags[4] = RNG_FLAG_GT | RNG_FLAG_EQ;
   rng.flags[5] = RNG_FLAG_ANY;
   /* OK: match at the 'z' */
   test_vstrc_char("find the z", &rng, 9, 2);

   rng.flags[12] = RNG_FLAG_GT | RNG_FLAG_EQ;
   rng.flags[13] = RNG_FLAG_LT | RNG_FLAG_EQ;
   /* Bad: undefined range */
   test_vstrc_char("undefined", &rng, -1, -1);

   rng.range[12] = 'a';
   rng.range[13] = 'c';
   /* OK: match at the 'a' */
   test_vstrc_char("get the abc", &rng, 8, 2);

   rng.flags[12] = RNG_FLAG_LT;
   rng.flags[13] = RNG_FLAG_GT;
   /* OK: no match up to null terminator */
   test_vstrc_char("no match", &rng, 8, 0);

   /* OK: no match, no null terminator */
   test_vstrc_char("0123456789abcdef", &rng, 16, 3);

   buf[0] = 'x';
   /* Bad: undefined string */
   test_vstrc_char(buf, &rng, -1, -1);

   buf[1] = 'z';
   /* Bad: valid match, but CC undefined */
   test_vstrc_char(buf, &rng, 1, -1);

   return 0;
}
