/* https://bugs.kde.org/show_bug.cgi?id=432801 */

#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <immintrin.h>

#include "../../memcheck.h"

// This function fails when compiled on clang version 10 or greater with -O2.
// It's unused by the test but left here as a copy of the error in the bug
// report https://bugs.kde.org/show_bug.cgi?id=432801
void standalone() {
   struct sigaction act;
   if (sigaction(SIGTERM, 0, &act) == 1) {
      return;
   }
   if (sigaction(SIGTERM, 0, &act) == 1) {
      return;
   }

   char pattern[] = "\x1\x2\x3\x4\x5\x6\x7\x8\x9";
   const unsigned long plen = strlen(pattern);
   pattern[1] = 0;
   size_t hp=0;
   for (size_t i = 0; i < plen; ++i)
      hp += pattern[i];
   volatile size_t j = 0;
   if (j == hp % 10) {
      j++;
   }
   printf("%zd\n", hp);
}

typedef union V128 {
  struct {
    uint64_t w64[2];  /* Note: little-endian */
  };
  struct {
   uint32_t w32[4];  /* Note: little-endian */
  };
  struct {
    uint16_t w16[8];  /* Note: little-endian */
  };
  struct {
   uint8_t w8[16];  /* Note: little-endian */
  };
} V128;

template <typename T>
static T cmpGT(V128 x, V128 y, size_t element_select) = delete;

template <>
uint64_t cmpGT(V128 x, V128 y, size_t element_select) {
  uint8_t x_data[16];
  uint8_t y_data[16];
  memcpy(x_data, &x, sizeof(V128));
  memcpy(y_data, &y, sizeof(V128));
  __asm__ __volatile__(
      "movdqu     (%1), %%xmm5 \n"
      "movdqu     (%0), %%xmm6 \n"
      // order swapped for AT&T style which has destination second.
      "pcmpgtq %%xmm5,%%xmm6 \n"
      "movdqu   %%xmm6, (%0) \n"
      :
      : "r"(x_data), "r"(y_data)
      : "memory", "xmm5", "xmm6" /* clobbers */);
  memcpy(&x, x_data, sizeof(V128));

  return ((uint64_t*)&x)[element_select];
}

template <>
uint32_t cmpGT(V128 x, V128 y, size_t element_select) {
  uint8_t x_data[16];
  uint8_t y_data[16];
  memcpy(x_data, &x, sizeof(V128));
  memcpy(y_data, &y, sizeof(V128));
  __asm__ __volatile__(
      "movdqu     (%1), %%xmm5 \n"
      "movdqu     (%0), %%xmm6 \n"
      // order swapped for AT&T style which has destination second.
      "pcmpgtd %%xmm5,%%xmm6 \n"
      "movdqu   %%xmm6, (%0) \n"
      :
      : "r"(x_data), "r"(y_data)
      : "memory", "xmm5", "xmm6" /* clobbers */);
  memcpy(&x, x_data, sizeof(V128));

  return ((uint32_t*)&x)[element_select];
}

template <>
uint16_t cmpGT(V128 x, V128 y, size_t element_select) {
  uint8_t x_data[16];
  uint8_t y_data[16];
  memcpy(x_data, &x, sizeof(V128));
  memcpy(y_data, &y, sizeof(V128));
  __asm__ __volatile__(
      "movdqu     (%1), %%xmm5 \n"
      "movdqu     (%0), %%xmm6 \n"
      // order swapped for AT&T style which has destination second.
      "pcmpgtw %%xmm5,%%xmm6 \n"
      "movdqu   %%xmm6, (%0) \n"
      :
      : "r"(x_data), "r"(y_data)
      : "memory", "xmm5", "xmm6" /* clobbers */);
  memcpy(&x, x_data, sizeof(V128));

  return ((uint16_t*)&x)[element_select];
}

template <>
uint8_t cmpGT(V128 x, V128 y, size_t element_select) {
  uint8_t x_data[16];
  uint8_t y_data[16];
  memcpy(x_data, &x, sizeof(V128));
  memcpy(y_data, &y, sizeof(V128));
  __asm__ __volatile__(
      "movdqu     (%1), %%xmm5 \n"
      "movdqu     (%0), %%xmm6 \n"
      // order swapped for AT&T style which has destination second.
      "pcmpgtb %%xmm5,%%xmm6 \n"
      "movdqu   %%xmm6, (%0) \n"
      :
      : "r"(x_data), "r"(y_data)
      : "memory", "xmm5", "xmm6" /* clobbers */);
  memcpy(&x, x_data, sizeof(V128));

  return ((uint8_t*)&x)[element_select];
}

static void set_vbits(V128 *addr, V128 vbits)
{
   for (size_t i=0 ; i<2 ; ++i) {
      (void)VALGRIND_SET_VBITS(&addr->w64[i], &vbits.w64[i], sizeof(vbits.w64[i]));
   }
}

// Convert a string like "123XXX45" to a value and vbits.
template <typename T>
static void string_to_vbits(const char *s, T *x, T *vx)
{
   *x = 0;
   *vx = 0;

   for (; *s; s++) {
      int lowered_c = tolower(*s);
      *x <<= 4;
      *vx <<= 4;
      if (lowered_c == 'x') {
         *vx |= 0xf;
      } else if (isdigit(lowered_c)) {
         *x |= lowered_c - '0';
      } else if (lowered_c >= 'a' && lowered_c <= 'f') {
         *x |= lowered_c - 'a' + 0xa;
      } else {
         fprintf(stderr, "Not a hex digit: %c\n", *s);
         exit(1);
      }
   }
}

template <typename T>
static V128 string_to_vbits(const char *s, size_t lane) {
   T x, vx;
   string_to_vbits(s, &x, &vx);

   V128 vx128 = {{0}};
   vx128.w32[0] = 0xffffffff;
   vx128.w32[1] = 0xffffffff;
   vx128.w32[2] = 0xffffffff;
   vx128.w32[3] = 0xffffffff;
   V128 x128 = {{0}};
   if (sizeof(T) == 8) {
     vx128.w64[lane] = vx;
     x128.w64[lane] = x;
   } else if (sizeof(T) == 4) {
     vx128.w32[lane] = vx;
     x128.w32[lane] = x;
   } else if (sizeof(T) == 2) {
     vx128.w16[lane] = vx;
     x128.w16[lane] = x;
   } else {
     vx128.w8[lane] = vx;
     x128.w8[lane] = x;
   }
   set_vbits(&x128, vx128);
   return x128;
}

template <typename T, int lane>
static void doit(const char *x, const char *y, bool expected_undefined, const char *err_msg) {
  int result = cmpGT<T>(string_to_vbits<T>(x, lane),
                        string_to_vbits<T>(y, lane),
                        lane);
  int undefined = VALGRIND_CHECK_VALUE_IS_DEFINED(result);
  if (!!undefined != expected_undefined) {
    fprintf(stderr, "ERROR: ");
  }
  // Force the result to be used.
  if (result) {
    fflush(stderr);
  }
  fprintf(stderr, "%s > %s, %s, %d == %d\n", x, y, err_msg, !!undefined, !!expected_undefined);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "First and only argument ought to be \"amd64\" or \"x86\"\n");
    exit(1);
  }
  bool amd64;
  if (strcmp(argv[1], "amd64") == 0) {
    amd64 = true;
  } else if (strcmp(argv[1], "x86") == 0) {
    amd64 = false;
  } else {
    fprintf(stderr, "First and only argument ought to be \"amd64\" or \"x86\"\n");
    exit(1);
  }
  if (amd64) {
    doit<uint64_t, 0>("xxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxx", true, "completely undefined, error above");
    doit<uint64_t, 0>("0000000000000000", "0000000000000000", false, "completely defined");
    doit<uint64_t, 0>("0000000000000000", "f000000000000000", false, "completely defined");
    doit<uint64_t, 0>("f000000000000000", "0000000000000000", false, "completely defined");
    doit<uint64_t, 0>("0000000000000000", "fxxxxxxxxxxxxxxx", false, "defined: 0 > all negatives");
    doit<uint64_t, 0>("0xxxxxxxxxxxxxxx", "fxxxxxxxxxxxxxxx", false, "defined: non-negatives > all negatives");
    doit<uint64_t, 0>("xxxxxxxxxxxxxxx0", "f000000000000000", true, "undefined, error above");
    doit<uint64_t, 0>("xxxxxxxxxxxxxxx1", "8000000000000000", false, "defined: ends with 1 > MIN_INT");
    doit<uint64_t, 0>("5xxxxxxxxxxxxxxx", "6xxxxxxxxxxxxxxx", false, "defined");
    doit<uint64_t, 0>("8xxxxxxxxxxxxxxx", "9xxxxxxxxxxxxxxx", false, "defined");
    doit<uint64_t, 0>("123456781234567x", "1234567812345678", true, "undefined, error above");
    doit<uint64_t, 0>("123456781234567x", "123456781234567f", false, "defined: x can't be more than f");
    doit<uint64_t, 0>("123456781234567x", "123456781234567e", true, "undefined: x can be more than e, error above");

    doit<uint64_t, 1>("xxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxx", true, "completely undefined, error above");
    doit<uint64_t, 1>("0000000000000000", "0000000000000000", false, "completely defined");
    doit<uint64_t, 1>("0000000000000000", "f000000000000000", false, "completely defined");
    doit<uint64_t, 1>("f000000000000000", "0000000000000000", false, "completely defined");
    doit<uint64_t, 1>("0000000000000000", "fxxxxxxxxxxxxxxx", false, "defined: 0 > all negatives");
    doit<uint64_t, 1>("0xxxxxxxxxxxxxxx", "fxxxxxxxxxxxxxxx", false, "defined: non-negatives > all negatives");
    doit<uint64_t, 1>("xxxxxxxxxxxxxxx0", "f000000000000000", true, "undefined, error above");
    doit<uint64_t, 1>("xxxxxxxxxxxxxxx1", "8000000000000000", false, "defined: ends with 1 > MIN_INT");
    doit<uint64_t, 1>("5xxxxxxxxxxxxxxx", "6xxxxxxxxxxxxxxx", false, "defined");
    doit<uint64_t, 1>("8xxxxxxxxxxxxxxx", "9xxxxxxxxxxxxxxx", false, "defined");
    doit<uint64_t, 1>("123456781234567x", "1234567812345678", true, "undefined, error above");
    doit<uint64_t, 1>("123456781234567x", "123456781234567f", false, "defined: x can't be more than f");
    doit<uint64_t, 1>("123456781234567x", "123456781234567e", true, "undefined: x can be more than e, error above");
  }
  doit<uint32_t, 0>("xxxxxxxx", "xxxxxxxx", true, "completely undefined, error above");
  doit<uint32_t, 0>("00000000", "00000000", false, "completely defined");
  doit<uint32_t, 0>("00000000", "f0000000", false, "completely defined");
  doit<uint32_t, 0>("f0000000", "00000000", false, "completely defined");
  doit<uint32_t, 0>("00000000", "fxxxxxxx", false, "defined: 0 > all negatives");
  doit<uint32_t, 0>("0xxxxxxx", "fxxxxxxx", false, "defined: non-negatives > all negatives");
  doit<uint32_t, 0>("xxxxxxx0", "f0000000", true, "undefined, error above");
  doit<uint32_t, 0>("xxxxxxx1", "80000000", false, "defined: ends with 1 > MIN_INT");
  doit<uint32_t, 0>("5xxxxxxx", "6xxxxxxx", false, "defined");
  doit<uint32_t, 0>("8xxxxxxx", "9xxxxxxx", false, "defined");
  doit<uint32_t, 0>("1234567x", "12345678", true, "undefined, error above");
  doit<uint32_t, 0>("1234567x", "1234567f", false, "defined: x can't be more than f");
  doit<uint32_t, 0>("1234567x", "1234567e", true, "undefined: x can be more than e, error above");

  doit<uint32_t, 1>("xxxxxxxx", "xxxxxxxx", true, "completely undefined, error above");
  doit<uint32_t, 1>("00000000", "00000000", false, "completely defined");
  doit<uint32_t, 1>("00000000", "f0000000", false, "completely defined");
  doit<uint32_t, 1>("f0000000", "00000000", false, "completely defined");
  doit<uint32_t, 1>("00000000", "fxxxxxxx", false, "defined: 0 > all negatives");
  doit<uint32_t, 1>("0xxxxxxx", "fxxxxxxx", false, "defined: non-negatives > all negatives");
  doit<uint32_t, 1>("xxxxxxx0", "f0000000", true, "undefined, error above");
  doit<uint32_t, 1>("xxxxxxx1", "80000000", false, "defined: ends with 1 > MIN_INT");
  doit<uint32_t, 1>("5xxxxxxx", "6xxxxxxx", false, "defined");
  doit<uint32_t, 1>("8xxxxxxx", "9xxxxxxx", false, "defined");
  doit<uint32_t, 1>("1234567x", "12345678", true, "undefined, error above");
  doit<uint32_t, 1>("1234567x", "1234567f", false, "defined: x can't be more than f");
  doit<uint32_t, 1>("1234567x", "1234567e", true, "undefined: x can be more than e, error above");

  doit<uint32_t, 2>("xxxxxxxx", "xxxxxxxx", true, "completely undefined, error above");
  doit<uint32_t, 2>("00000000", "00000000", false, "completely defined");
  doit<uint32_t, 2>("00000000", "f0000000", false, "completely defined");
  doit<uint32_t, 2>("f0000000", "00000000", false, "completely defined");
  doit<uint32_t, 2>("00000000", "fxxxxxxx", false, "defined: 0 > all negatives");
  doit<uint32_t, 2>("0xxxxxxx", "fxxxxxxx", false, "defined: non-negatives > all negatives");
  doit<uint32_t, 2>("xxxxxxx0", "f0000000", true, "undefined, error above");
  doit<uint32_t, 2>("xxxxxxx1", "80000000", false, "defined: ends with 1 > MIN_INT");
  doit<uint32_t, 2>("5xxxxxxx", "6xxxxxxx", false, "defined");
  doit<uint32_t, 2>("8xxxxxxx", "9xxxxxxx", false, "defined");
  doit<uint32_t, 2>("1234567x", "12345678", true, "undefined, error above");
  doit<uint32_t, 2>("1234567x", "1234567f", false, "defined: x can't be more than f");
  doit<uint32_t, 2>("1234567x", "1234567e", true, "undefined: x can be more than e, error above");

  doit<uint32_t, 3>("xxxxxxxx", "xxxxxxxx", true, "completely undefined, error above");
  doit<uint32_t, 3>("00000000", "00000000", false, "completely defined");
  doit<uint32_t, 3>("00000000", "f0000000", false, "completely defined");
  doit<uint32_t, 3>("f0000000", "00000000", false, "completely defined");
  doit<uint32_t, 3>("00000000", "fxxxxxxx", false, "defined: 0 > all negatives");
  doit<uint32_t, 3>("0xxxxxxx", "fxxxxxxx", false, "defined: non-negatives > all negatives");
  doit<uint32_t, 3>("xxxxxxx0", "f0000000", true, "undefined, error above");
  doit<uint32_t, 3>("xxxxxxx1", "80000000", false, "defined: ends with 1 > MIN_INT");
  doit<uint32_t, 3>("5xxxxxxx", "6xxxxxxx", false, "defined");
  doit<uint32_t, 3>("8xxxxxxx", "9xxxxxxx", false, "defined");
  doit<uint32_t, 3>("1234567x", "12345678", true, "undefined, error above");
  doit<uint32_t, 3>("1234567x", "1234567f", false, "defined: x can't be more than f");
  doit<uint32_t, 3>("1234567x", "1234567e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 0>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 0>("0000", "0000", false, "completely defined");
  doit<uint16_t, 0>("0000", "f000", false, "completely defined");
  doit<uint16_t, 0>("f000", "0000", false, "completely defined");
  doit<uint16_t, 0>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 0>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 0>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 0>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 0>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 0>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 0>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 0>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 0>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 1>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 1>("0000", "0000", false, "completely defined");
  doit<uint16_t, 1>("0000", "f000", false, "completely defined");
  doit<uint16_t, 1>("f000", "0000", false, "completely defined");
  doit<uint16_t, 1>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 1>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 1>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 1>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 1>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 1>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 1>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 1>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 1>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 2>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 2>("0000", "0000", false, "completely defined");
  doit<uint16_t, 2>("0000", "f000", false, "completely defined");
  doit<uint16_t, 2>("f000", "0000", false, "completely defined");
  doit<uint16_t, 2>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 2>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 2>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 2>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 2>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 2>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 2>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 2>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 2>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 3>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 3>("0000", "0000", false, "completely defined");
  doit<uint16_t, 3>("0000", "f000", false, "completely defined");
  doit<uint16_t, 3>("f000", "0000", false, "completely defined");
  doit<uint16_t, 3>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 3>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 3>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 3>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 3>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 3>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 3>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 3>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 3>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 4>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 4>("0000", "0000", false, "completely defined");
  doit<uint16_t, 4>("0000", "f000", false, "completely defined");
  doit<uint16_t, 4>("f000", "0000", false, "completely defined");
  doit<uint16_t, 4>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 4>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 4>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 4>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 4>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 4>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 4>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 4>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 4>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 5>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 5>("0000", "0000", false, "completely defined");
  doit<uint16_t, 5>("0000", "f000", false, "completely defined");
  doit<uint16_t, 5>("f000", "0000", false, "completely defined");
  doit<uint16_t, 5>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 5>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 5>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 5>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 5>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 5>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 5>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 5>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 5>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 6>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 6>("0000", "0000", false, "completely defined");
  doit<uint16_t, 6>("0000", "f000", false, "completely defined");
  doit<uint16_t, 6>("f000", "0000", false, "completely defined");
  doit<uint16_t, 6>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 6>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 6>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 6>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 6>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 6>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 6>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 6>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 6>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint16_t, 7>("xxxx", "xxxx", true, "completely undefined, error above");
  doit<uint16_t, 7>("0000", "0000", false, "completely defined");
  doit<uint16_t, 7>("0000", "f000", false, "completely defined");
  doit<uint16_t, 7>("f000", "0000", false, "completely defined");
  doit<uint16_t, 7>("0000", "fxxx", false, "defined: 0 > all negatives");
  doit<uint16_t, 7>("0xxx", "fxxx", false, "defined: non-negatives > all negatives");
  doit<uint16_t, 7>("xxx0", "f000", true, "undefined, error above");
  doit<uint16_t, 7>("xxx1", "8000", false, "defined: ends with 1 > MIN_INT");
  doit<uint16_t, 7>("5xxx", "6xxx", false, "defined");
  doit<uint16_t, 7>("8xxx", "9xxx", false, "defined");
  doit<uint16_t, 7>("123x", "1234", true, "undefined, error above");
  doit<uint16_t, 7>("123x", "123f", false, "defined: x can't be more than f");
  doit<uint16_t, 7>("123x", "123e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 0>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 0>("00", "00", false, "completely defined");
  doit<uint8_t, 0>("00", "f0", false, "completely defined");
  doit<uint8_t, 0>("f0", "00", false, "completely defined");
  doit<uint8_t, 0>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 0>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 0>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 0>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 0>("5x", "6x", false, "defined");
  doit<uint8_t, 0>("8x", "9x", false, "defined");
  doit<uint8_t, 0>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 0>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 0>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 1>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 1>("00", "00", false, "completely defined");
  doit<uint8_t, 1>("00", "f0", false, "completely defined");
  doit<uint8_t, 1>("f0", "00", false, "completely defined");
  doit<uint8_t, 1>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 1>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 1>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 1>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 1>("5x", "6x", false, "defined");
  doit<uint8_t, 1>("8x", "9x", false, "defined");
  doit<uint8_t, 1>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 1>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 1>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 2>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 2>("00", "00", false, "completely defined");
  doit<uint8_t, 2>("00", "f0", false, "completely defined");
  doit<uint8_t, 2>("f0", "00", false, "completely defined");
  doit<uint8_t, 2>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 2>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 2>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 2>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 2>("5x", "6x", false, "defined");
  doit<uint8_t, 2>("8x", "9x", false, "defined");
  doit<uint8_t, 2>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 2>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 2>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 3>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 3>("00", "00", false, "completely defined");
  doit<uint8_t, 3>("00", "f0", false, "completely defined");
  doit<uint8_t, 3>("f0", "00", false, "completely defined");
  doit<uint8_t, 3>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 3>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 3>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 3>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 3>("5x", "6x", false, "defined");
  doit<uint8_t, 3>("8x", "9x", false, "defined");
  doit<uint8_t, 3>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 3>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 3>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 4>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 4>("00", "00", false, "completely defined");
  doit<uint8_t, 4>("00", "f0", false, "completely defined");
  doit<uint8_t, 4>("f0", "00", false, "completely defined");
  doit<uint8_t, 4>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 4>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 4>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 4>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 4>("5x", "6x", false, "defined");
  doit<uint8_t, 4>("8x", "9x", false, "defined");
  doit<uint8_t, 4>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 4>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 4>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 5>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 5>("00", "00", false, "completely defined");
  doit<uint8_t, 5>("00", "f0", false, "completely defined");
  doit<uint8_t, 5>("f0", "00", false, "completely defined");
  doit<uint8_t, 5>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 5>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 5>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 5>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 5>("5x", "6x", false, "defined");
  doit<uint8_t, 5>("8x", "9x", false, "defined");
  doit<uint8_t, 5>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 5>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 5>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 6>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 6>("00", "00", false, "completely defined");
  doit<uint8_t, 6>("00", "f0", false, "completely defined");
  doit<uint8_t, 6>("f0", "00", false, "completely defined");
  doit<uint8_t, 6>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 6>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 6>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 6>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 6>("5x", "6x", false, "defined");
  doit<uint8_t, 6>("8x", "9x", false, "defined");
  doit<uint8_t, 6>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 6>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 6>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 7>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 7>("00", "00", false, "completely defined");
  doit<uint8_t, 7>("00", "f0", false, "completely defined");
  doit<uint8_t, 7>("f0", "00", false, "completely defined");
  doit<uint8_t, 7>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 7>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 7>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 7>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 7>("5x", "6x", false, "defined");
  doit<uint8_t, 7>("8x", "9x", false, "defined");
  doit<uint8_t, 7>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 7>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 7>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 8>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 8>("00", "00", false, "completely defined");
  doit<uint8_t, 8>("00", "f0", false, "completely defined");
  doit<uint8_t, 8>("f0", "00", false, "completely defined");
  doit<uint8_t, 8>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 8>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 8>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 8>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 8>("5x", "6x", false, "defined");
  doit<uint8_t, 8>("8x", "9x", false, "defined");
  doit<uint8_t, 8>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 8>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 8>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 9>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 9>("00", "00", false, "completely defined");
  doit<uint8_t, 9>("00", "f0", false, "completely defined");
  doit<uint8_t, 9>("f0", "00", false, "completely defined");
  doit<uint8_t, 9>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 9>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 9>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 9>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 9>("5x", "6x", false, "defined");
  doit<uint8_t, 9>("8x", "9x", false, "defined");
  doit<uint8_t, 9>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 9>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 9>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 10>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 10>("00", "00", false, "completely defined");
  doit<uint8_t, 10>("00", "f0", false, "completely defined");
  doit<uint8_t, 10>("f0", "00", false, "completely defined");
  doit<uint8_t, 10>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 10>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 10>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 10>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 10>("5x", "6x", false, "defined");
  doit<uint8_t, 10>("8x", "9x", false, "defined");
  doit<uint8_t, 10>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 10>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 10>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 11>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 11>("00", "00", false, "completely defined");
  doit<uint8_t, 11>("00", "f0", false, "completely defined");
  doit<uint8_t, 11>("f0", "00", false, "completely defined");
  doit<uint8_t, 11>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 11>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 11>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 11>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 11>("5x", "6x", false, "defined");
  doit<uint8_t, 11>("8x", "9x", false, "defined");
  doit<uint8_t, 11>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 11>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 11>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 12>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 12>("00", "00", false, "completely defined");
  doit<uint8_t, 12>("00", "f0", false, "completely defined");
  doit<uint8_t, 12>("f0", "00", false, "completely defined");
  doit<uint8_t, 12>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 12>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 12>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 12>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 12>("5x", "6x", false, "defined");
  doit<uint8_t, 12>("8x", "9x", false, "defined");
  doit<uint8_t, 12>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 12>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 12>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 13>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 13>("00", "00", false, "completely defined");
  doit<uint8_t, 13>("00", "f0", false, "completely defined");
  doit<uint8_t, 13>("f0", "00", false, "completely defined");
  doit<uint8_t, 13>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 13>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 13>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 13>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 13>("5x", "6x", false, "defined");
  doit<uint8_t, 13>("8x", "9x", false, "defined");
  doit<uint8_t, 13>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 13>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 13>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 14>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 14>("00", "00", false, "completely defined");
  doit<uint8_t, 14>("00", "f0", false, "completely defined");
  doit<uint8_t, 14>("f0", "00", false, "completely defined");
  doit<uint8_t, 14>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 14>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 14>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 14>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 14>("5x", "6x", false, "defined");
  doit<uint8_t, 14>("8x", "9x", false, "defined");
  doit<uint8_t, 14>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 14>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 14>("1x", "1e", true, "undefined: x can be more than e, error above");

  doit<uint8_t, 15>("xx", "xx", true, "completely undefined, error above");
  doit<uint8_t, 15>("00", "00", false, "completely defined");
  doit<uint8_t, 15>("00", "f0", false, "completely defined");
  doit<uint8_t, 15>("f0", "00", false, "completely defined");
  doit<uint8_t, 15>("00", "fx", false, "defined: 0 > all negatives");
  doit<uint8_t, 15>("0x", "fx", false, "defined: non-negatives > all negatives");
  doit<uint8_t, 15>("x0", "f0", true, "undefined, error above");
  doit<uint8_t, 15>("x1", "80", false, "defined: ends with 1 > MIN_INT");
  doit<uint8_t, 15>("5x", "6x", false, "defined");
  doit<uint8_t, 15>("8x", "9x", false, "defined");
  doit<uint8_t, 15>("1x", "12", true, "undefined, error above");
  doit<uint8_t, 15>("1x", "1f", false, "defined: x can't be more than f");
  doit<uint8_t, 15>("1x", "1e", true, "undefined: x can be more than e, error above");


  return 0;
}
