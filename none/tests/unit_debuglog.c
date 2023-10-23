/* Test %f format specifier */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"

#undef vg_assert
#define vg_assert(e)                   assert(e)
#undef vg_assert2
#define vg_assert2(e, fmt, args...)    assert(e)

#include "coregrind/m_debuglog.c"

int run(const char *format, ...)
{
  int n, num_stars;
  const char *p;
  printf_buf buf;
  va_list vargs;

  // Count number of '*' in format
  num_stars = 0;
  for (p = format; *p; ++p)
    if (*p == '*') ++num_stars;

  va_start(vargs, format);
  fprintf(stderr, "%s\tprintf =   ", format);
  n = vfprintf(stderr, format, vargs);
  fprintf(stderr, "\twrote %3d chars\n", n);
  va_end(vargs);

  buf.n = 0;
  buf.buf[0] = 0;

  fprintf(stderr, "%s\tdebuglog = ", format);
  va_start(vargs, format);
  n = VG_(debugLog_vprintf)(add_to_buf, &buf, format, vargs);
  va_end(vargs);

  emit(buf.buf, strlen(buf.buf));
  fprintf(stderr, "\twrote %3d chars\n", n);

  return num_stars;
}

int main(int argc, char *argv[])
{
  double value;

  fprintf(stderr, "...testing value 0\n");
  value = 0.0;
  run("|%f|", value);
  run("|%2f|", value);
  run("|%9f|", value);
  run("|%8.0f|", value);
  run("|%8.1f|", value);
  run("|%8.2f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing value 3.7  (with rounding)\n");
  value = 3.7;
  run("|%f|", value);
  run("|%4f|", value);
  run("|%9f|", value);
  run("|%4.0f|", value);
  run("|%4.1f|", value);
  run("|%4.2f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing value 123.01\n");
  value = 123.01;
  run("|%f|", value);
  run("|%4f|", value);
  run("|%9f|", value);
  run("|%8.0f|", value);
  run("|%8.1f|", value);
  run("|%8.2f|", value);
  run("|%8.3f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing value 3.0019  (with rounding)\n");
  value = 3.0019;
  run("|%f|", value);
  run("|%10f|", value);
  run("|%10.0f|", value);
  run("|%10.3f|", value);
  run("|%10.4f|", value);
  run("|%.4f|", value);
  run("|%.9f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing value -123.456 (with rounding)\n");
  value = -123.456;
  run("|%f|", value);
  run("|%10f|", value);
  run("|%10.0f|", value);
  run("|%10.1f|", value);
  run("|%10.2f|", value);
  run("|%10.3f|", value);
  run("|%10.4f|", value);
  run("|%10.5f|", value);
  run("|%.4f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing value = -123.456 width = '*'\n");
  value = -123.456;
  run("|%*f|", 10, value);
  run("|%*f|", 2, value);
  run("|%*f.1|", 10, value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing precision = '*'\n");
  value = -123.456;
  run("|%.*f|", 10, value);
  run("|%.*f|", 2, value);
  run("|%10.*f|", 2, value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing width/precision = '*'\n");
  value = -123.456;
  run("|%*.*f|", 20, 5, value);
  run("|%*.*f|", 1, 4, value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing left justification\n");
  value = 3.1415;
  run("|%10f|", value);
  run("|%-10f|", value);

  fprintf(stderr, "\n");
  fprintf(stderr, "...testing strings\n");
  const char *str = "abcd";
  run("|%s|", str);
  run("|%9s|", str);
  run("|%-9s|", str);
  run("|%*s|", 6, str);
  
  fprintf(stderr, "\n");
  fprintf(stderr, "...testing integers\n");
  long long ival = -1004005;
  run("|%lld|", ival);
  //  runint("|%'lld|", ival);     // locale specific (LC_NUMERIC)
  run("|%15lld|", ival);
  run("|%-15lld|", ival);
  //  runint("|%'-15lld|", ival);  // locale specific (LC_NUMERIC)
  run("|%100lld|", ival);
  run("|%*lld|", 13, ival);

  value = 0.99685224;
  run("|%3.0f|", value);
  run("|%3.1f|", value);
  run("|%3.2f|", value);
  run("|%3.3f|", value);
  run("|%3.4f|", value);
  run("|%3.5f|", value);

  return 0;
}
