#define _GNU_SOURCE
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "../../config.h"

/* mallocs an mem block and fills it with A. A needs to be a zero
   terminated string. The A string chars, minus the terminating zero
   are copied into the returned mem block.  */
static void *
create_mem (const char *a)
{
  size_t len = strlen (a);
  void *mem = malloc (len);
  memcpy (mem, a, len);
  return mem;
}

int
main ()
{
  char *haystack;
  char *needle;

  haystack = create_mem ("a");
  needle = create_mem ("a");
#if defined(VGO_darwin) || defined(VGO_solaris)
  /*
   * macOS manpage says
   * If big_len is smaller than little_len, if little_len is 0, if big_len is 0 or if
   * little occurs nowhere in big, NULL is returned; otherwise a pointer to the first
   * character of the first occurrence of little is returned.
   */
  assert (memmem (haystack, 0, needle, 0) == NULL);
  assert (memmem (haystack, 1, needle, 0) == NULL);
#else
  assert (memmem (haystack, 0, needle, 0) == haystack);
  assert (memmem (haystack, 1, needle, 0) == haystack);
#endif
  assert (memmem (haystack, 0, needle, 1) == NULL);
  assert (memmem (haystack, 1, needle, 1) == haystack);
  free (haystack);
  free (needle);

  haystack = create_mem ("abc");
  needle = create_mem ("bc");
#if defined(VGO_darwin) || defined(VGO_solaris)
  assert (memmem (haystack, 3, needle, 0) == NULL);
#else
  assert (memmem (haystack, 3, needle, 0) == haystack);
#endif
  assert (memmem (haystack, 3, needle, 2) == haystack + 1);
  assert (memmem (haystack + 1, 2, needle, 2) == haystack + 1);
  assert (memmem (haystack + 2, 1, needle, 2) == NULL);
  free (haystack);
  free (needle);

  haystack = create_mem ("abcabcabc");
  needle = create_mem ("bca");
  assert (memmem (haystack, 9, needle, 3) == haystack + 1);
  free (haystack);
  free (needle);

  haystack = create_mem ("abcabcabc");
  needle = create_mem ("bcad");
  assert (memmem (haystack, 9, needle, 4) == NULL);
  free (haystack);
  free (needle);

  haystack = create_mem ("xxxxxxxxxxxxxxxxxABC");
  needle = create_mem ("ABCD");
  assert (memmem (haystack, 20, needle, 2) == haystack + 17);
  assert (memmem (haystack + 3, 17, needle, 2) == haystack + 17);
  assert (memmem (haystack + 15, 5, needle, 2) == haystack + 17);
  assert (memmem (haystack, 20, needle, 3) == haystack + 17);
  assert (memmem (haystack + 3, 17, needle, 3) == haystack + 17);
  assert (memmem (haystack + 15, 5, needle, 3) == haystack + 17);
  assert (memmem (haystack, 20, needle, 4) == NULL);
  assert (memmem (haystack + 3, 5, needle, 4) == NULL);
  assert (memmem (haystack + 15, 5, needle, 4) == NULL);
  free (haystack);
  free (needle);

  return 0;
}
