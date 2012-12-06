// Uses various wchar_t * functions that have hand written SSE assembly
// implementations in glibc. wcslen, wcscpy, wcscmp, wcsrchr, wcschr.

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

int main(int argc, char **argv)
{
  wchar_t a[] = L"The spazzy orange tiger jumped over the tawny jaguar.";
  wchar_t *b, *c;
  wchar_t *d, *e;

  size_t l = wcslen (a);
  fprintf (stderr, "wcslen: %zd\n", l); // wcslen: 53

  b = (wchar_t *) malloc((l + 1) * sizeof (wchar_t));
  c = wcscpy (b, a);

  fprintf (stderr, "wcscmp equal: %d\n", wcscmp (a, b)); // wcscmp equal: 0

  d = wcsrchr (a, L'd');
  e = wcschr (a, L'd');

  fprintf (stderr, "wcsrchr == wcschr: %d\n", d == e); // wcsrchr == wcschr: 1

  free (c); // b == c
  return 0;
}
