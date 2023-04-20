#include <stdlib.h>
#include <wchar.h>

int main ()
{
  wchar_t *s = (wchar_t *) malloc (8 * sizeof (wchar_t));
  s[0] = '-';
  s[1] = 'N';
  s[2] = 'A';
  s[3] = 'N';
  s[4] = ' ';
  s[5] = '3';
  s[6] = '3';
  s[7] = '\0';
  return wmemcmp (s + 1, L"NAN", 3) == 0;
}

