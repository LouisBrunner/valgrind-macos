// This tests --mode=copy with various copying functions.

#define _GNU_SOURCE // For mempcpy.
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <strings.h>
#include "../../config.h"

void f(char* a, char* b, wchar_t* wa, wchar_t* wb);
void test_malloc();

int main(void) {
   char a[1000];
   char b[1000];
   for (int i = 0; i < 1000; i++) {
      a[i] = 'a';
      b[i] = 'b';
   }
   a[999] = '\0';
   b[999] = '\0';

   wchar_t wa[250];
   wchar_t wb[250];
   for (int i = 0; i < 250; i++) {
      wa[i] = L'A';
      wb[i] = L'B';
   }
   wa[249] = L'\0';
   wb[249] = L'\0';

   for (int i = 0; i < 100; i++) {
      f(a, b, wa, wb);
   }

   test_malloc();
   return 0;
}

void f(char* a, char* b, wchar_t* wa, wchar_t* wb) {
   // The memcpy is duplicated so we have 10 calls
   // which was a nice round 100,000 until wcpncpy
   // and wcsncpy were added
   memcpy (a, b, 1000); // Redirects to memmove
   memcpy (a, b, 1000); // Redirects to memmove
   memmove(a, b, 1000);
#if defined(HAVE_MEMPCPY)
   mempcpy(a, b, 1000);
#else
   memcpy(a, b, 1000);
#endif
   bcopy  (a, b, 1000); // Redirects to memmove
   strcpy (a, b);
   strncpy(a, b, 1000);
   stpcpy (a, b);       // Redirects to strcpy
   stpncpy(a, b, 1000);
   wcscpy (wa, wb);
#if defined(HAVE_WCPNCPY)
   wcpncpy(wa, wb, 1000/sizeof(*wa));
#else
   memcpy(a, b, 1000);
#endif
#if defined(HAVE_WCSNCPY)
    wcsncpy(wa, wb, 1000/sizeof(*wa));
#else
   memcpy(a, b, 1000);
#endif
}

void test_malloc() {
   // At one point malloc was broken with --mode=copy(!), and Valgrind was
   // printing messages like "VG_USERREQ__CLIENT_CALL1: func=0x0" when malloc
   // was called. So check that it's basically working...
   char* p = malloc(100);
   p = realloc(p, 200);
   free(p);
}
