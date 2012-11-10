/** @brief Unit-test for DRD's vector clock implementation. */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "drd/drd_vc.c"


/* Replacements for Valgrind core functionality. */

void* VG_(malloc)(const HChar* cc, SizeT nbytes)
{ return malloc(nbytes); }
void* VG_(realloc)(const HChar* cc, void* p, SizeT size)
{ return realloc(p, size); }
void  VG_(free)(void* p)
{ return free(p); }
void  VG_(assert_fail)(Bool isCore, const HChar* assertion, const HChar* file,
                       Int line, const HChar* function, const HChar* format,
                       ...)
{
  fprintf(stderr,
          "%s:%u: %s%sAssertion `%s' failed.\n",
          file,
          line,
          function ? (char*)function : "",
          function ? ": " : "",
          assertion);
  fflush(stdout);
  fflush(stderr);
  abort();
}

void* VG_(memset)(void *s, Int c, SizeT sz)
{ return memset(s, c, sz); }
void* VG_(memcpy)(void *d, const void *s, SizeT sz)
{ return memcpy(d, s, sz); }
Int VG_(memcmp)(const void* s1, const void* s2, SizeT n)
{ return memcmp(s1, s2, n); }
UInt VG_(printf)(const HChar *format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vprintf(format, vargs); va_end(vargs); return ret; }
UInt VG_(snprintf)(HChar* buf, Int size, const HChar *format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vsnprintf(buf, size, format, vargs); va_end(vargs); return ret; }
SizeT VG_(strlen)(const HChar* str) { return strlen(str); }
UInt VG_(message)(VgMsgKind kind, const HChar* format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vprintf(format, vargs); va_end(vargs); printf("\n"); return ret; }
Bool DRD_(is_suppressed)(const Addr a1, const Addr a2)
{ assert(0); }


/* Actual unit test */

static void vc_unittest(void)
{
  int i;
  char *str;
  VectorClock vc1;
  VCElem vc1elem[] = { { 3, 7 }, { 5, 8 }, };
  VectorClock vc2;
  VCElem vc2elem[] = { { 1, 4 }, { 3, 9 }, };
  VectorClock vc3;
  VCElem vc4elem[] = { { 1, 3 }, { 2, 1 }, };
  VectorClock vc4;
  VCElem vc5elem[] = { { 1, 4 }, };
  VectorClock vc5;

  DRD_(vc_init)(&vc1, vc1elem, sizeof(vc1elem)/sizeof(vc1elem[0]));
  DRD_(vc_init)(&vc2, vc2elem, sizeof(vc2elem)/sizeof(vc2elem[0]));
  DRD_(vc_init)(&vc3, 0, 0);
  DRD_(vc_init)(&vc4, vc4elem, sizeof(vc4elem)/sizeof(vc4elem[0]));
  DRD_(vc_init)(&vc5, vc5elem, sizeof(vc5elem)/sizeof(vc5elem[0]));

  DRD_(vc_combine)(&vc3, &vc1);
  DRD_(vc_combine)(&vc3, &vc2);

  fprintf(stderr, "vc1: %s", (str = DRD_(vc_aprint)(&vc1)));
  free(str);
  fprintf(stderr, "\nvc2: %s", (str = DRD_(vc_aprint)(&vc2)));
  free(str);
  fprintf(stderr, "\nvc3: %s", (str = DRD_(vc_aprint)(&vc3)));
  free(str);
  fprintf(stderr, "\n");
  fprintf(stderr, "vc_lte(vc1, vc2) = %d, vc_lte(vc1, vc3) = %d,"
          " vc_lte(vc2, vc3) = %d\nvc_lte(",
          DRD_(vc_lte)(&vc1, &vc2), DRD_(vc_lte)(&vc1, &vc3),
          DRD_(vc_lte)(&vc2, &vc3));
  fprintf(stderr, "%s", (str = DRD_(vc_aprint)(&vc4)));
  free(str);
  fprintf(stderr, ", ");
  fprintf(stderr, "%s", (str = DRD_(vc_aprint)(&vc5)));
  free(str);
  fprintf(stderr, ") = %d sw %d\n",
          DRD_(vc_lte)(&vc4, &vc5), DRD_(vc_lte)(&vc5, &vc4));

  for (i = 0; i < 64; i++)
    DRD_(vc_reserve)(&vc1, i);
  for (i = 64; i > 0; i--)
    DRD_(vc_reserve)(&vc1, i);

  DRD_(vc_cleanup)(&vc1);
  DRD_(vc_cleanup)(&vc2);
  DRD_(vc_cleanup)(&vc3);
}

int main(int argc, char** argv)
{
  vc_unittest();
  return 0;
}
