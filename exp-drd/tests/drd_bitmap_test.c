#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "coregrind/m_oset.c"
#include "exp-drd/drd_bitmap.c"
#include "exp-drd/pub_drd_bitmap.h"


/* Replacements for core functionality. */

void* VG_(malloc)(SizeT nbytes)
{ return malloc(nbytes); }
void  VG_(free)(void* p)
{ return free(p); }
void  VG_(assert_fail)(Bool isCore, const Char* expr, const Char* file,
                       Int line, const Char* fn, const HChar* format, ...)
{ __assert_fail(expr, file, line, fn); abort(); }
void* VG_(memset)(void *s, Int c, SizeT sz)
{ return memset(s, c, sz); }
void* VG_(memcpy)(void *d, const void *s, SizeT sz)
{ return memcpy(d, s, sz); }
Int VG_(memcmp)(const void* s1, const void* s2, SizeT n)
{ return memcmp(s1, s2, n); }
UInt VG_(printf)(const HChar *format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vprintf(format, vargs); va_end(vargs); return ret; }
Bool drd_is_suppressed(const Addr a1, const Addr a2)
{ assert(0); }


/* Unit test */
static
struct { Addr address; SizeT size; BmAccessTypeT access_type; }
  s_args[] = {
    {            0, 1, eLoad  },
    {          666, 4, eLoad  },
    {          667, 2, eStore },
    {         1024, 1, eStore },
    {     0xffffUL, 1, eStore },
    { 0x0001ffffUL, 1, eLoad  },
    { 0x00ffffffUL, 1, eLoad  },
    { 0xffffffffUL, 1, eStore },
  };

void bm_test(void)
{
  struct bitmap* bm;
  struct bitmap* bm2;
  unsigned i, j;

  VG_(printf)("Start of DRD BM unit test.\n");

  bm = bm_new();

  for (i = 0; i < sizeof(s_args)/sizeof(s_args[0]); i++)
  {
    bm_access_range(bm,
                    s_args[i].address,
                    s_args[i].address + s_args[i].size,
                    s_args[i].access_type);
  }

  VG_(printf)("Map contents -- should contain 10 addresses:\n");
  bm_print(bm);

  for (i = 0; i < sizeof(s_args)/sizeof(s_args[0]); i++)
  {
    for (j = 0; j < s_args[i].size; j++)
    {
      tl_assert(bm_has_1(bm, s_args[i].address + j, s_args[i].access_type));
    }
  }

  VG_(printf)("Merge result:\n");
  bm2 = bm_new();
  bm_merge2(bm2, bm);
  bm_merge2(bm2, bm);
  bm_print(bm);

  VG_(printf)("Deleting bitmap bm\n");
  bm_delete(bm);
  VG_(printf)("Deleting bitmap bm2\n");
  bm_delete(bm2);

  VG_(printf)("End of DRD BM unit test.\n");
}

int main(int argc, char** argv)
{
  bm_test();
  return 0;
}
