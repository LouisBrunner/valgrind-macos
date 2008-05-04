#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
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
UInt VG_(message)(VgMsgKind kind, const HChar* format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vprintf(format, vargs); va_end(vargs); printf("\n"); return ret; }
Bool drd_is_suppressed(const Addr a1, const Addr a2)
{ assert(0); }


/* Actual unit test */

static int s_verbose = 1;

static
struct { Addr address; SizeT size; BmAccessTypeT access_type; }
  s_test1_args[] = {
    {                           0, 1, eLoad  },
    {                         666, 4, eLoad  },
    {                         667, 2, eStore },
    {                        1024, 1, eStore },
    {                   0xffffULL, 1, eStore },
    {               0x0001ffffULL, 1, eLoad  },
    {               0x00ffffffULL, 1, eLoad  },
    { 0xfffffffeULL - ADDR0_COUNT, 1, eStore },
#if defined(VGP_amd64_linux) || defined(VGP_ppc64_linux)
    { 0xffffffffULL - ADDR0_COUNT, 1, eStore },
    {               0xffffffffULL, 1, eStore },
    {              0x100000000ULL, 1, eStore },
    {         -2ULL - ADDR0_COUNT, 1, eStore },
#endif
  };

void bm_test1(void)
{
  struct bitmap* bm;
  struct bitmap* bm2;
  unsigned i, j;

  bm = bm_new();

  for (i = 0; i < sizeof(s_test1_args)/sizeof(s_test1_args[0]); i++)
  {
    bm_access_range(bm,
                    s_test1_args[i].address,
                    s_test1_args[i].address + s_test1_args[i].size,
                    s_test1_args[i].access_type);
  }

  if (s_verbose)
  {
    VG_(printf)("Bitmap contents:\n");
    bm_print(bm);
  }

  for (i = 0; i < sizeof(s_test1_args)/sizeof(s_test1_args[0]); i++)
  {
    for (j = 0; j < s_test1_args[i].size; j++)
    {
      tl_assert(bm_has_1(bm,
                         s_test1_args[i].address + j,
                         s_test1_args[i].access_type));
    }
  }

  if (s_verbose)
    VG_(printf)("Merge result:\n");
  bm2 = bm_new();
  bm_merge2(bm2, bm);
  bm_merge2(bm2, bm);
  if (s_verbose)
    bm_print(bm2);
  //assert(bm_equal(bm, bm2));
  assert(bm_equal(bm2, bm));

  if (s_verbose)
    VG_(printf)("Deleting bitmap bm\n");
  bm_delete(bm);
  if (s_verbose)
    VG_(printf)("Deleting bitmap bm2\n");
  bm_delete(bm2);
}

/* Test whether bm_equal() works correctly. */
void bm_test2()
{
  struct bitmap* bm1;
  struct bitmap* bm2;

  bm1 = bm_new();
  bm2 = bm_new();
  bm_access_load_1(bm1, 7);
  bm_access_load_1(bm2, ADDR0_COUNT + 7);
  assert(! bm_equal(bm1, bm2));
  assert(! bm_equal(bm2, bm1));
  bm_access_load_1(bm2, 7);
  assert(! bm_equal(bm1, bm2));
  assert(! bm_equal(bm2, bm1));
  bm_access_store_1(bm1, ADDR0_COUNT + 7);
  assert(! bm_equal(bm1, bm2));
  assert(! bm_equal(bm2, bm1));
  bm_delete(bm1);
  bm_delete(bm2);
}

int main(int argc, char** argv)
{
  int optchar;

  while ((optchar = getopt(argc, argv, "q")) != EOF)
  {
    switch (optchar)
    {
    case 'q':
      s_verbose = 0;
      break;
    default:
      fprintf(stderr, "Usage: %s [-q].\n", argv[0]);
      break;
    }
  }

  VG_(printf)("Start of DRD BM unit test.\n");

  bm_test1();
  bm_test2();

  VG_(printf)("End of DRD BM unit test.\n");

  return 0;
}
