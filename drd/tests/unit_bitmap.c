/** @brief Unit-test for DRD's bitmap implementation. */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "coregrind/m_oset.c"
#include "drd/drd_bitmap.c"
#include "drd/drd_bitmap2_node.c"
#include "drd/pub_drd_bitmap.h"


#ifndef MIN
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif
#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif


/* Replacements for Valgrind core functionality. */

void* VG_(malloc)(HChar* cc, SizeT nbytes)
{ return malloc(nbytes); }
void  VG_(free)(void* p)
{ return free(p); }
void  VG_(assert_fail)(Bool isCore, const Char* assertion, const Char* file,
                       Int line, const Char* function, const HChar* format,
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
UInt VG_(message)(VgMsgKind kind, const HChar* format, ...)
{ UInt ret; va_list vargs; va_start(vargs, format); ret = vprintf(format, vargs); va_end(vargs); printf("\n"); return ret; }
Bool DRD_(is_suppressed)(const Addr a1, const Addr a2)
{ assert(0); }


/* Actual unit test */

static int s_verbose = 1;

static
struct { Addr address; SizeT size; BmAccessTypeT access_type; }
  s_test1_args[] = {
    {                           0, 0, eLoad  },
    {                           0, 1, eLoad  },
    {                         666, 4, eLoad  },
    {                         667, 2, eStore },
    {                        1024, 1, eStore },
    {                   0xffffULL, 1, eStore },
    {               0x0001ffffULL, 1, eLoad  },
    {               0x00ffffffULL, 1, eLoad  },
    { 0xffffffffULL - (((1 << ADDR_LSB_BITS) + 1) << ADDR_IGNORED_BITS),
                                   1, eStore },
#if defined(VGP_amd64_linux) || defined(VGP_ppc64_linux) || defined(VGP_ppc64_aix5)
    { 0xffffffffULL - (1 << ADDR_LSB_BITS << ADDR_IGNORED_BITS),
                                   1, eStore },
    {               0xffffffffULL, 1, eStore },
    {              0x100000000ULL, 1, eStore },
    { -2ULL - (1 << ADDR_LSB_BITS << ADDR_IGNORED_BITS),
                                   1, eStore },
#endif
  };

/**
 * Compare two bitmaps and if different, print the differences.
 */
int bm_equal_print_diffs(struct bitmap* bm1, struct bitmap* bm2)
{
  int equal;

  equal = DRD_(bm_equal)(bm1, bm2);
  if (s_verbose && ! equal)
  {
    unsigned i;

    VG_(printf)("Bitmaps are different.\n");
    for (i = 0; i < 0x10000; i++)
    {
      if (DRD_(bm_has_1)(bm1, i, eLoad) != DRD_(bm_has_1)(bm2, i, eLoad)
          || DRD_(bm_has_1)(bm1, i, eStore) != DRD_(bm_has_1)(bm2, i, eStore))
      {
        printf("0x%x %c %c %c %c\n",
               i,
               DRD_(bm_has_1)(bm1, i, eLoad)  ? 'R' : ' ',
               DRD_(bm_has_1)(bm1, i, eStore) ? 'W' : ' ',
               DRD_(bm_has_1)(bm2, i, eLoad)  ? 'R' : ' ',
               DRD_(bm_has_1)(bm2, i, eStore) ? 'W' : ' '
               );
      }
    }
    fflush(stdout);
  }

  return equal;
}

void bm_test1(void)
{
  struct bitmap* bm;
  struct bitmap* bm2;
  unsigned i, j;

  bm = DRD_(bm_new)();

  for (i = 0; i < sizeof(s_test1_args)/sizeof(s_test1_args[0]); i++)
  {
    DRD_(bm_access_range)(bm,
                          s_test1_args[i].address,
                          s_test1_args[i].address + s_test1_args[i].size,
                          s_test1_args[i].access_type);
  }

  for (i = 0; i < sizeof(s_test1_args)/sizeof(s_test1_args[0]); i++)
  {
    for (j = 0;
         first_address_with_higher_lsb(j) <= s_test1_args[i].size;
         j = first_address_with_higher_lsb(j))
    {
      tl_assert(DRD_(bm_has_1)(bm,
                               s_test1_args[i].address + j,
                               s_test1_args[i].access_type));
    }
  }

  bm2 = DRD_(bm_new)();
  DRD_(bm_merge2)(bm2, bm);
  DRD_(bm_merge2)(bm2, bm);
  assert(bm_equal_print_diffs(bm2, bm));

  if (s_verbose)
    VG_(printf)("Deleting bitmap bm\n");
  DRD_(bm_delete)(bm);
  if (s_verbose)
    VG_(printf)("Deleting bitmap bm2\n");
  DRD_(bm_delete)(bm2);
}

/** Test whether bm_equal() works correctly. */
void bm_test2()
{
  struct bitmap* bm1;
  struct bitmap* bm2;

  bm1 = DRD_(bm_new)();
  bm2 = DRD_(bm_new)();
  DRD_(bm_access_load_1)(bm1, 7);
  DRD_(bm_access_load_1)(bm2, make_address(1, 0) + 7);
  assert(! DRD_(bm_equal)(bm1, bm2));
  assert(! DRD_(bm_equal)(bm2, bm1));
  DRD_(bm_access_load_1)(bm2, 7);
  assert(! DRD_(bm_equal)(bm1, bm2));
  assert(! DRD_(bm_equal)(bm2, bm1));
  DRD_(bm_access_store_1)(bm1, make_address(1, 0) + 7);
  assert(! DRD_(bm_equal)(bm1, bm2));
  assert(! DRD_(bm_equal)(bm2, bm1));
  DRD_(bm_delete)(bm2);
  DRD_(bm_delete)(bm1);
}

/** Torture test of the functions that set or clear a range of bits. */
void bm_test3(const int outer_loop_step, const int inner_loop_step)
{
  unsigned i, j;
  struct bitmap* bm1;
  struct bitmap* bm2;

  const Addr lb = make_address(2, 0) - 2 * BITS_PER_UWORD;
  const Addr ub = make_address(2, 0) + 2 * BITS_PER_UWORD;

  assert(outer_loop_step >= 1);
  assert((outer_loop_step % ADDR_GRANULARITY) == 0);
  assert(inner_loop_step >= 1);
  assert((inner_loop_step % ADDR_GRANULARITY) == 0);

  bm1 = DRD_(bm_new)();
  bm2 = DRD_(bm_new)();
  for (i = lb; i < ub; i += outer_loop_step)
  {
    for (j = i + ADDR_GRANULARITY; j < ub; j += inner_loop_step)
    {
      DRD_(bm_access_range_load)(bm1, i, j);
      DRD_(bm_clear_load)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_1)(bm1, i);
      DRD_(bm_clear_load)(bm1, i, i + MAX(1, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_2)(bm1, i);
      DRD_(bm_clear_load)(bm1, i, i + MAX(2, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_4)(bm1, i);
      DRD_(bm_clear_load)(bm1, i, i + MAX(4, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_8)(bm1, i);
      DRD_(bm_clear_load)(bm1, i, i + MAX(8, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));

      DRD_(bm_access_range_store)(bm1, i, j);
      DRD_(bm_clear_store)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_store_1)(bm1, i);
      DRD_(bm_clear_store)(bm1, i, i + MAX(1, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_store_2)(bm1, i);
      DRD_(bm_clear_store)(bm1, i, i + MAX(2, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_store_4)(bm1, i);
      DRD_(bm_clear_store)(bm1, i, i + MAX(4, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_store_8)(bm1, i);
      DRD_(bm_clear_store)(bm1, i, i + MAX(8, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));

      DRD_(bm_access_range_load)(bm1, i, j);
      DRD_(bm_access_range_store)(bm1, i, j);
      DRD_(bm_clear)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_1)(bm1, i);
      DRD_(bm_access_store_1)(bm1, i);
      DRD_(bm_clear)(bm1, i, i + MAX(1, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_2)(bm1, i);
      DRD_(bm_access_store_2)(bm1, i);
      DRD_(bm_clear)(bm1, i, i + MAX(2, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_4)(bm1, i);
      DRD_(bm_access_store_4)(bm1, i);
      DRD_(bm_clear)(bm1, i, i + MAX(4, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_access_load_8)(bm1, i);
      DRD_(bm_access_store_8)(bm1, i);
      DRD_(bm_clear)(bm1, i, i + MAX(8, ADDR_GRANULARITY));
      assert(bm_equal_print_diffs(bm1, bm2));
    }
  }
  DRD_(bm_access_range_load)(bm1, 0, make_address(2, 0) + 2 * BITS_PER_UWORD);
  DRD_(bm_access_range_store)(bm1, 0, make_address(2, 0) + 2 * BITS_PER_UWORD);
  DRD_(bm_access_range_load)(bm2, 0, make_address(2, 0) + 2 * BITS_PER_UWORD);
  DRD_(bm_access_range_store)(bm2, 0, make_address(2, 0) + 2 * BITS_PER_UWORD);
  for (i = make_address(1, 0) - 2 * BITS_PER_UWORD;
       i < make_address(1, 0) + 2 * BITS_PER_UWORD;
       i += outer_loop_step)
  {
    for (j = i + 1; j < ub; j += inner_loop_step)
    {
      DRD_(bm_clear_load)(bm1, i, j);
      DRD_(bm_access_range_load)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_load)(bm1, i, i+1);
      DRD_(bm_access_load_1)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_load)(bm1, i, i+2);
      DRD_(bm_access_load_2)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_load)(bm1, i, i+4);
      DRD_(bm_access_load_4)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_load)(bm1, i, i+8);
      DRD_(bm_access_load_8)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));

      DRD_(bm_clear_store)(bm1, i, j);
      DRD_(bm_access_range_store)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_store)(bm1, i, i+1);
      DRD_(bm_access_store_1)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_store)(bm1, i, i+2);
      DRD_(bm_access_store_2)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_store)(bm1, i, i+4);
      DRD_(bm_access_store_4)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));
      DRD_(bm_clear_store)(bm1, i, i+8);
      DRD_(bm_access_store_8)(bm1, i);
      assert(bm_equal_print_diffs(bm1, bm2));

      DRD_(bm_clear)(bm1, i, j);
      DRD_(bm_access_range_load)(bm1, i, j);
      DRD_(bm_access_range_store)(bm1, i, j);
      assert(bm_equal_print_diffs(bm1, bm2));
    }
  }
  DRD_(bm_delete)(bm2);
  DRD_(bm_delete)(bm1);
}

int main(int argc, char** argv)
{
  int outer_loop_step = ADDR_GRANULARITY;
  int inner_loop_step = ADDR_GRANULARITY;
  int optchar;

  while ((optchar = getopt(argc, argv, "s:t:q")) != EOF)
  {
    switch (optchar)
    {
    case 's':
      outer_loop_step = atoi(optarg);
      break;
    case 't':
      inner_loop_step = atoi(optarg);
      break;
    case 'q':
      s_verbose = 0;
      break;
    default:
      fprintf(stderr,
              "Usage: %s [-s<outer_loop_step>] [-t<inner_loop_step>] [-q].\n",
              argv[0]);
      break;
    }
  }

  fprintf(stderr, "Start of DRD BM unit test.\n");

  bm_test1();
  bm_test2();
  bm_test3(outer_loop_step, inner_loop_step);

  fprintf(stderr, "End of DRD BM unit test.\n");

  return 0;
}
