
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "../memcheck.h"

// Test VALGRIND_CREATE_META_MEMPOOL features, the VALGRIND_MEMPOOL_METAPOOL and
// VALGRIND_MEMPOOL_AUTO_FREE flags.
// Also show that without these, having a custom allocator that:
// - Allocates a MEMPOOL
// - Uses ITSELF to get large blocks to populate the pool (so these are marked
//   as MALLOCLIKE blocks)
// - Then passes out MALLOCLIKE blocks out of these pool blocks
// Was not previously supported by the 'loose model' for mempools in memcheck
// because it spotted these (correctly) as overlapping blocks (test case 3
// below).
// The VALGRIND_MEMPOOL_METAPOOL says not to treat these as overlaps.
//
// Also, when one of these metapool blocks is freed, memcheck will not auto-free
// the MALLOCLIKE blocks allocated from the meta-pool, and report them as leaks.
// When VALGRIND_MEMPOOL_AUTO_FREE is passed, no such leaks are reported.
// This is for custom allocators that destroy a pool without freeing the objects
// allocated from it, because that is the defined behaviour of the allocator.

struct pool
{
  size_t allocated;
  size_t used;
  uint8_t *buf;
};

struct cell
{  
  struct cell *next;
  char x[16 - sizeof(void*)];
};

static struct pool _PlainPool, *PlainPool = &_PlainPool;
static struct pool _MetaPool,  *MetaPool  = &_MetaPool;

#define N 10
#define POOL_BLOCK_SIZE   4096
// For easy testing, the plain mempool uses N allocations, the
// metapool 2 * N (so 10 reported leaks are from the plain pool, 20 must be
// from the metapool.

static int    MetaPoolFlags = 0;
static int    CleanupBeforeExit = 0;

static struct cell *cells_plain[2 * N];
static struct cell *cells_meta[2 * N];

static char   PlainBlock[POOL_BLOCK_SIZE];
static char   MetaBlock[POOL_BLOCK_SIZE];

void create_meta_pool (void)
{
   VALGRIND_CREATE_META_MEMPOOL(MetaPool, 0, 0, MetaPoolFlags);
   VALGRIND_MEMPOOL_ALLOC(MetaPool, MetaBlock, POOL_BLOCK_SIZE);

   MetaPool->buf = (uint8_t *) MetaBlock;
   MetaPool->allocated = POOL_BLOCK_SIZE;
   MetaPool->used = 0;

   /* A pool-block is expected to have metadata, and the core of
      valgrind sees a MALLOCLIKE_BLOCK that starts at the same address
      as a MEMPOOLBLOCK as a MEMPOOLBLOCK, hence never as a leak.
      Introduce such some simulated metadata.
   */

   MetaPool->buf  += sizeof(uint8_t);
   MetaPool->used += sizeof(uint8_t);
}

static void create_plain_pool (void)
{
   VALGRIND_CREATE_MEMPOOL(PlainPool, 0, 0);
   
   PlainPool->buf = (uint8_t *) PlainBlock;
   PlainPool->allocated = POOL_BLOCK_SIZE;
   PlainPool->used = 0;

   /* Same overhead */
   PlainPool->buf  += sizeof(uint8_t);
   PlainPool->used += sizeof(uint8_t);
}

static void *allocate_meta_style (struct pool *p, size_t n)
{
  void *a = p->buf + p->used;
  assert(p->used + n < p->allocated);

  // Simulate a custom allocator that allocates memory either directly for
  // the application or for a custom memory pool: All are marked as MALLOCLIKE.
  VALGRIND_MALLOCLIKE_BLOCK(a, n, 0, 0);
  p->used += n;

  return a;
}

static void *allocate_plain_style (struct pool *p, size_t n)
{
  void *a = p->buf + p->used;
  assert(p->used + n < p->allocated);

  // And this is custom allocator that knows what it is allocating from a pool.
  VALGRIND_MEMPOOL_ALLOC(p, a, n);
  p->used += n;

  return a;
}

/* flags */

static void set_flags ( int n )
{
  switch (n) {
     // Case 0: No special flags. VALGRIND_CREATE_META_MEMPOOL is same as
     // VALGRIND_CREATE_MEMPOOL.
     // When mempools are destroyed, the METAPOOL leaks because auto-free is
     // missing. Must show 2*N (20) leaks.
     // The VALGRIND_MEMPOOL_ALLOC items from the plain pool are automatically
     // destroyed. CleanupBeforeExit means the metapool is freed and destroyed
     // (simulating an app that cleans up before it exits), and when false it
     // simply exits with the pool unaltered.
     case 0:
        MetaPoolFlags     = 0;
        CleanupBeforeExit = 1;
        break;

     // Case 1: VALGRIND_MEMPOOL_METAPOOL, no auto-free.
     // Without explicit free, these MALLOCLIKE_BLOCK blocks are considered
     // leaks. So this case should show same as case 0: 20 leaks.
     case 1:
        MetaPoolFlags     = VALGRIND_MEMPOOL_METAPOOL;
        CleanupBeforeExit = 1;
        break;

     // Same as before, but now the MALLOCLIKE blocks are auto-freed.
     // Must show 0 leaks.
     case 2:
        MetaPoolFlags = VALGRIND_MEMPOOL_AUTO_FREE | VALGRIND_MEMPOOL_METAPOOL;
        CleanupBeforeExit = 1;
        break;

     case 3:
        // Just auto-free, with cleanup. The cleanup removes the overlapping
        // blocks, so this is the same as case 2: No leaks, no problems.
        MetaPoolFlags     = VALGRIND_MEMPOOL_AUTO_FREE;
        CleanupBeforeExit = 1;
        break;

     case 4:
        // No auto-free, no cleanup. Leaves overlapping blocks detected
        // by valgrind, but those are ignored because of the METAPOOL.
        // So, no crash, no problems, but 20 leaks.
        MetaPoolFlags     = VALGRIND_MEMPOOL_METAPOOL;
        CleanupBeforeExit = 0;
        break;

     case 5:
        // Main reason for the VALGRIND_MEMPOOL_METAPOOL flags: When not
        // specified, and the application has a memorypool that has MALLOC_LIKE
        // overlapping allocations, that leaves block(s) that overlap.
        // Causes a fatal error.
        // The METAPOOL allows the overlap. Test must show that without that
        // flag, a fatal error occurs.
        MetaPoolFlags     = 0;
        CleanupBeforeExit = 0;
        break;

     default:
        assert(0);
  }
}

int main( int argc, char** argv )
{
   int arg;
   size_t i;

   assert(argc == 2);
   assert(argv[1]);
   assert(strlen(argv[1]) == 1);
   assert(argv[1][0] >= '0' && argv[1][0] <= '9');
   arg = atoi( argv[1] );
   set_flags( arg );

   create_plain_pool();
   create_meta_pool();

   // N plain allocs
   for (i = 0; i < N; ++i) {
      cells_plain[i] = allocate_plain_style(PlainPool,sizeof(struct cell));  
   }

   // 2*N meta allocs
   for (i = 0; i < 2 * N; ++i) {
      cells_meta[i] = allocate_meta_style(MetaPool,sizeof(struct cell));  
   }

   // Leak the memory from the pools by losing the pointers.
   for (i = 0; i < N; ++i) {
      cells_plain[i] = NULL;
   }

   for (i = 0; i < 2 * N; ++i) {
      cells_meta[i] = NULL;
   }

   // This must free MALLOCLIKE allocations from the pool when
   // VALGRIND_MEMPOOL_AUTO_FREE
   // is set for the pool and report leaks when not.
   if (CleanupBeforeExit) {
      VALGRIND_MEMPOOL_FREE(MetaPool, MetaBlock);
      VALGRIND_DESTROY_MEMPOOL(MetaPool);
   }

   // Cleanup.
   VALGRIND_DESTROY_MEMPOOL(PlainPool);

   return 0;
}
