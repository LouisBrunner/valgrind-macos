#include <time.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "../memcheck.h"

// Test VALGRIND_CREATE_MEMPOOL_EXT features, the VALGRIND_MEMPOOL_METAPOOL and
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
#define NOISE_SIZE        256

// For easy testing, the plain mempool uses N allocations, the
// metapool 2 * N (so 10 reported leaks are from the plain pool, 20 must be
// from the metapool).

static int    MetaPoolFlags = 0;
static int    CleanupBeforeExit = 0;
static int    GenerateNoise = 0;
static int    NoiseCounter = 0;

static struct cell *cells_plain[2 * N];
static struct cell *cells_meta[2 * N];

static unsigned char *noise[3 * N];

static char   PlainBlock[POOL_BLOCK_SIZE];
static char   MetaBlock[POOL_BLOCK_SIZE];

void create_meta_pool (void)
{
   VALGRIND_CREATE_MEMPOOL_EXT(MetaPool, 0, 0, MetaPoolFlags);
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

  // And this is custom allocator that knows that it is allocating from a pool.
  VALGRIND_MEMPOOL_ALLOC(p, a, n);
  p->used += n;

  return a;
}

/* flags */

static void set_flags ( int n )
{
  switch (n) {
     // Case 0: No special flags. VALGRIND_CREATE_MEMPOOL_EXT is same as
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
        MetaPoolFlags = VALGRIND_MEMPOOL_METAPOOL | VALGRIND_MEMPOOL_AUTO_FREE;
        CleanupBeforeExit = 1;
        break;

     case 3: // Note: this is incorrect behaviour, and aborts valgrind.
        // (so it is not exercised during regression testing).
        // Just auto-free, not marked with meta pool flag.
        // This is an error, and will cause valgrind to abort when the pool
        // is created.
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

     case 6:
        // Test the VG_(HT_remove_at_Iter)() function, which removes a chunk
        // from a hashlist without the need to reset the iterator. The pool
        // is auto_freed, and the best test for the function (besides the ones
        // already done above) is by allocating lots of other chunks that are
        // NOT part of the pool so the MC_Alloc lists contain other stuff.
	// That will make the iterator find stuff AND skip stuff.
        MetaPoolFlags     = VALGRIND_MEMPOOL_METAPOOL | VALGRIND_MEMPOOL_AUTO_FREE;
        CleanupBeforeExit = 1;
        GenerateNoise     = 1;
        break;

     default:
        assert(0);
  }
}

static void GenerateNoisyBit (void)
{
   // In case the HT_remove_at_Iter messes up the administration, the wrong
   // blocks may be deleted from the list, making access to these noise-blocks
   // invalid. So fill 256-byte blocks with easily tested contents.

   noise[NoiseCounter] = malloc(NOISE_SIZE);
   assert(noise[NoiseCounter] != NULL);
   memset(noise[NoiseCounter],(unsigned char) (NoiseCounter % 256), NOISE_SIZE);
   NoiseCounter++;
}

static void CheckNoiseContents (void)
{
   int i;

   for (i = 0; i < NoiseCounter; i++) {
      unsigned char Check = (unsigned char) ( i % 256);
      int j;

      for (j = 0; j < NOISE_SIZE; j++) {
         assert(noise[i][j] == Check);
      }
   }
}

int main( int argc, char** argv )
{
   int arg;
   size_t i;

   assert(argc == 2 || argc == 3);
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

      if (GenerateNoise)
         GenerateNoisyBit();
   }

   // 2*N meta allocs
   for (i = 0; i < 2 * N; ++i) {
      cells_meta[i] = allocate_meta_style(MetaPool,sizeof(struct cell));  

      if (GenerateNoise)
         GenerateNoisyBit();
   }

   // Leak the memory from the pools by losing the pointers.
   for (i = 0; i < N; ++i) {
      cells_plain[i] = NULL;
   }

   for (i = 0; i < 2 * N; ++i) {
      cells_meta[i] = NULL;
   }

   if (GenerateNoise)
      CheckNoiseContents();

   // This must free MALLOCLIKE allocations from the pool when
   // VALGRIND_MEMPOOL_AUTO_FREE
   // is set for the pool and report leaks when not.

   if (CleanupBeforeExit) {
      VALGRIND_MEMPOOL_FREE(MetaPool, MetaBlock);

      if (GenerateNoise)
         CheckNoiseContents();

       VALGRIND_DESTROY_MEMPOOL(MetaPool);

      if (GenerateNoise)
         CheckNoiseContents();

   }

   // Cleanup.
   VALGRIND_DESTROY_MEMPOOL(PlainPool);

   if (GenerateNoise)
      CheckNoiseContents();

   // Try to trigger an error in the bookkeeping by freeing the noise bits.
   // Valgrind should report no leaks, and zero memory in use. If the 
   // new HT_remove_at_Iter function would corrupt the bookkeeping in any
   // way, this should bring it out!
   if (GenerateNoise) {
      for (i = 0; i < NoiseCounter; i++)
         free(noise[i]);
   }


  // Perf test
   if (argc == 3) {
      struct pool perf_plain_pool;
      void *perf_plain_block;
      struct pool perf_meta_pool;
      void *perf_meta_block;
      size_t pool_block_size;
      int n;
      int nr_elts = atoi( argv[2] );
      time_t dnow;
#define tprintf(...) (dnow = time(NULL),          \
                      printf(__VA_ARGS__),        \
                      printf(" %s", ctime(&dnow)))

      pool_block_size = nr_elts * sizeof(struct cell) + sizeof(uint8_t) + 1;

      // Create perf meta pool
      VALGRIND_CREATE_MEMPOOL_EXT
         (&perf_meta_pool, 0, 0,
          VALGRIND_MEMPOOL_METAPOOL | VALGRIND_MEMPOOL_AUTO_FREE);
      perf_meta_block = malloc(pool_block_size);

      VALGRIND_MEMPOOL_ALLOC(&perf_meta_pool, perf_meta_block, 
                             pool_block_size);

      perf_meta_pool.buf = (uint8_t *) perf_meta_block;
      perf_meta_pool.allocated = pool_block_size;
      perf_meta_pool.used = 0;

                               
      perf_meta_pool.buf  += sizeof(uint8_t);
      perf_meta_pool.used += sizeof(uint8_t);

      // Create perf plain pool
      VALGRIND_CREATE_MEMPOOL(&perf_plain_pool, 0, 0);
      perf_plain_block = malloc(pool_block_size);
   
      perf_plain_pool.buf = (uint8_t *) perf_plain_block;
      perf_plain_pool.allocated = pool_block_size;;
      perf_plain_pool.used = 0;

      perf_plain_pool.buf  += sizeof(uint8_t);
      perf_plain_pool.used += sizeof(uint8_t);
      
      tprintf("allocating %d elts", nr_elts);
      for (n = 0; n < nr_elts; n++) {
         (void) allocate_meta_style (&perf_meta_pool, sizeof(struct cell));
         (void) allocate_plain_style (&perf_plain_pool, sizeof(struct cell));
      }

      tprintf("freeing mempool");
      VALGRIND_MEMPOOL_FREE(&perf_meta_pool, perf_meta_block);
      tprintf("destroying mempool");
      VALGRIND_DESTROY_MEMPOOL(&perf_meta_pool);
      tprintf("done");

   }
   return 0;
}
