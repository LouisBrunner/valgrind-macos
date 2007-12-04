
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "../memcheck.h"

struct cell
{  
  struct cell *next;
  int x;
};

struct pool
{
  size_t allocated;
  size_t used;
  uint8_t *buf;
};

void * 
allocate_from_pool(struct pool *p, size_t n)
{
  void *a = p->buf + p->used;
  assert(p->used + n < p->allocated);
  VALGRIND_MEMPOOL_ALLOC(p, a, n);
  p->used += n;
  return a;
}

struct pool *
allocate_pool()
{
  struct pool *p = malloc(sizeof(struct pool));
  assert(p);
  p->allocated = 4096;
  p->used = 0;
  p->buf = malloc(p->allocated);
  assert(p->buf);
  memset(p->buf, 0, p->allocated);
  VALGRIND_CREATE_MEMPOOL(p, 0, 0);
  VALGRIND_MAKE_MEM_NOACCESS(p->buf, p->allocated);
  return p;
}

#define N 100

/* flags */
int static_roots = 0;
int trim_pool    = 0;
int destroy_pool = 0;
void set_flags ( int n )
{
  switch (n) {
     case 0:
        static_roots = 0;
        trim_pool    = 0;
        destroy_pool = 0;
        break;
     case 1:
        static_roots = 0;
        trim_pool    = 1;
        destroy_pool = 0;
        break;
     case 2:
        static_roots = 0;
        trim_pool    = 0;
        destroy_pool = 1;
        break;
     case 3:
        static_roots = 1;
        trim_pool    = 0;
        destroy_pool = 0;
        break;
     case 4:
        static_roots = 1;
        trim_pool    = 1;
        destroy_pool = 0;
        break;
     case 5:
        static_roots = 1;
        trim_pool    = 0;
        destroy_pool = 1;
        break;
     default:
        assert(0);
  }
}


struct cell *cells_static[N];


int main( int argc, char** argv )
{
  struct cell *cells_local[N];
  int arg;
  size_t i;
  struct pool *p      = allocate_pool();
  struct cell **cells = static_roots ? cells_static : cells_local;

  assert(argc == 2);
  assert(argv[1]);
  assert(strlen(argv[1]) == 1);
  assert(argv[1][0] >= '0' && argv[1][0] <= '5');
  arg = atoi( argv[1] );
  set_flags( arg );

  memset(cells_static, 0, sizeof(cells_static));
  memset(cells_local,  0, sizeof(cells_local));

  for (i = 0; i < N; ++i) {
    cells[i] = allocate_from_pool(p, sizeof(struct cell));  
  }

  if (trim_pool)
  VALGRIND_MEMPOOL_TRIM(p, 
			p->buf+(10 * sizeof(struct cell)), 
			20 * sizeof(struct cell) + 2);

  if (destroy_pool)
  VALGRIND_DESTROY_MEMPOOL(p);

  return 0;
}
