#include <unistd.h>
#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>

#include "../memcheck.h"

#define SUPERBLOCK_SIZE 100000
#define REDZONE_SIZE 8

static const int USE_MMAP = 0;

typedef struct _level_list
{
   struct _level_list *next;
   char *where;
   // Padding ensures the struct is the same size on 32-bit and 64-bit
   // machines.
   char padding[16 - 2*sizeof(char*)];
} level_list;

typedef struct _pool {
   char *mem;
   char *where; 
   level_list *levels;
   int size, left;
   // Padding ensures the struct is the same size on 32-bit and 64-bit
   // machines.
   char padding[24 - 3*sizeof(char*)];
} pool;

pool *make_pool()
{
   pool *p;

   if(USE_MMAP) {
      p = (pool *)mmap(0, sizeof(pool), PROT_READ|PROT_WRITE|PROT_EXEC,
                       MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
      p->where = p->mem = (char *)mmap(NULL, SUPERBLOCK_SIZE,
                                       PROT_READ|PROT_WRITE|PROT_EXEC,
                                       MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
   } else {
      p = (pool *)malloc(sizeof(pool));
      p->where = p->mem = (char *)malloc(SUPERBLOCK_SIZE);
   }

   p->size = p->left = SUPERBLOCK_SIZE;
   p->levels = NULL;
   (void) VALGRIND_MAKE_MEM_NOACCESS(p->where, SUPERBLOCK_SIZE);
   return p;
}

void push(pool *p)
{
   level_list *l;

   if(USE_MMAP)
      l = (level_list *)mmap(0, sizeof(level_list),
                             PROT_READ|PROT_WRITE|PROT_EXEC,
                             MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
   else
      l = (level_list *)malloc(sizeof(level_list));

   l->next = p->levels;
   l->where = p->where;
   VALGRIND_CREATE_MEMPOOL(l->where, REDZONE_SIZE, 0);
   p->levels = l;
}

void pop(pool *p)
{
   level_list *l = p->levels;
   p->levels = l->next;
   VALGRIND_DESTROY_MEMPOOL(l->where);
   (void) VALGRIND_MAKE_MEM_NOACCESS(l->where, p->where-l->where);
   p->where = l->where;
   if(USE_MMAP)
      munmap(l, sizeof(level_list));
   else
      free(l);
}

void destroy_pool(pool *p)
{
   level_list *l = p->levels;

   while(l) {
      pop(p);
   }
   if(USE_MMAP) {
      munmap(p->mem, SUPERBLOCK_SIZE);
      munmap(p, sizeof(pool));
   } else {
      free(p->mem);
      free(p);
   }
}

char *allocate(pool *p, int size)
{
   char *where;
   p->left -= size + (REDZONE_SIZE*2);
   where = p->where + REDZONE_SIZE;
   p->where += size + (REDZONE_SIZE*2);
   VALGRIND_MEMPOOL_ALLOC(p->levels->where, where, size);
   return where;
}

//-------------------------------------------------------------------------
// Rest
//-------------------------------------------------------------------------

void test(void)
{
   char *x1, *x2, *x3, *x4, *x5;

   pool *p = make_pool();

   push(p);

   x1 = allocate(p, 10);
   x2 = allocate(p, 20);
   push(p);
   x3 = allocate(p, 10);
   x4 = allocate(p, 20);

   *x1 = 'a';  // valid
   *x2 = 'b';  // valid

   x1[-1] = 'h'; // invalid
   x1[10] = 'i'; // invalid

   pop(p);

   *x3 = 'c';  // invalid
   *x4 = 'd';  // invalid

   *x1 = 'e';  // valid
   *x2 = 'f';  // valid

   x5 = allocate(p, 10);

   *x5 = 'g';  // valid

   // pop(p);

   // *x5 = 'g';  // invalid

   // destroy_pool(p);
}

int main(void)
{
   test();
   return 0;
}
