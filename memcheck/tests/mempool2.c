
// Simplified version of mempool.c, that is more oriented towards
// checking that the description of invalid addresses is correct.

#include <stdio.h>
#include <unistd.h>
#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>

#include "../memcheck.h"

#define SUPERBLOCK_SIZE 100000
#define REDZONE_SIZE 8

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

pool *make_pool( int use_mmap )
{
   pool *p;

   if (use_mmap) {
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

void push(pool *p, int use_mmap )
{
   level_list *l;

   if (use_mmap)
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

void pop(pool *p, int use_mmap)
{
   level_list *l = p->levels;
   p->levels = l->next;
   VALGRIND_DESTROY_MEMPOOL(l->where);
   (void) VALGRIND_MAKE_MEM_NOACCESS(l->where, p->where-l->where);
   p->where = l->where;
   if (use_mmap)
      munmap(l, sizeof(level_list));
   else
      free(l);
}

void destroy_pool(pool *p, int use_mmap)
{
   level_list *l = p->levels;

   while(l) {
      pop(p, use_mmap);
   }
   if (use_mmap) {
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
   char *x1, *x2;
   char res = 0;

   // p1 is a malloc-backed pool
   pool *p1 = make_pool(0);

   // p2 is a mmap-backed pool
   pool *p2 = make_pool(1);

   push(p1, 0);
   push(p2, 1);

   x1 = allocate(p1, 10);
   x2 = allocate(p2, 20);

   fprintf(stderr,
           "\n------ out of range reads in malloc-backed pool ------\n\n");
   res += x1[-1];
   res += x1[10];

   fprintf(stderr,
           "\n------ out of range reads in mmap-backed pool ------\n\n");
   res += x2[-1]; // invalid
   res += x2[20]; // invalid

   fprintf(stderr,
           "\n------ Illegal memory pool address  ------\n\n");
   VALGRIND_MEMPOOL_FREE(p1, x1); // Should be p1->mem

   fprintf(stderr,
           "\n------ read free in malloc-backed pool ------\n\n");
   VALGRIND_MEMPOOL_FREE(p1->mem, x1);
   res += x1[5];

   fprintf(stderr,
           "\n------ read free in mmap-backed pool ------\n\n");
   VALGRIND_MEMPOOL_FREE(p2->mem, x2);
   res += x2[11];

   fprintf(stderr,
           "\n------ double free in malloc-backed pool ------\n\n");
   VALGRIND_MEMPOOL_FREE(p1->mem, x1);

   fprintf(stderr,
           "\n------ double free in mmap-backed pool ------\n\n");
   VALGRIND_MEMPOOL_FREE(p2->mem, x2);

   {
      // test that redzone are still protected even if the user forgets
      // to mark the superblock noaccess.
      char superblock[100];

      VALGRIND_CREATE_MEMPOOL(superblock, REDZONE_SIZE, 0);
      // User should mark the superblock no access to benefit
      // from full Valgrind memcheck protection.
      // VALGRIND_MEMPOOL_ALLOC will however still ensure the
      // redzones are protected.
      VALGRIND_MEMPOOL_ALLOC(superblock, superblock+30, 10);

      res += superblock[30]; // valid
      res += superblock[39]; // valid

      fprintf(stderr,
              "\n------ 2 invalid access in 'no no-access superblock' ---\n\n");
      res += superblock[29]; // invalid
      res += superblock[40]; // invalid

      VALGRIND_DESTROY_MEMPOOL(superblock);
   }
   // claim res is used, so gcc can't nuke this all
   __asm__ __volatile__("" : : "r"(res));

   fprintf(stderr,
           "\n------ done ------\n\n");
   pop(p1, 0);
   pop(p2, 1);
   destroy_pool(p1, 0);
   destroy_pool(p2, 1);
}

int main(void)
{
   test();
   return 0;
}
