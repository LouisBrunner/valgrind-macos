#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "helgrind/helgrind.h"

#define MAX 1000000
static unsigned char shadow[MAX];


#define V(cond, testline)                                               \
   do { if (!(cond))                                                    \
         fprintf (stderr, "Test at line %d Failed verif at line %d: " #cond "\n", \
                  testline, __LINE__); }                                \
   while (0)

#define CHK(a1,a2,a3,a4) check(__LINE__,a1,a2,a3,a4)
/* Check that [p, p+len[ has access access.
   If heap, check that one byte before and after is unaccessible */
static void check (int testline, void *p, int len, unsigned char access, int heap)
{
   int i;
   long int r;

   assert (len < 1000000); // Do not exceed the shadow array

   if (len == 0 && p == NULL)
      return;
   // malloc(0) can return a ptr or NULL.
   // Let's not check NULL

   r = VALGRIND_HG_GET_ABITS (p, shadow, len);
   V (r == VALGRIND_HG_GET_ABITS (p, NULL, len), testline);
   V (access == 0xff ? r == len : r == 0, testline);
   for (i = 0; i < len; i++)
      V(shadow[i] == access, testline);
   if (heap) {
      /* Check the range starting 1 byte before. */
      r = VALGRIND_HG_GET_ABITS (p-1, shadow, len+1);
      V (r == VALGRIND_HG_GET_ABITS (p-1, NULL, len+1), testline);
      V (access == 0xff ? r == len : r == 0, testline);
      V (shadow[0] == 0x00, testline);
      for (i = 1; i < len+1; i++)
         V (shadow[i] == access, testline);
      /* Same but one byte after. We need special cases for
         a len 0,*/
      r = VALGRIND_HG_GET_ABITS (p+1, shadow, len);
      V (r == VALGRIND_HG_GET_ABITS (p+1, NULL, len), testline);
      if (len == 0)
         V (r == 0, testline);
      else
         V (access == 0xff ? r == len-1 : r == 0, testline);
      for (i = 0; i < len-1; i++)
         V(shadow[i] == access, testline);
      if (len != 0)
         V(shadow[len-1] == 0x00, testline);
   }
}

/* return an address on the stack, with big var on the stack,
   to ensure it is really unaddressable when calling check. */
static void* popped_stack_address(void)
{
   char s[MAX];
   memcpy(s, shadow, MAX);
   char *p;

   p = &s[MAX/2-1-s[0]];
   CHK(p, 1, 0xFF, 0);
   return p;
}

int main ( void )
{
   char *p;

   /* Basic test for an heap object */
   fprintf(stderr, "basic heap test\n");
   p = malloc (100);
   CHK (p, 100, 0xff, 1);
   free (p);
   CHK (p, 100, 0x00, 1);

   /* Basic test for some code : verify 50 bytes of check function code
      is accessible. */
   fprintf(stderr, "code test\n");
   CHK (check, 50, 0xff, 0);

   /* Check something on the stack */
   fprintf(stderr, "stack test\n");
   CHK (&p, sizeof(p), 0xff, 0);


   /* Now shake the heap, to verify various sizes */
   fprintf(stderr, "doing many heap blocks\n");
   int i;
   int j;
#  define X 200
#  define Y 4
   void *ptr[X][Y];
   int sz[X][Y];
   int f[X][Y]; // already freed or not ?
   for (i = 0; i < X; i++) {
      for (j = 0; j < Y; j++) {
         f[i][j] = 1;
         // A SecMap represents 8Kb. We test the boundaries
         // around such secmap (X/2 bytes before and after)
         // We test with blocks covering from 0 till Y-1 secmaps
         sz[i][j] = j * 8192 - (j == 0 ? 0 : X/2) + i;
         ptr[i][j] = malloc(sz[i][j]);
         CHK(ptr[i][j],sz[i][j], 0xff, 1);
      }
   }
   /* Shake and check when doing random free */
   fprintf(stderr, "random heap free and checks\n");
   for (i = 0; i < X*Y/10; i++) {
      int x = rand() % X;
      int y = rand() % Y;
      if (f[x][y]) {
         CHK(ptr[x][y],sz[x][y], 0xff, 1);
         free(ptr[x][y]);
         f[x][y] = 0;
      }
      CHK(ptr[x][y],sz[x][y], 0x00, 1);
   }

#if 0
   /* Check that a use after return gives unaddressable. */
   CHK (popped_stack_address(), 1, 0x00, 0);
   /* Well well, it seems helgrind keeps the stack accessible */
#endif
   (void) popped_stack_address();

   return 0;
}

