
#include <stdlib.h>
#include <stdio.h>
#include "valgrind.h"

/* Program that checks all numbers of args (0 through 12) work for
   wrapping.  Also calls originals which trash all the iregs in an
   attempt to shake out any problems caused by insufficient saving of
   caller-save registers around the hidden call instruction. */

typedef unsigned int  UInt;

#define ROL(_x,n) (((_x) << n) | ((UInt)(_x)) >> ((8*sizeof(UInt)-n)))

#define TRASH_IREGS(_rlval, _vec) \
   do { \
      UInt* vec = (_vec);   \
      /* x86 spills for v > 4, amd64 for v > 12. */   \
      UInt i, sum = 0;   \
      UInt v1 = vec[1-1];   \
      UInt v2 = vec[2-1];   \
      UInt v3 = vec[3-1];   \
      UInt v4 = vec[4-1];   \
      UInt v5 = vec[5-1];   \
      UInt v6 = vec[6-1];   \
      UInt v7 = vec[7-1];   \
      UInt v8 = vec[8-1];   \
      UInt v9 = vec[9-1];   \
      UInt v10 = vec[10-1];   \
      UInt v11 = vec[11-1];   \
      UInt v12 = vec[12-1];   \
      for (i = 0; i < 50; i++) {   \
         v1 = ROL(v1,1);   \
         v2 = ROL(v2,2);   \
         v3 = ROL(v3,3);   \
         v4 = ROL(v4,4);   \
         v5 = ROL(v5,5);   \
         v6 = ROL(v6,6);   \
         v7 = ROL(v7,7);   \
         v8 = ROL(v8,8);   \
         v9 = ROL(v9,9);   \
         v10 = ROL(v10,10);   \
         v11 = ROL(v11,11);   \
         v12 = ROL(v12,12);   \
         sum ^= (v1-v2);   \
         sum ^= (v1-v3);   \
         sum ^= (v1-v4);   \
         sum ^= (v1-v5);   \
         sum ^= (v1-v6);   \
         sum ^= (v1-v7);   \
         sum ^= (v1-v8);   \
         sum ^= (v1-v9);   \
         sum ^= (v1-v10);   \
         sum ^= (v1-v11);   \
         sum ^= (v1-v12);   \
      }   \
      _rlval = sum;   \
   } while (0)

/* --------------- 0 --------------- */  

UInt fn_0 ( void )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_0) ( UInt a1 )
{
   UInt r;
   void* fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("fn_0  wrapper pre ()\n");
   CALL_FN_W_v(r, fn);
   printf("fn_0  wrapper post1 = %d\n", (int)r);
   CALL_FN_v_v(fn);
   printf("fn_0  wrapper post2 = %d\n", (int)r);
   return r;
}

/* --------------- 1 --------------- */  

UInt fn_1 ( UInt a1 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_1) ( UInt a1 )
{
   UInt r;
   void* fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("fn_1  wrapper pre ( %d )\n", (int)a1);
   CALL_FN_W_W(r, fn, a1);
   printf("fn_1  wrapper post1 = %d\n", (int)r);
   CALL_FN_v_W(fn, a1);
   printf("fn_1  wrapper post2 = %d\n", (int)r);
   return r;
}

/* --------------- 2 --------------- */  

UInt fn_2 ( UInt a1, UInt a2 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_2) ( UInt a1, UInt a2 )
{
   UInt r;
   void* fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("fn_2  wrapper pre ( %d, %d )\n", (int)a1, (int)a2);
   CALL_FN_W_WW(r, fn, a1, a2);
   printf("fn_2  wrapper post1 = %d\n", (int)r);
   CALL_FN_v_WW(fn, a1, a2);
   printf("fn_2  wrapper post2 = %d\n", (int)r);
   return r;
}

/* --------------- main --------------- */  

int main ( void )
{
   UInt w;

   printf("fn_0  ...\n");
   w = fn_0();
   printf("      ...  %d\n\n", (int)w);

   printf("fn_1  ...\n");
   w = fn_1(42);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_2  ...\n");
   w = fn_2(42,43);
   printf("      ...  %d\n\n", (int)w);

   return 0;
}

