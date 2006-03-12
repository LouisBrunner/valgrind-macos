
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "valgrind.h"

/* Program that checks all numbers of args (0 through 12) work for
   wrapping.  Also calls originals which trash all the iregs in an
   attempt to shake out any problems caused by insufficient saving of
   caller-save registers around the hidden call instruction. */

typedef unsigned int  UInt;

#define ROL(_x,n) (((_x) << n) | ((UInt)(_x)) >> ((8*sizeof(UInt)-n)))

#define TRASH_IREGS(_rlval, _vec) \
   do { \
      register UInt* vec = (_vec);   \
      /* x86 spills for v > 4, amd64 for v > 12.  Getting ppc */ \
      /* to spill is quite difficult, requiring v > 28 or so. */ \
      register UInt i, sum = 0;   \
      register UInt v1 = vec[1-1];   \
      register UInt v2 = vec[2-1];   \
      register UInt v3 = vec[3-1];   \
      register UInt v4 = vec[4-1];   \
      register UInt v5 = vec[5-1];   \
      register UInt v6 = vec[6-1];   \
      register UInt v7 = vec[7-1];   \
      register UInt v8 = vec[8-1];   \
      register UInt v9 = vec[9-1];   \
      register UInt v10 = vec[10-1];   \
      register UInt v11 = vec[11-1];   \
      register UInt v12 = vec[12-1];   \
      register UInt v13 = vec[13-1];   \
      register UInt v14 = vec[14-1];   \
      register UInt v15 = vec[15-1];   \
      register UInt v16 = vec[16-1];   \
      register UInt v17 = vec[17-1];   \
      register UInt v18 = vec[18-1];   \
      register UInt v19 = vec[19-1];   \
      register UInt v20 = vec[20-1];   \
      register UInt v21 = vec[21-1];   \
      register UInt v22 = vec[22-1];   \
      register UInt v23 = vec[23-1];   \
      register UInt v24 = vec[24-1];   \
      register UInt v25 = vec[25-1];   \
      register UInt v26 = vec[26-1];   \
      register UInt v27 = vec[27-1];   \
      register UInt v28 = vec[28-1];   \
      register UInt v29 = vec[29-1];   \
      for (i = 0; i < 50; i++) {   \
         v1  = ROL(v1,1);   \
         v2  = ROL(v2,2);   \
         v3  = ROL(v3,3);   \
         v4  = ROL(v4,4);   \
         v5  = ROL(v5,5);   \
         v6  = ROL(v6,6);   \
         v7  = ROL(v7,7);   \
         v8  = ROL(v8,8);   \
         v9  = ROL(v9,9);   \
         v10 = ROL(v10,10);   \
         v11 = ROL(v11,11);   \
         v12 = ROL(v12,12);   \
         v13 = ROL(v13,13);   \
         v14 = ROL(v14,14);   \
         v15 = ROL(v15,15);   \
         v16 = ROL(v16,16);   \
         v17 = ROL(v17,17);   \
         v18 = ROL(v18,18);   \
         v19 = ROL(v19,19);   \
         v20 = ROL(v20,20);   \
         v21 = ROL(v21,21);   \
         v22 = ROL(v22,22);   \
         v23 = ROL(v23,23);   \
         v24 = ROL(v24,24);   \
         v25 = ROL(v25,25);   \
         v26 = ROL(v26,26);   \
         v27 = ROL(v27,27);   \
         v28 = ROL(v28,28);   \
         v29 = ROL(v29,29);   \
         sum ^= ((0xFFF & v1) * i);   \
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
         sum ^= (v1-v13);   \
         sum ^= (v1-v14);   \
         sum ^= (v1-v15);   \
         sum ^= (v1-v16);   \
         sum ^= (v1-v17);   \
         sum ^= (v1-v18);   \
         sum ^= (v1-v19);   \
         sum ^= (v1-v20);   \
         sum ^= (v1-v21);   \
         sum ^= (v1-v22);   \
         sum ^= (v1-v23);   \
         sum ^= (v1-v24);   \
         sum ^= (v1-v25);   \
         sum ^= (v1-v26);   \
         sum ^= (v1-v27);   \
         sum ^= (v1-v28);   \
         sum ^= (v1-v29);   \
      }   \
      _rlval = sum;   \
   } while (0)


/* Returns one, in a way that gcc probably can't constant fold out */

volatile int one_actual_return_value = 0; /* the value one() returns */

 __attribute__((noinline))
int one ( void )
{
   int i, sum, a[7];
   for (i = 0; i < 7; i++)
      a[i] = i;
   a[3] = 3+one_actual_return_value;
   sum = 0;
   for (i = 7-1; i >= 0; i--)
      sum += a[i] - i;
   return sum;
}

#define LOOPS_START                                                \
   { register int len = one();                                     \
     register int x0; for (x0 = 0x1000; x0 < 0x1000+len; x0++) {   \
     register int x1; for (x1 = 0x1100; x1 < 0x1100+len; x1++) {   \
     register int x2; for (x2 = 0x1200; x2 < 0x1200+len; x2++) {   \
     register int x3; for (x3 = 0x1300; x3 < 0x1300+len; x3++) {   \
     register int x4; for (x4 = 0x1400; x4 < 0x1400+len; x4++) {   \
     register int x5; for (x5 = 0x1500; x5 < 0x1500+len; x5++) {   \
     register int x6; for (x6 = 0x1600; x6 < 0x1600+len; x6++) {   \
     register int x7; for (x7 = 0x1700; x7 < 0x1700+len; x7++) {   \
     register int x8; for (x8 = 0x1800; x8 < 0x1800+len; x8++) {   \
     register int x9; for (x9 = 0x1900; x9 < 0x1900+len; x9++) {   \
     register int xA; for (xA = 0x1A00; xA < 0x1A00+len; xA++) {   \
     register int xB; for (xB = 0x1B00; xB < 0x1B00+len; xB++) {   \
     register int xC; for (xC = 0x1C00; xC < 0x1C00+len; xC++) {   \
     register int xD; for (xD = 0x1D00; xD < 0x1D00+len; xD++) {   \
     register int xE; for (xE = 0x1E00; xE < 0x1E00+len; xE++) {   \
     register int xF; for (xF = 0x1F00; xF < 0x1F00+len; xF++) {   \
     /* */

#define LOOPS_END \
     assert(xF >= 0x1F00 && xF <= 0x1F00+len); }                   \
     assert(xE >= 0x1E00 && xE <= 0x1E00+len); }                   \
     assert(xD >= 0x1D00 && xD <= 0x1D00+len); }                   \
     assert(xC >= 0x1C00 && xC <= 0x1C00+len); }                   \
     assert(xB >= 0x1B00 && xB <= 0x1B00+len); }                   \
     assert(xA >= 0x1A00 && xA <= 0x1A00+len); }                   \
     assert(x9 >= 0x1900 && x9 <= 0x1900+len); }                   \
     assert(x8 >= 0x1800 && x8 <= 0x1800+len); }                   \
     assert(x7 >= 0x1700 && x7 <= 0x1700+len); }                   \
     assert(x6 >= 0x1600 && x6 <= 0x1600+len); }                   \
     assert(x5 >= 0x1500 && x5 <= 0x1500+len); }                   \
     assert(x4 >= 0x1400 && x4 <= 0x1400+len); }                   \
     assert(x3 >= 0x1300 && x3 <= 0x1300+len); }                   \
     assert(x2 >= 0x1200 && x2 <= 0x1200+len); }                   \
     assert(x1 >= 0x1100 && x1 <= 0x1100+len); }                   \
     assert(x0 >= 0x1000 && x0 <= 0x1000+len); }                   \
   }

/* General idea is for the wrappers to use LOOPS_START / LOOPS_END to
   soak up lots of int registers.  And the orig fn uses TRASH_IREGS to
   do the same.  If there is insufficient saving of caller-saves regs
   by the CALL_FN_* macros, then hopefully the assertions in LOOPS_END
   will fail. */

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
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_0  wrapper pre ()\n");
    CALL_FN_W_v(r, fn);
    printf("fn_0  wrapper post1 = %d\n", (int)r);
    CALL_FN_v_v(fn);
    printf("fn_0  wrapper post2 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 1 --------------- */  

UInt fn_1 ( UInt a1 )
{
   UInt  r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_1) ( UInt a1 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_1  wrapper pre ( %d )\n", (int)a1);
    CALL_FN_W_W(r, fn, a1);
    printf("fn_1  wrapper post1 = %d\n", (int)r);
    CALL_FN_v_W(fn, a1);
    printf("fn_1  wrapper post2 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 2 --------------- */  

UInt fn_2 ( UInt a1, UInt a2 )
{
   UInt r = 0;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_2) ( UInt a1, UInt a2 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_2  wrapper pre ( %d, %d )\n", (int)a1, (int)a2);
    CALL_FN_W_WW(r, fn, a1, a2);
    printf("fn_2  wrapper post1 = %d\n", (int)r);
    CALL_FN_v_WW(fn, a1, a2);
    printf("fn_2  wrapper post2 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 3 --------------- */  

UInt fn_3 ( UInt a1, UInt a2, UInt a3 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_3) ( UInt a1, UInt a2, UInt a3 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_3  wrapper pre ( %d, %d, %d )\n", (int)a1, (int)a2, (int)a3);
    CALL_FN_W_WWW(r, fn, a1, a2, a3);
    printf("fn_3  wrapper post1 = %d\n", (int)r);
    CALL_FN_v_WWW(fn, a1, a2, a3);
    printf("fn_3  wrapper post2 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 4 --------------- */  

UInt fn_4 ( UInt a1, UInt a2, UInt a3, UInt a4 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_4) 
   ( UInt a1, UInt a2, UInt a3, UInt a4 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_4  wrapper pre ( %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4);
    CALL_FN_W_WWWW(r, fn, a1, a2, a3, a4);
    printf("fn_4  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 5 --------------- */  

UInt fn_5 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_5) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_5  wrapper pre ( %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5);
    CALL_FN_W_5W(r, fn, a1, a2, a3, a4, a5);
    printf("fn_5  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 6 --------------- */  

UInt fn_6 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_6) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_6  wrapper pre ( %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6);
    CALL_FN_W_6W(r, fn, a1, a2, a3, a4, a5, a6);
    printf("fn_6  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 7 --------------- */  

UInt fn_7 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
            UInt a7 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_7) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_7  wrapper pre ( %d, %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7);
    CALL_FN_W_7W(r, fn, a1, a2, a3, a4, a5, a6, a7);
    printf("fn_7  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 8 --------------- */  

UInt fn_8 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
            UInt a7, UInt a8 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   words[8-1] = a8;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_8) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7, UInt a8 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_8  wrapper pre ( %d, %d, %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7, (int)a8);
    CALL_FN_W_8W(r, fn, a1, a2, a3, a4, a5, a6, a7, a8);
    printf("fn_8  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 9 --------------- */  

UInt fn_9 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
            UInt a7, UInt a8, UInt a9 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   words[8-1] = a8;
   words[9-1] = a9;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_9) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7, UInt a8, UInt a9 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_9  wrapper pre ( %d, %d, %d, %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7, (int)a8, (int)a9);
    CALL_FN_W_9W(r, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    printf("fn_9  wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 10 --------------- */  

UInt fn_10 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
             UInt a7, UInt a8, UInt a9, UInt a10 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   words[8-1] = a8;
   words[9-1] = a9;
   words[10-1] = a10;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_10) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7, UInt a8, UInt a9, UInt a10 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_10 wrapper pre ( %d, %d, %d, %d, %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7, (int)a8, (int)a9, (int)a10);
    CALL_FN_W_10W(r, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
    printf("fn_10 wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 11 --------------- */  

UInt fn_11 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
             UInt a7, UInt a8, UInt a9, UInt a10, UInt a11 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   words[8-1] = a8;
   words[9-1] = a9;
   words[10-1] = a10;
   words[11-1] = a11;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_11) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7, UInt a8, UInt a9, UInt a10, UInt a11 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_11 wrapper pre ( %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7, (int)a8, (int)a9, (int)a10, (int)a11);
    CALL_FN_W_11W(r, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
    printf("fn_11 wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- 12 --------------- */  

UInt fn_12 ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
             UInt a7, UInt a8, UInt a9, UInt a10, UInt a11, UInt a12 )
{
   UInt r;
   UInt* words = calloc(200, sizeof(UInt));
   words[1-1] = a1;
   words[2-1] = a2;
   words[3-1] = a3;
   words[4-1] = a4;
   words[5-1] = a5;
   words[6-1] = a6;
   words[7-1] = a7;
   words[8-1] = a8;
   words[9-1] = a9;
   words[10-1] = a10;
   words[11-1] = a11;
   words[12-1] = a12;
   TRASH_IREGS(r, words);
   free(words);
   return r;
}

UInt I_WRAP_SONAME_FNNAME_ZU(NONE,fn_12) 
   ( UInt a1, UInt a2, UInt a3, UInt a4, UInt a5, UInt a6,
     UInt a7, UInt a8, UInt a9, UInt a10, UInt a11, UInt a12 )
{
   UInt   r = 0;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   LOOPS_START
    printf("fn_12 wrapper pre ( %d, %d, %d, %d, %d, %d, "
                               "%d, %d, %d, %d, %d, %d )\n", 
           (int)a1, (int)a2, (int)a3, (int)a4, (int)a5, (int)a6,
           (int)a7, (int)a8, (int)a9, (int)a10, (int)a11, (int)a12);
    CALL_FN_W_12W(r, fn, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
    printf("fn_12 wrapper post1 = %d\n", (int)r);
   LOOPS_END
   return r;
}

/* --------------- main --------------- */  

int main ( void )
{
   UInt w;

   one_actual_return_value = 1;

   printf("fn_0  ...\n");
   w = fn_0();
   printf("      ...  %d\n\n", (int)w);

   printf("fn_1  ...\n");
   w = fn_1(42);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_2  ...\n");
   w = fn_2(42,43);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_3  ...\n");
   w = fn_3(42,43,44);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_4  ...\n");
   w = fn_4(42,43,44,45);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_5  ...\n");
   w = fn_5(42,43,44,45,46);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_6  ...\n");
   w = fn_6(42,43,44,45,46,47);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_7  ...\n");
   w = fn_7(42,43,44,45,46,47,48);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_8  ...\n");
   w = fn_8(42,43,44,45,46,47,48,49);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_9  ...\n");
   w = fn_9(42,43,44,45,46,47,48,49,50);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_10 ...\n");
   w = fn_10(42,43,44,45,46,47,48,49,50,51);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_11 ...\n");
   w = fn_11(42,43,44,45,46,47,48,49,50,51,52);
   printf("      ...  %d\n\n", (int)w);

   printf("fn_12 ...\n");
   w = fn_12(42,43,44,45,46,47,48,49,50,51,52,53);
   printf("      ...  %d\n\n", (int)w);

   return 0;
}
