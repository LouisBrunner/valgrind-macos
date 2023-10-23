#include <stdio.h>

#define HEAD "agfi %[i], -8\n" \
             "jl 1f\n" \
             "larl %[table], 6f\n" \
             "sllg %[i], %[i], 3(0)\n" \
             "clgfi %[i], 4*8\n"

#define TAIL "0: lghi %[i], 200\n" \
             "j 7f\n" \
             "1: lghi %[i], 100\n" \
             "j 7f\n" \
             "2: lghi %[i], 111\n" \
             "j 7f\n" \
             "3: lghi %[i], 122\n" \
             "j 7f\n" \
             "4: lghi %[i], 133\n" \
             "j 7f\n" \
             "5: lghi %[i], 144\n" \
             "j 7f\n" \
             "6:\n" \
             ".quad 2b\n" \
             ".quad 3b\n" \
             ".quad 4b\n" \
             ".quad 5b\n" \
             ".quad 5b\n" \
             "7:\n"

static long bic0(long i)
{
   void *table;
   asm volatile(HEAD
                "brcl 10, 0f\n"
                ".insn rxy, 0xe30000000047, 0, 8(%[i],%[table])\n"
                "lg %[table],0(%[i],%[table])\n"
                "br %[table]\n"
                TAIL
                : [i] "+a" (i)
                , [table] "=a" (table)
                :: "cc");
   return i;
}

static long bic4(long i)
{
   void *table;
   asm volatile(HEAD
                ".insn rxy, 0xe30000000047, 4, 0(%[i],%[table])\n"
                TAIL
                : [i] "+a" (i)
                , [table] "=a" (table)
                :: "cc");
   return i;
}

static long bic15(long i)
{
   void *table;
   asm volatile(HEAD
                "brcl 10, 0f\n"
                ".insn rxy, 0xe30000000047, 15, 0(%[i],%[table])\n"
                TAIL
                : [i] "+a" (i)
                , [table] "=a" (table)
                :: "cc");
   return i;
}

int main()
{
   long i;

   for (i = 0; i < 16; i++) {
      printf("bic0:  %ld -> %ld\n", i, bic0(i));
      printf("bic4:  %ld -> %ld\n", i, bic4(i));
      printf("bic15: %ld -> %ld\n", i, bic15(i));
   }
}
