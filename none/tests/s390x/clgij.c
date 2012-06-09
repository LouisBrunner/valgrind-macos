#include <stdio.h>
#include <stdint.h>
#include "opcodes.h"

#define BRASLCLOBBER "0","1","2","3","4","5","14", \
		     "f0","f1","f2","f3","f4","f5","f6","f7"

void if_eq(void)        { printf("equal\n");   }
void if_ne(void)        { printf("not equal\n");   }
void if_gt(void)        { printf("greater than\n");   }
void if_le(void)        { printf("less or equal\n");   }
void if_lt(void)        { printf("less than\n");   }
void if_ge(void)        { printf("greater or equal\n");   }
void if_taken(void)     { printf("taken\n");   }
void if_not_taken(void) { printf("not taken\n");   }

#undef LT
#define NEVER 0
#define GT 2
#define LT 4
#define NE 6
#define EQ 8
#define LE C
#define GE A
#define ALWAYS E


void compare_never(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,NEVER,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_not_taken\n\t"
                "j     0f\n\t"
                "brasl 14,if_taken\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_always(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,ALWAYS,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_not_taken\n\t"
                "j     0f\n\t"
                "brasl 14,if_taken\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_le42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,LE,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_gt\n\t"
                "j     0f\n\t"
                "brasl 14,if_le\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_ge42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,GE,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_lt\n\t"
                "j     0f\n\t"
                "brasl 14,if_ge\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_gt42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,GT,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_le\n\t"
                "j     0f\n\t"
                "brasl 14,if_gt\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_lt42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,LT,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_ge\n\t"
                "j     0f\n\t"
                "brasl 14,if_lt\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_eq42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,EQ,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_ne\n\t"
                "j     0f\n\t"
                "brasl 14,if_eq\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

void compare_ne42(uint64_t value)
{
   register uint64_t val asm("r7") = value;

   asm volatile(
                "aghi  15,-160\n\t"
                CLGIJ(7,NE,8,2a) "\n\t"    /* 0x2a == 42 */
                "brasl 14,if_eq\n\t"
                "j     0f\n\t"
                "brasl 14,if_ne\n\t"
                "0: aghi 15,160\n\t" : : "d"(val) : "15", BRASLCLOBBER);
   return;
}

int main()
{
   compare_eq42(12);
   compare_eq42(42);
   compare_eq42(100);

   compare_ne42(12);
   compare_ne42(42);
   compare_ne42(100);

   compare_gt42(12);
   compare_gt42(42);
   compare_gt42(100);

   compare_lt42(12);
   compare_lt42(42);
   compare_lt42(100);

   compare_le42(12);
   compare_le42(42);
   compare_le42(100);

   compare_ge42(12);
   compare_ge42(42);
   compare_ge42(100);

   compare_never(12);
   compare_never(42);
   compare_never(100);

   compare_always(12);
   compare_always(42);
   compare_always(100);

   return 0;
}
