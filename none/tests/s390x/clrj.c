#include <stdio.h>
#include <stdint.h>
#include "opcodes.h"

#define BRASLCLOBBER "cc", "0","1","2","3","4","5","14",        \
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


void compare_never(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,NEVER) "\n\t"
                "brasl 14,if_not_taken\n\t"
                "j     0f\n\t"
                "brasl 14,if_taken\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_always(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,ALWAYS) "\n\t"
                "brasl 14,if_not_taken\n\t"
                "j     0f\n\t"
                "brasl 14,if_taken\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_le(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,LE) "\n\t"
                "brasl 14,if_gt\n\t"
                "j     0f\n\t"
                "brasl 14,if_le\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_ge(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,GE) "\n\t"
                "brasl 14,if_lt\n\t"
                "j     0f\n\t"
                "brasl 14,if_ge\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_gt(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,GT) "\n\t"
                "brasl 14,if_le\n\t"
                "j     0f\n\t"
                "brasl 14,if_gt\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_lt(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,LT) "\n\t"
                "brasl 14,if_ge\n\t"
                "j     0f\n\t"
                "brasl 14,if_lt\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_eq(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,EQ) "\n\t"
                "brasl 14,if_ne\n\t"
                "j     0f\n\t"
                "brasl 14,if_eq\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

void compare_ne(uint32_t value1, uint32_t value2)
{
   register uint32_t val1 asm("r7") = value1;
   register uint32_t val2 asm("r8") = value2;

   asm volatile(
                "aghi  15,-160\n\t"
                CLRJ(7,8,8,NE) "\n\t"
                "brasl 14,if_eq\n\t"
                "j     0f\n\t"
                "brasl 14,if_ne\n\t"
                "0: aghi 15,160\n\t"
                : : "d"(val1), "d"(val2) : BRASLCLOBBER);
   return;
}

int main()
{
   compare_eq(12, 42);
   compare_eq(42, 42);
   compare_eq(100, 42);

   compare_ne(12, 42);
   compare_ne(42, 42);
   compare_ne(100, 42);

   compare_gt(12, 42);
   compare_gt(42, 42);
   compare_gt(100, 42);

   compare_lt(12, 42);
   compare_lt(42, 42);
   compare_lt(100, 42);

   compare_le(12, 42);
   compare_le(42, 42);
   compare_le(100, 42);

   compare_ge(12, 42);
   compare_ge(42, 42);
   compare_ge(100, 42);

   compare_never(12, 42);
   compare_never(42, 42);
   compare_never(100, 42);

   compare_always(12, 42);
   compare_always(42, 42);
   compare_always(100, 42);

   return 0;
}
