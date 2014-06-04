#include <stdio.h>

#define N 256

unsigned long long reg_val_double[N];

void init_reg_val_double()
{
   unsigned long c = 19650218UL;
   int i;
   reg_val_double[0]= c & 0xffffffffUL;
   for (i = 1; i < N; i++) {
      reg_val_double[i] = (1812433253UL * (reg_val_double[i - 1] ^
                          (reg_val_double[i - 1] >> 30)) + i);
   }
}


/* Make a copy of original array to prevent the unexpected changes by Atomic Add
   Instructions */
unsigned long long reg_val_double_copy[N]; 

void copy_reg_val_double()
{
   int i;
   for (i = 0; i < N; i++) {
      reg_val_double_copy[i] = reg_val_double[i];
   }
}

/* TEST1_32/64 macro is used in load atomic increment/decrement/set/clear
   instructions. After executing each instruction we must check both memory
   location and register value.

   1: Move arguments (offset and base address) to registers 
   2: Add offset and base address to make absolute address
   3: Execute instruction
   4: Move result from register ($t3)
   5: Load memory data ('lw' for 32bit instruction and 'ld' for 64bit addresses)
*/
#define TEST1_32(instruction, offset,mem)                    \
{                                                            \
   unsigned long out = 0;                                    \
   unsigned long res_mem = 0;                                \
   __asm__ volatile(                                         \
     "move         $t0, %2"        "\n\t"                    \
     "move         $t1, %3"        "\n\t"                    \
     "daddu        $t0, $t1, $t0"  "\n\t"                    \
     instruction " $t3, ($t0)"     "\n\t"                    \
     "move         %0,  $t3"       "\n\t"                    \
     "lw           %1,  0($t0)"    "\n\t"                    \
     : "=&r" (out), "=&r"(res_mem)                           \
     : "r" (mem) , "r" (offset)                              \
     : "$12", "$13", "cc", "memory"                          \
     );                                                      \
   printf("%s :: offset: 0x%x, out: 0x%lx, result:0x%lx\n",  \
          instruction, offset, out, res_mem);                \
}

#define TEST1_64(instruction, offset,mem)                     \
{                                                             \
   unsigned long out = 0;                                     \
   unsigned long res_mem = 0;                                 \
   __asm__ volatile(                                          \
     "move         $t0, %2"        "\n\t"                     \
     "move         $t1, %3"        "\n\t"                     \
     "daddu        $t0, $t1, $t0"  "\n\t"                     \
     instruction " $t3, ($t0)"     "\n\t"                     \
     "move         %0,  $t3"       "\n\t"                     \
     "ld           %1,  0($t0)"    "\n\t"                     \
     : "=&r" (out), "=&r"(res_mem)                            \
     : "r" (mem) , "r" (offset)                               \
     : "$12", "$13", "cc", "memory"                           \
     );                                                       \
   printf("%s :: offset: 0x%x, out: 0x%lx, result: 0x%lx\n",  \
          instruction, offset, out, res_mem);                 \
}

/* Test 2 macro is used for pop/dpop/baddu instructions. After executing each
   instructions the macro performs following operations:

   1: Move arguments to registers
   2: Execute instruction
   3: Move result to register ($t3)
*/
#define TEST2(instruction, RSVal, RTVal)                            \
{                                                                   \
   unsigned long out;                                               \
   __asm__ volatile(                                                \
      "move $t1, %1"  "\n\t"                                        \
      "move $t2, %2"  "\n\t"                                        \
      instruction     "\n\t"                                        \
      "move %0, $t3"  "\n\t"                                        \
      : "=&r" (out)                                                 \
      : "r" (RSVal), "r" (RTVal)                                    \
      : "$12", "$13", "cc", "memory"                                \
        );                                                          \
   printf("%s :: rd 0x%lx, rs 0x%llx, rt 0x%llx\n",                 \
          instruction, out, (long long) RSVal, (long long) RTVal);  \
}

/* TEST3 macro is used for store atomic add and store atomic add doubleword 
   instructions. Following operations are performed by the test macro:

   1: Move arguments to the register
   2: Add offset and base address to make absolute address 
   3: Execute instruction
   4: Load memory data
*/
#define TEST3(instruction, offset, mem, value)                   \
{                                                                \
    unsigned long out = 0;                                       \
    unsigned long outPre = 0;                                    \
   __asm__ volatile(                                             \
     "move         $t0, %2"        "\n\t"                        \
     "move         $t1, %3"        "\n\t"                        \
     "daddu        $t0, $t1, $t0"  "\n\t"                        \
     "ld           %1,  0($t0)"    "\n\t"                        \
     "move         $t2, %4"        "\n\t"                        \
     instruction " $t2, ($t0)"     "\n\t"                        \
     "ld           %0,  0($t0)"    "\n\t"                        \
     : "=&r" (out), "=&r" (outPre)                               \
     : "r" (mem) , "r" (offset), "r" (value)                     \
     : "$12", "$13", "$14", "cc", "memory"                       \
     );                                                          \
     printf("%s :: value: 0x%llx, memPre: 0x%lx, mem: 0x%lx\n",  \
            instruction, value, outPre, out);                    \
}

/* TEST4_32/64 is used for load atomic add/swap instructions. Following
   operations are performed by macro after execution of each instruction:

   1: Move arguments to register.
   2: Add offset and base address to make absolute address.
   3: Execute instruction.
   4: Move result to register.
   5: Load memory data ('lw' for 32bit instruction and 'ld' for 64bit).
*/
#define TEST4_32(instruction, offset, mem)                   \
{                                                            \
    unsigned long out = 0;                                   \
    unsigned long res_mem = 0;                               \
   __asm__ volatile(                                         \
      "move         $t0, %2"          "\n\t"                 \
      "move         $t1, %3"          "\n\t"                 \
      "daddu        $t0, $t0, $t1"    "\n\t"                 \
      instruction " $t3, ($t0), $t1"  "\n\t"                 \
      "move         %0,  $t3"         "\n\t"                 \
      "lw           %1,  0($t0)"      "\n\t"                 \
      : "=&r" (out), "=&r"(res_mem)                          \
      : "r" (mem) , "r" (offset)                             \
      : "$12", "$13", "cc", "memory"                         \
     );                                                      \
   printf("%s :: offset: 0x%x, out: 0x%lx, result:0x%lx\n",  \
          instruction, offset, out, res_mem);                \
}

#define TEST4_64(instruction, offset, mem)                    \
{                                                             \
    unsigned long out = 0;                                    \
    unsigned long res_mem = 0;                                \
   __asm__ volatile(                                          \
      "move         $t0, %2"          "\n\t"                  \
      "move         $t1, %3"          "\n\t"                  \
      "daddu        $t0, $t0,   $t1"  "\n\t"                  \
      instruction " $t3, ($t0), $t1"  "\n\t"                  \
      "move         %0,  $t3"         "\n\t"                  \
      "ld           %1,  0($t0)"      "\n\t"                  \
     : "=&r" (out), "=&r"(res_mem)                            \
     : "r" (mem) , "r" (offset)                               \
     : "$12", "$13", "cc", "memory"                           \
     );                                                       \
   printf("%s :: offset: 0x%x, out: 0x%lx, result: 0x%lx\n",  \
          instruction, offset, out, res_mem);                 \
}

typedef enum {
   BADDU, POP, DPOP, SAA, SAAD, LAA, LAAD, LAW, LAWD, LAI, LAID, LAD, LADD,
   LAS, LASD, LAC, LACD
} cvm_op;

int main()
{
#if (_MIPS_ARCH_OCTEON2)
   init_reg_val_double();
   int i,j;
   cvm_op op;
   for (op = BADDU; op <= LACD; op++) {
      switch(op){
         /* Unsigned Byte Add - BADDU rd, rs, rt; Cavium OCTEON */
         case BADDU: {
            for(i = 4; i < N; i += 4)
               for(j = 4; j < N; j += 4)
                  TEST2("baddu $t3, $t1, $t2", reg_val_double[i],
                                               reg_val_double[j]);
            break;
         }
         case POP: {  /* Count Ones in a Word - POP */
            for(j = 4; j < N; j += 4)
               TEST2("pop $t3, $t1", reg_val_double[j], 0);
            break;
         }
         case DPOP: {  /* Count Ones in a Doubleword - DPOP */
            for(j = 8; j < N; j += 8)
               TEST2("dpop $t3, $t1", reg_val_double[j], 0);
            break;
         }
         case SAA: {  /* Atomic Add Word - saa rt, (base). */
            copy_reg_val_double();
            for(j = 4; j < N; j += 4)
               TEST3("saa", j, reg_val_double_copy, reg_val_double[j]);
            break;
         }
         case SAAD: {  /* Atomic Add Double - saad rt, (base). */
            copy_reg_val_double();
            for(j = 8; j < N; j += 8)
               TEST3("saad", j, reg_val_double_copy, reg_val_double[j]);
            break;
         }
         case LAA: {  /* Load Atomic Add Word - laa rd, (base), rt. */
            copy_reg_val_double();
            for(j = 4; j < N; j += 4)
               TEST4_32("laa", j, reg_val_double_copy);
            break;
         }
         case LAAD: {  /* Load Atomic Add Double - laad rd, (base), rt */
            copy_reg_val_double();
            for(j = 8; j < N; j += 8)
               TEST4_64("laad ", j, reg_val_double_copy);
            break;
         }
         case LAW: {  /* Load Atomic Swap Word - law rd, (base), rt */
            copy_reg_val_double();
            for(j = 4; j < N; j += 4)
               TEST4_32("law", j, reg_val_double_copy);
            break;
         }
         case LAWD: {  /* Load Atomic Swap Double - lawd rd, (base), rt */
            copy_reg_val_double();
            for(j = 8; j < N; j += 8)
               TEST4_64("lawd", j, reg_val_double_copy);
            break;
         }
         case LAI: {  /* Load Atomic Increment Word - lai rd, (base) */
            copy_reg_val_double();
            for(i = 4; i < N; i += 4)
               TEST1_32("lai", i, reg_val_double_copy);
            break;
         }
         case LAID: {  /* Load Atomic Increment Double - laid rd, (base) */
            copy_reg_val_double();
            for(i = 8; i < N; i += 8)
              TEST1_64("laid ", i, reg_val_double_copy);
            break;
         }
         case LAD: {  /* Load Atomic Decrement Word - lad rd, (base) */
            copy_reg_val_double();
            for(i = 4; i < N; i += 4)
               TEST1_32("lad", i, reg_val_double_copy);
            break;
         }
         case LADD: {  /* Load Atomic Decrement Double - ladd rd, (base) */
            copy_reg_val_double();
            for(i = 8; i < N; i += 8)
               TEST1_64("ladd",i, reg_val_double_copy);
            break;
         }
         case LAS:{   /* Load Atomic Set Word - las rd, (base) */
            copy_reg_val_double();
            for(i = 4; i < N; i += 4)
               TEST1_32("las",i, reg_val_double_copy);
            break;
         }
         case LASD:{  /* Load Atomic Set Word - lasd rd, (base) */
            copy_reg_val_double();
            for(i = 8; i < N; i += 8)
               TEST1_64("lasd",i, reg_val_double_copy);
            break;
         }
         case LAC: {  /* Load Atomic Clear Word - lac rd, (base) */
            copy_reg_val_double();
            for(i = 4; i < N; i += 4)
               TEST1_32("lac",i, reg_val_double_copy);
            break;
         }
         case LACD: {  /* Load Atomic Clear Double - lacd rd, (base) */
            copy_reg_val_double();
            for(i = 8; i < N; i += 8)
               TEST1_64("lacd",i, reg_val_double_copy);
            break;
         }
         default:
            printf("Nothing to be executed \n");
      }
   }
#endif
   return 0;
}
