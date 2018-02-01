#include <stdio.h>
#include "const.h"


#define TEST1(RSval, RD)                   \
{                                          \
   unsigned long long out = 0;             \
   __asm__ __volatile__(                   \
      ".set  noreorder"          "\n\t"    \
      "move  $"#RD", %1"         "\n\t"    \
      "b end"#RD                 "\n\t"    \
      "nop"                      "\n\t"    \
      "daddi $"#RD", $"#RD", 5"  "\n\t"    \
      "end"#RD":"                "\n\t"    \
      "daddi $"#RD", $"#RD", 1"  "\n\t"    \
      "move  %0,     $"#RD       "\n\t"    \
      ".set  reorder"            "\n\t"    \
      : "=r" (out)                         \
      : "r" (RSval)                        \
      : #RD                                \
   );                                      \
   printf("B :: 0x%llx, RSval: 0x%llx\n",  \
          out, (long long) RSval);         \
}

#define TEST2(RSval, RD)                          \
{                                                 \
   unsigned long long out = 0;                    \
   __asm__ __volatile__(                          \
      ".set  noreorder"          "\n\t"           \
      "move  $"#RD", %1"         "\n\t"           \
      "bal   end1"#RD            "\n\t"           \
      "nop"                      "\n\t"           \
      "daddi $"#RD", $"#RD", 5"  "\n\t"           \
      "b     r_end"#RD           "\n\t"           \
      "nop"                      "\n\t"           \
      "end1"#RD":"               "\n\t"           \
      "daddi $"#RD", $"#RD", 1"  "\n\t"           \
      "jr    $ra"                "\n\t"           \
      "nop"                      "\n\t"           \
      "r_end"#RD":"              "\n\t"           \
      "move  %0,     $"#RD       "\n\t"           \
      ".set  reorder"            "\n\t"           \
      : "=r" (out)                                \
      : "r" (RSval)                               \
      : #RD, "ra"                                 \
   );                                             \
   printf("B BAL JR :: 0x%llx, RSval: 0x%llx\n",  \
          out, (long long) RSval);                \
}

#define TEST2a(RSval, RD)                           \
{                                                   \
   unsigned long long out = 0;                      \
   __asm__ __volatile__(                            \
      ".set  noreorder"          "\n\t"             \
      "move  $"#RD", %1"         "\n\t"             \
      "bal   end12"#RD           "\n\t"             \
      "nop"                      "\n\t"             \
      "daddi $"#RD", $"#RD", 5"  "\n\t"             \
      "move  $t9, $ra"           "\n\t"             \
      "jr    $t9"                "\n\t"             \
      "nop"                      "\n\t"             \
      "daddi $"#RD", $"#RD", 1"  "\n\t"             \
      "end12"#RD":"              "\n\t"             \
      "daddi $"#RD", $"#RD", 1"  "\n\t"             \
      "move  $t9, $ra"           "\n\t"             \
      "jal   $t9"                "\n\t"             \
      "nop"                      "\n\t"             \
      "move  %0,     $"#RD       "\n\t"             \
      ".set  reorder"            "\n\t"             \
      : "=r" (out)                                  \
      : "r" (RSval)                                 \
      : #RD, "t9", "ra"                             \
   );                                               \
   printf("BAL JR JAL :: 0x%llx, RSval: 0x%llx\n",  \
          out, (long long) RSval);                  \
}

#define TEST2b(RSval, RD)                           \
{                                                   \
   unsigned long long out = 0;                      \
   __asm__ __volatile__(                            \
      ".set  noreorder"          "\n\t"             \
      "move  $"#RD", %1"         "\n\t"             \
      "bal   end13"#RD           "\n\t"             \
      "nop"                      "\n\t"             \
      "daddi $"#RD", $"#RD", 5"  "\n\t"             \
      "move  $t9,    $t0"        "\n\t"             \
      "j     $t9"                "\n\t"             \
      "nop"                      "\n\t"             \
      "daddi $"#RD", $"#RD", 1"  "\n\t"             \
      "end13"#RD":"              "\n\t"             \
      "daddi $"#RD", $"#RD", 1"  "\n\t"             \
      "move  $t9,    $ra"        "\n\t"             \
      "jalr  $t0,    $t9"        "\n\t"             \
      "nop"                      "\n\t"             \
      "move  %0,     $"#RD       "\n\t"             \
      ".set  reorder"            "\n\t"             \
      : "=r" (out)                                  \
      : "r" (RSval)                                 \
      : #RD, "t0", "t9", "ra"                       \
   );                                               \
   printf("BAL JR JAL :: 0x%llx, RSval: 0x%llx\n",  \
          out, (long long) RSval);                  \
}

#define TEST3(instruction, RDval, RSval, RTval, RD, RS, RT)              \
{                                                                        \
   unsigned long long out = 0;                                           \
   __asm__ __volatile__(                                                 \
      ".set        noreorder"                             "\n\t"         \
      "move        $"#RS", %1"                            "\n\t"         \
      "move        $"#RT", %2"                            "\n\t"         \
      "move        $"#RD", %3"                            "\n\t"         \
      instruction" $"#RS", $"#RT", end"instruction#RDval  "\n\t"         \
      "nop"                                               "\n\t"         \
      "daddi       $"#RD", $"#RD", 5"                     "\n\t"         \
      "end"instruction#RDval":"                           "\n\t"         \
      "daddi       $"#RD", $"#RD", 1"                     "\n\t"         \
      "move        %0,     $"#RD                          "\n\t"         \
      ".set        reorder"                               "\n\t"         \
      : "=r" (out)                                                       \
      : "r" (RSval), "r" (RTval), "r" (RDval)                            \
      : #RD, #RS, #RT                                                    \
   );                                                                    \
   printf(instruction" :: out: 0x%llx, RSval: 0x%llx, RTval: 0x%llx\n",  \
          out, (long long) RSval, (long long) RTval);                    \
}

#define TEST4(instruction, RDval, RSval, RD, RS)          \
{                                                         \
   unsigned long long out = 0;                            \
   __asm__ __volatile__(                                  \
      ".set        noreorder"                     "\n\t"  \
      "move        $"#RS", %1"                    "\n\t"  \
      "move        $"#RD", %2"                    "\n\t"  \
      instruction" $"#RS", end"instruction#RDval  "\n\t"  \
      "nop"                                       "\n\t"  \
      "daddi       $"#RD", $"#RD", 8"             "\n\t"  \
      "end"instruction#RDval":"                   "\n\t"  \
      "daddi       $"#RD", $"#RD", 1"             "\n\t"  \
      "move        %0,     $"#RD                  "\n\t"  \
      ".set        reorder"                       "\n\t"  \
      : "=r" (out)                                        \
      : "r" (RSval), "r" (RDval)                          \
      : #RD, #RS                                          \
   );                                                     \
   printf(instruction" :: out: 0x%llx, RSval: 0x%llx\n",  \
          out, (long long) RSval);                        \
}

#define TEST5(instruction, RDval, RSval, RD, RS)            \
{                                                           \
   unsigned long long out = 0;                              \
   __asm__ __volatile__(                                    \
      ".set        noreorder"                       "\n\t"  \
      "move        $"#RD", %2"                      "\n\t"  \
      "move        $"#RS", %1"                      "\n\t"  \
      instruction" $"#RS", end21"instruction#RDval  "\n\t"  \
      "nop"                                         "\n\t"  \
      "daddi       $"#RD", $"#RD", 5"               "\n\t"  \
      "b           r_end"instruction#RDval          "\n\t"  \
      "nop"                                         "\n\t"  \
      "end21"instruction#RDval":"                   "\n\t"  \
      "daddi       $"#RD", $"#RD", 1"               "\n\t"  \
      "jr          $ra"                             "\n\t"  \
      "r_end"instruction#RDval":"                   "\n\t"  \
      "move        %0,     $"#RD                    "\n\t"  \
      ".set        reorder"                         "\n\t"  \
      : "=r" (out)                                          \
      : "r" (RSval), "r" (RDval)                            \
      : #RD, #RS, "ra"                                      \
   );                                                       \
   printf(instruction" :: out: 0x%llx, RSval: 0x%llx\n",    \
          out, (long long) RSval);                          \
}

int main()
{
   int i;
   init_reg_val2();
#if (__mips_isa_rev < 6)

   printf("B \n");
   for (i = 0; i < N; i++)
      TEST1(reg_val1[i], t0);

   printf("BAL \n");
   for (i = 0; i < N; i++)
      TEST2(reg_val1[i], t0);

   printf("--- BEQ ---  if RSval == RTval then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TEST3("beq", 0,  0,          1,          2,  3,  4);
   TEST3("beq", 1,  1,          1,          3,  4,  5);
   TEST3("beq", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TEST3("beq", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TEST3("beq", 4,  0xfffffffe, 0xffffffff, 6,  7,  8);
   TEST3("beq", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TEST3("beq", 6,  0x5,        0x5,        8,  9,  10);
   TEST3("beq", 7,  -3,         -4,         9,  10, 11);
   TEST3("beq", 8,  125,        125,        10, 11, 12);
   TEST3("beq", 9,  0x80000000, 0x80000000, 11, 12, 15);
   TEST3("beq", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TEST3("beq", 11, 0x256,      0x256,      13, 14, 15);
   TEST3("beq", 12, 0x55,       0x55,       14, 15, 16);
   TEST3("beq", 13, 0xfff,      0xdd,       15, 16, 17);
   TEST3("beq", 14, -1,         0x5,        16, 17, 18);
   TEST3("beq", 15, -1,         -1,         17, 18, 19);

   printf("--- BGEZ ---  if RSval >= 0 then " \
          "out = RDval + 1 else out = RDval + 9\n");
   TEST4("bgez", 0,  0,          2,  3);
   TEST4("bgez", 1,  1,          3,  4);
   TEST4("bgez", 2,  0xffffffff, 4,  5);
   TEST4("bgez", 3,  0xffffffff, 5,  6);
   TEST4("bgez", 4,  0xfffffffe, 6,  7);
   TEST4("bgez", 5,  0xffffffff, 7,  8);
   TEST4("bgez", 6,  0x5,        8,  9);
   TEST4("bgez", 7,  -3,         9,  10);
   TEST4("bgez", 8,  125,        10, 11);
   TEST4("bgez", 9,  0x80000000, 11, 12);
   TEST4("bgez", 10, 0xffffffff, 12, 13);
   TEST4("bgez", 11, 0x256,      13, 14);
   TEST4("bgez", 12, 0x55,       14, 15);
   TEST4("bgez", 13, 0xfff,      15, 16);
   TEST4("bgez", 14, -1,         16, 17);
   TEST4("bgez", 15, -1,         17, 18);

   printf("--- BGEZAL ---  if RSval >= 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TEST5("bgezal", 0,  0,          2,  3);
   TEST5("bgezal", 1,  1,          3,  4);
   TEST5("bgezal", 2,  0xffffffff, 4,  5);
   TEST5("bgezal", 3,  0xffffffff, 5,  6);
   TEST5("bgezal", 4,  0xfffffffe, 6,  7);
   TEST5("bgezal", 5,  0xffffffff, 7,  8);
   TEST5("bgezal", 6,  0x5,        8,  9);
   TEST5("bgezal", 7,  -3,         9,  10);
   TEST5("bgezal", 8,  125,        10, 11);
   TEST5("bgezal", 9,  0x80000000, 11, 12);
   TEST5("bgezal", 10, 0xffffffff, 12, 13);
   TEST5("bgezal", 11, 0x256,      13, 14);
   TEST5("bgezal", 12, 0x55,       14, 15);
   TEST5("bgezal", 13, 0xfff,      15, 16);
   TEST5("bgezal", 14, -1,         16, 17);
   TEST5("bgezal", 15, -1,         17, 18);

   printf("--- BGTZ ---  if RSval > 0 then " \
          "out = RDval + 1 else out = RDval + 9\n");
   TEST4("bgtz", 0,  0,          2,  3);
   TEST4("bgtz", 1,  1,          3,  4);
   TEST4("bgtz", 2,  0xffffffff, 4,  5);
   TEST4("bgtz", 3,  0xffffffff, 5,  6);
   TEST4("bgtz", 4,  0xfffffffe, 6,  7);
   TEST4("bgtz", 5,  0xffffffff, 7,  8);
   TEST4("bgtz", 6,  0x5,        8,  9);
   TEST4("bgtz", 7,  -3,         9,  10);
   TEST4("bgtz", 8,  125,        10, 11);
   TEST4("bgtz", 9,  0x80000000, 11, 12);
   TEST4("bgtz", 10, 0xffffffff, 12, 13);
   TEST4("bgtz", 11, 0x256,      13, 14);
   TEST4("bgtz", 12, 0x55,       14, 15);
   TEST4("bgtz", 13, 0xfff,      15, 16);
   TEST4("bgtz", 14, -1,         16, 17);
   TEST4("bgtz", 15, -1,         17, 18);

   printf("--- BLEZ ---  if RSval <= 0 then " \
          "out = RDval + 1 else out = RDval + 9\n");
   TEST4("blez", 0,  0,          2,  3);
   TEST4("blez", 1,  1,          3,  4);
   TEST4("blez", 2,  0xffffffff, 4,  5);
   TEST4("blez", 3,  0xffffffff, 5,  6);
   TEST4("blez", 4,  0xfffffffe, 6,  7);
   TEST4("blez", 5,  0xffffffff, 7,  8);
   TEST4("blez", 6,  0x5,        8,  9);
   TEST4("blez", 7,  -3,         9,  10);
   TEST4("blez", 8,  125,        10, 11);
   TEST4("blez", 9,  0x80000000, 11, 12);
   TEST4("blez", 10, 0xffffffff, 12, 13);
   TEST4("blez", 11, 0x256,      13, 14);
   TEST4("blez", 12, 0x55,       14, 15);
   TEST4("blez", 13, 0xfff,      15, 16);
   TEST4("blez", 14, -1,         16, 17);
   TEST4("blez", 15, -1,         17, 18);

   printf("--- BLTZ ---  if RSval < 0 then " \
          "out = RDval + 1 else out = RDval + 9\n");
   TEST4("bltz", 0,  0,          2,  3);
   TEST4("bltz", 1,  1,          3,  4);
   TEST4("bltz", 2,  0xffffffff, 4,  5);
   TEST4("bltz", 3,  0xffffffff, 5,  6);
   TEST4("bltz", 4,  0xfffffffe, 6,  7);
   TEST4("bltz", 5,  0xffffffff, 7,  8);
   TEST4("bltz", 6,  0x5,        8,  9);
   TEST4("bltz", 7,  -3,         9,  10);
   TEST4("bltz", 8,  125,        10, 11);
   TEST4("bltz", 9,  0x80000000, 11, 12);
   TEST4("bltz", 10, 0xffffffff, 12, 13);
   TEST4("bltz", 11, 0x256,      13, 14);
   TEST4("bltz", 12, 0x55,       14, 15);
   TEST4("bltz", 13, 0xfff,      15, 16);
   TEST4("bltz", 14, -1,         16, 17);
   TEST4("bltz", 15, -1,         17, 18);

   printf("--- BLTZAL ---  if RSval < 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TEST5("bltzal", 0, 0,           2,  3);
   TEST5("bltzal", 1, 1,           3,  4);
   TEST5("bltzal", 2, 0xffffffff,  4,  5);
   TEST5("bltzal", 3, 0xffffffff,  5,  6);
   TEST5("bltzal", 4, 0xfffffffe,  6,  7);
   TEST5("bltzal", 5, 0xffffffff,  7,  8);
   TEST5("bltzal", 6, 0x5,         8,  9);
   TEST5("bltzal", 7, -3,          9,  10);
   TEST5("bltzal", 8, 125,         10, 11);
   TEST5("bltzal", 9, 0x80000000,  11, 12);
   TEST5("bltzal", 10, 0xffffffff, 12, 13);
   TEST5("bltzal", 11, 0x256,      13, 14);
   TEST5("bltzal", 12, 0x55,       14, 15);
   TEST5("bltzal", 13, 0xfff,      15, 16);
   TEST5("bltzal", 14, -1,         16, 17);
   TEST5("bltzal", 15, -1,         17, 18);

   printf("--- BNE ---  if RSval != RTval then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TEST3("bne", 0,  0,          1,          2,  3,  4);
   TEST3("bne", 1,  1,          1,          3,  4,  5);
   TEST3("bne", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TEST3("bne", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TEST3("bne", 4,  0xfffffffe, 0xffffffff, 6,  7,  8);
   TEST3("bne", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TEST3("bne", 6,  0x5,        0x5,        8,  9,  10);
   TEST3("bne", 7,  -3,         -4,         9,  10, 11);
   TEST3("bne", 8,  125,        125,        10, 11, 12);
   TEST3("bne", 9,  0x80000000, 0x80000000, 11, 12, 15);
   TEST3("bne", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TEST3("bne", 11, 0x256,      0x256,      13, 14, 15);
   TEST3("bne", 12, 0x55,       0x55,       14, 15, 16);
   TEST3("bne", 13, 0xfff,      0xdd,       15, 16, 17);
   TEST3("bne", 14, -1,         0x5,        16, 17, 18);
   TEST3("bne", 15, -1,         -1,         17, 18, 19);

   printf("JAL, JR \n");
   for (i = 0; i < N; i++)
      TEST2a(reg_val1[i], t0);

   printf("J, JALR \n");
   for (i = 0; i < N; i++)
      TEST2b(reg_val1[i], t1);
#endif
   return 0;
}
