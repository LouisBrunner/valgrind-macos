#include <stdio.h>

#define TESTINST1(RSval, RD)            \
{                                       \
   unsigned int out = 0;                \
   __asm__ __volatile__(                \
      ".set noreorder"          "\n\t"  \
      "move $"#RD", %1"         "\n\t"  \
      "b    end"#RSval          "\n\t"  \
      "nop"                     "\n\t"  \
      "addiu $"#RD", $"#RD", 5"  "\n\t" \
      "end"#RSval":"            "\n\t"  \
      "addiu $"#RD", $"#RD", 1"  "\n\t" \
      "move %0,     $"#RD       "\n\t"  \
      ".set reorder"            "\n\t"  \
      : "=r" (out)                      \
      : "r" (RSval)                     \
      : #RD                             \
   );                                   \
   printf("B :: %d, RSval: %d\n",       \
          out, RSval);                  \
}

#define TESTINST2(RSval, RD)            \
{                                       \
   unsigned int out = 0;                \
   __asm__ __volatile__(                \
      ".set noreorder"          "\n\t"  \
      "move $"#RD", %1"         "\n\t"  \
      "b    end12"#RSval        "\n\t"  \
      "addiu $"#RD", $"#RD", 3"  "\n\t" \
      "addiu $"#RD", $"#RD", 5"  "\n\t" \
      "end12"#RSval":"          "\n\t"  \
      "addiu $"#RD", $"#RD", 3"  "\n\t" \
      "move %0,     $"#RD       "\n\t"  \
      ".set reorder"            "\n\t"  \
      : "=r" (out)                      \
      : "r" (RSval)                     \
      : #RD                             \
   );                                   \
   printf("B :: %d, RSval: %d\n",       \
          out, RSval);                  \
}

#define TESTINST3(RSval, RD)              \
{                                         \
   unsigned int out = 0;                  \
   __asm__ __volatile__(                  \
      ".set noreorder"          "\n\t"    \
      "move $"#RD", %1"         "\n\t"    \
      "bal  end21"#RSval        "\n\t"    \
      "nop"                     "\n\t"    \
      "addiu $"#RD", $"#RD", 5"  "\n\t"   \
      "b    r_end"#RSval        "\n\t"    \
      "nop"                     "\n\t"    \
      "addiu $"#RD", $"#RD", 1"  "\n\t"   \
      "end21"#RSval":"          "\n\t"    \
      "addiu $"#RD", $"#RD", 1"  "\n\t"   \
      "jr   $ra"                "\n\t"    \
      "nop"                     "\n\t"    \
      "r_end"#RSval":"          "\n\t"    \
      "move %0,     $"#RD       "\n\t"    \
      ".set reorder"            "\n\t"    \
      : "=r" (out)                        \
      : "r" (RSval)                       \
      : #RD, "ra"                         \
   );                                     \
   printf("B BAL JR :: %d, RSval: %d\n",  \
          out, RSval);                    \
}

#define TESTINST3j(RSval, RD)               \
{                                           \
   unsigned int out = 0;                    \
   __asm__ __volatile__(                    \
      ".set noreorder"              "\n\t"  \
      "move $"#RD", %1"             "\n\t"  \
      "dla  $t9,    end31"#RSval    "\n\t"  \
      "jal  $t9"                    "\n\t"  \
      "nop"                         "\n\t"  \
      "addiu $"#RD", $"#RD", 5"      "\n\t" \
      "dla  $t9,    r_end11"#RSval  "\n\t"  \
      "j    $t9"                    "\n\t"  \
      "nop"                         "\n\t"  \
      "end31"#RSval":"              "\n\t"  \
      "addiu $"#RD", $"#RD", 1"      "\n\t" \
      "jr   $ra"                    "\n\t"  \
      "nop"                         "\n\t"  \
      "r_end11"#RSval":"            "\n\t"  \
      "move %0, $"#RD               "\n\t"  \
      ".set reorder"                "\n\t"  \
      : "=r" (out)                          \
      : "r" (RSval)                         \
      : #RD, "t9"                           \
   );                                       \
   printf("J JAL JR :: %d, RSval: %d\n",    \
          out, RSval);                      \
}

#define TESTINST3ja(RSval, RD)             \
{                                          \
   unsigned int out = 0;                   \
   __asm__ __volatile__(                   \
      ".set noreorder"            "\n\t"   \
      "move $"#RD", %1"           "\n\t"   \
      "dla  $t9,    end41"#RSval  "\n\t"   \
      "jalr $t0,    $t9"          "\n\t"   \
      "nop"                       "\n\t"   \
      "addiu $"#RD", $"#RD", 5"    "\n\t"  \
      "dla  $t9, r_end21"#RSval   "\n\t"   \
      "j    $t9"                  "\n\t"   \
      "nop"                       "\n\t"   \
      "addiu $"#RD", $"#RD", 1"    "\n\t"  \
      "end41"#RSval":"            "\n\t"   \
      "addiu $"#RD", $"#RD", 1"    "\n\t"  \
      "move $t9,    $t0"          "\n\t"   \
      "jr   $t9"                  "\n\t"   \
      "nop"                       "\n\t"   \
      "r_end21"#RSval":"          "\n\t"   \
      "move %0,     $"#RD         "\n\t"   \
      ".set reorder"              "\n\t"   \
      : "=r" (out)                         \
      : "r" (RSval)                        \
      : #RD, "t0", "t9"                    \
   );                                      \
   printf("J JALR JR :: %d, RSval: %d\n",  \
          out, RSval);                     \
}

#define TESTINST4(instruction, RDval, RSval, RTval, RD, RS, RT)         \
{                                                                       \
   unsigned int out = 0;                                                \
   __asm__ __volatile__(                                                \
      ".set noreorder"                                    "\n\t"        \
      "move        $"#RS", %1"                            "\n\t"        \
      "move        $"#RT", %2"                            "\n\t"        \
      "move        $"#RD", %3"                            "\n\t"        \
      instruction" $"#RS", $"#RT", end"instruction#RDval  "\n\t"        \
      "nop"                                               "\n\t"        \
      "addiu        $"#RD", $"#RD", 5"                     "\n\t"       \
      "end"instruction#RDval":"                           "\n\t"        \
      "addiu        $"#RD", $"#RD", 1"                     "\n\t"       \
      "move        %0,     $" #RD                         "\n\t"        \
      ".set reorder"                                      "\n\t"        \
      : "=r" (out)                                                      \
      : "r" (RSval), "r" (RTval), "r" (RDval)                           \
      : #RD, #RS, #RT                                                   \
   );                                                                   \
   printf(instruction" :: out: %d, RDval: %d, RSval: %d, RTval: %d\n",  \
          out, RDval, RSval, RTval);                                    \
}

#define TESTINST5(instruction, RDval, RSval, RD, RS)          \
{                                                             \
   unsigned int out = 0;                                      \
   __asm__ __volatile__(                                      \
      ".set        noreorder"                     "\n\t"      \
      "move        $"#RS", %1"                    "\n\t"      \
      "move        $"#RD", %2"                    "\n\t"      \
      instruction" $"#RS", end"instruction#RDval  "\n\t"      \
      "nop"                                       "\n\t"      \
      "addiu        $"#RD", $"#RD", 5"             "\n\t"     \
      "end"instruction#RDval":"                   "\n\t"      \
      "addiu        $"#RD", $"#RD", 1"             "\n\t"     \
      "move        %0,     $"#RD                  "\n\t"      \
      ".set        reorder"                       "\n\t"      \
      : "=r" (out)                                            \
      : "r" (RSval), "r" (RDval)                              \
      : #RD, #RS                                              \
    );                                                        \
    printf(instruction" :: out: %d, RDval: %d, RSval: %d\n",  \
           out, RDval, RSval);                                \
}

#define TESTINST6(instruction, RDval, RSval, RD, RS)         \
{                                                            \
   unsigned int out = 0;                                     \
   __asm__ __volatile__(                                     \
      ".set        noreorder"                       "\n\t"   \
      "move        $"#RD", %2"                      "\n\t"   \
      "move        $"#RS", %1"                      "\n\t"   \
      instruction" $"#RS", end21"instruction#RDval  "\n\t"   \
      "nop"                                         "\n\t"   \
      "addiu        $"#RD", $"#RD", 5"               "\n\t"  \
      "b           r_end"instruction#RDval          "\n\t"   \
      "nop"                                         "\n\t"   \
      "end21"instruction#RDval":"                   "\n\t"   \
      "addiu        $"#RD", $"#RD", 1"               "\n\t"  \
      "jr          $ra"                             "\n\t"   \
      "r_end"instruction#RDval":"                   "\n\t"   \
      "move        %0, $"#RD                        "\n\t"   \
      ".set        reorder"                         "\n\t"   \
      : "=r" (out)                                           \
      : "r" (RSval), "r" (RDval)                             \
      : #RD, #RS, "ra"                                       \
   );                                                        \
   printf(instruction" :: out: %d, RDval: %d, RSval: %d\n",  \
          out, RDval, RSval);                                \
}

#define TESTINST4l(instruction, RDval, RSval, RTval, RD, RS, RT)        \
{                                                                       \
   unsigned int out = 0;                                                \
   __asm__ __volatile__(                                                \
      ".set        noreorder"                             "\n\t"        \
      "move        $"#RS", %1"                            "\n\t"        \
      "move        $"#RT", %2"                            "\n\t"        \
      "move        $"#RD", %3"                            "\n\t"        \
      instruction" $"#RS", $"#RT", end"instruction#RDval  "\n\t"        \
      "addiu        $"#RD", $"#RD", 3"                     "\n\t"       \
      "addiu        $"#RD", $"#RD", 5"                     "\n\t"       \
      "end"instruction#RDval":"                           "\n\t"        \
      "addiu        $"#RD", $"#RD", 1"                     "\n\t"       \
      "move        %0, $"#RD                              "\n\t"        \
      ".set        reorder"                               "\n\t"        \
      : "=r" (out)                                                      \
      : "r" (RSval), "r" (RTval), "r" (RDval)                           \
      : #RD, #RS, #RT                                                   \
   );                                                                   \
   printf(instruction" :: out: %d, RDval: %d, RSval: %d, RTval: %d\n",  \
          out, RDval, RSval, RTval);                                    \
}

#define TESTINST5l(instruction, RDval, RSval, RD, RS)        \
{                                                            \
   unsigned int out = 0;                                     \
   __asm__ __volatile__(                                     \
      ".set        noreorder"                     "\n\t"     \
      "move        $"#RS", %1"                    "\n\t"     \
      "move        $"#RD", %2"                    "\n\t"     \
      instruction" $"#RS", end"instruction#RDval  "\n\t"     \
      "addiu        $"#RD", $"#RD", 3"             "\n\t"    \
      "addiu        $"#RD", $"#RD", 5"             "\n\t"    \
      "end"instruction#RDval":"                   "\n\t"     \
      "addiu        $"#RD", $"#RD", 1"             "\n\t"    \
      "move        %0,     $"#RD                  "\n\t"     \
      ".set        reorder"                       "\n\t"     \
      : "=r" (out)                                           \
      : "r" (RSval), "r" (RDval)                             \
      : #RD, #RS                                             \
   );                                                        \
   printf(instruction" :: out: %d, RDval: %d, RSval: %d\n",  \
          out, RDval, RSval);                                \
}

#define TESTINST6l(instruction, RDval, RSval, RD, RS)        \
{                                                            \
   unsigned int out = 0;                                     \
   __asm__ __volatile__(                                     \
      ".set        noreorder"                       "\n\t"   \
      "move        $"#RD", %2"                      "\n\t"   \
      "move        $"#RS", %1"                      "\n\t"   \
      instruction" $"#RS", end21"instruction#RDval  "\n\t"   \
      "addiu        $"#RD", $"#RD", 3"               "\n\t"  \
      "addiu        $"#RD", $"#RD", 5"               "\n\t"  \
      "b           r_end"instruction#RDval          "\n\t"   \
      "nop"                                         "\n\t"   \
      "end21"instruction#RDval":"                   "\n\t"   \
      "addiu        $"#RD", $"#RD", 1"               "\n\t"  \
      "jr          $ra"                             "\n\t"   \
      "nop"                                         "\n\t"   \
      "r_end"instruction#RDval":"                   "\n\t"   \
      "move        %0, $"#RD                        "\n\t"   \
      ".set        reorder"                         "\n\t"   \
      : "=r" (out)                                           \
      : "r" (RSval), "r" (RDval)                             \
      : #RD, #RS, "ra"                                       \
   );                                                        \
   printf(instruction" :: out: %d, RDval: %d, RSval: %d\n",  \
          out, RDval, RSval);                                \
}


int main()
{

   printf("b\n");
   TESTINST1(0,  2);
   TESTINST1(1,  3);
   TESTINST1(2,  4);
   TESTINST1(3,  5);
   TESTINST1(4,  6);
   TESTINST1(5,  7);
   TESTINST1(6,  8);
   TESTINST1(7,  9);
   TESTINST1(8,  10);
   TESTINST1(9,  11);
   TESTINST1(10, 12);
   TESTINST1(11, 13);
   TESTINST1(12, 14);
   TESTINST1(13, 15);
   TESTINST1(14, 16);
   TESTINST1(15, 17);
   TESTINST1(16, 18);
   TESTINST1(17, 19);
   TESTINST1(18, 20);
   TESTINST1(19, 21);
   TESTINST1(20, 22);
   TESTINST1(21, 23);
   TESTINST1(22, 24);
   TESTINST1(23, 25);

   printf("b\n");
   TESTINST2(0,  2);
   TESTINST2(1,  3);
   TESTINST2(2,  4);
   TESTINST2(3,  5);
   TESTINST2(4,  6);
   TESTINST2(5,  7);
   TESTINST2(6,  8);
   TESTINST2(7,  9);
   TESTINST2(8,  10);
   TESTINST2(9,  11);
   TESTINST2(10, 12);
   TESTINST2(11, 13);
   TESTINST2(12, 14);
   TESTINST2(13, 15);
   TESTINST2(14, 16);
   TESTINST2(15, 17);
   TESTINST2(16, 18);
   TESTINST2(17, 19);
   TESTINST2(18, 20);
   TESTINST2(19, 21);
   TESTINST2(20, 22);
   TESTINST2(21, 23);
   TESTINST2(22, 24);
   TESTINST2(23, 25);

   printf("b, bal, jr\n");
   TESTINST3(0,  2);
   TESTINST3(1,  3);
   TESTINST3(2,  4);
   TESTINST3(3,  5);
   TESTINST3(4,  6);
   TESTINST3(5,  7);
   TESTINST3(6,  8);
   TESTINST3(7,  9);
   TESTINST3(8,  10);
   TESTINST3(9,  11);
   TESTINST3(10, 12);
   TESTINST3(11, 13);
   TESTINST3(12, 14);
   TESTINST3(13, 15);
   TESTINST3(14, 16);
   TESTINST3(15, 17);
   TESTINST3(16, 18);
   TESTINST3(17, 19);
   TESTINST3(18, 20);
   TESTINST3(19, 21);
   TESTINST3(20, 22);
   TESTINST3(21, 23);
   TESTINST3(22, 24);
   TESTINST3(23, 25);

   printf("--- BEQ ---  if RSval == RTval then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST4("beq", 0,  0,          1,          2,  3 , 4);
   TESTINST4("beq", 1,  1,          1,          3,  4,  5);
   TESTINST4("beq", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TESTINST4("beq", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TESTINST4("beq", 4,  0xfffffffe, 0xffffffff, 6,  8,  9);
   TESTINST4("beq", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TESTINST4("beq", 6,  0x5,        0x5,        8,  9,  10);
   TESTINST4("beq", 7,  -3,         -4,         9,  10, 11);
   TESTINST4("beq", 8,  125,        125,        10, 11, 12);
   TESTINST4("beq", 9,  0x80000000, 0x80000000, 11, 12, 13);
   TESTINST4("beq", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("beq", 11, 0x256,      0x256,      13, 14, 15);
   TESTINST4("beq", 12, 0x55,       0x55,       14, 15, 16);
   TESTINST4("beq", 13, 0xfff,      0xdd,       16, 17, 18);
   TESTINST4("beq", 14, -1,         0x5,        2,  25, 24);
   TESTINST4("beq", 15, -1,         -1,         25, 24, 7);

   printf("--- BNE ---  if RSval != RTval then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST4("bne", 0,  0,          1,          2,  3,  4);
   TESTINST4("bne", 1,  1,          1,          3,  4,  5);
   TESTINST4("bne", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TESTINST4("bne", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TESTINST4("bne", 4,  0xfffffffe, 0xffffffff, 6,  8,  9);
   TESTINST4("bne", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TESTINST4("bne", 6,  0x5,        0x5,        8,  9,  10);
   TESTINST4("bne", 7,  -3,         -4,         9,  10, 11);
   TESTINST4("bne", 8,  125,        125,        10, 11, 12);
   TESTINST4("bne", 9,  0x80000000, 0x80000000, 11, 12, 13);
   TESTINST4("bne", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bne", 11, 0x256,      0x256,      13, 14, 15);
   TESTINST4("bne", 12, 0x55,       0x55,       14, 15, 16);
   TESTINST4("bne", 13, 0xfff,      0xdd,       16, 17, 18);
   TESTINST4("bne", 14, -1,         0x5,        2,  25, 24);
   TESTINST4("bne", 15, -1,         -1,         25, 24, 7);

   printf("--- BEQZ ---  if RSval == 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("beqz", 0,  0,          2,  3);
   TESTINST5("beqz", 1,  1,          3,  4);
   TESTINST5("beqz", 2,  0xffffffff, 4,  5);
   TESTINST5("beqz", 3,  0xffffffff, 5,  6);
   TESTINST5("beqz", 4,  0xfffffffe, 6,  8);
   TESTINST5("beqz", 5,  0xffffffff, 7,  8);
   TESTINST5("beqz", 6,  0x5,        8,  9);
   TESTINST5("beqz", 7,  -3,         9,  10);
   TESTINST5("beqz", 8,  125,        10, 11);
   TESTINST5("beqz", 9,  0x80000000, 11, 12);
   TESTINST5("beqz", 10, 0xffffffff, 12, 13);
   TESTINST5("beqz", 11, 0x256,      13, 14);
   TESTINST5("beqz", 12, 0x55,       14, 15);
   TESTINST5("beqz", 13, 0xfff,      16, 17);
   TESTINST5("beqz", 14, -1,         2,  25);
   TESTINST5("beqz", 15, -1,         25, 24);

   printf("--- BGEZ ---  if RSval >= 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("bgez", 0,  0,          2,  3);
   TESTINST5("bgez", 1,  1,          3,  4);
   TESTINST5("bgez", 2,  0xffffffff, 4,  5);
   TESTINST5("bgez", 3,  0xffffffff, 5,  6);
   TESTINST5("bgez", 4,  0xfffffffe, 6,  8);
   TESTINST5("bgez", 5,  0xffffffff, 7,  8);
   TESTINST5("bgez", 6,  0x5,        8,  9);
   TESTINST5("bgez", 7,  -3,         9,  10);
   TESTINST5("bgez", 8,  125,        10, 11);
   TESTINST5("bgez", 9,  0x80000000, 11, 12);
   TESTINST5("bgez", 10, 0xffffffff, 12, 13);
   TESTINST5("bgez", 11, 0x256,      13, 14);
   TESTINST5("bgez", 12, 0x55,       14, 15);
   TESTINST5("bgez", 13, 0xfff,      16, 17);
   TESTINST5("bgez", 14, -1,         2,  25);
   TESTINST5("bgez", 15, -1,         25, 24);

   printf("--- BGTZ ---  if RSval > 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("bgtz", 0,  0,          2,  3);
   TESTINST5("bgtz", 1,  1,          3,  4);
   TESTINST5("bgtz", 2,  0xffffffff, 4,  5);
   TESTINST5("bgtz", 3,  0xffffffff, 5,  6);
   TESTINST5("bgtz", 4,  0xfffffffe, 6,  8);
   TESTINST5("bgtz", 5,  0xffffffff, 7,  8);
   TESTINST5("bgtz", 6,  0x5,        8,  9);
   TESTINST5("bgtz", 7,  -3,         9,  10);
   TESTINST5("bgtz", 8,  125,        10, 11);
   TESTINST5("bgtz", 9,  0x80000000, 11, 12);
   TESTINST5("bgtz", 10, 0xffffffff, 12, 13);
   TESTINST5("bgtz", 11, 0x256,      13, 14);
   TESTINST5("bgtz", 12, 0x55,       14, 15);
   TESTINST5("bgtz", 13, 0xfff,      16, 17);
   TESTINST5("bgtz", 14, -1,         2,  25);
   TESTINST5("bgtz", 15, -1,         25, 24);

   printf("--- BLEZ ---  if RSval <= 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("blez", 0,  0,          2,  3);
   TESTINST5("blez", 1,  1,          3,  4);
   TESTINST5("blez", 2,  0xffffffff, 4,  5);
   TESTINST5("blez", 3,  0xffffffff, 5,  6);
   TESTINST5("blez", 4,  0xfffffffe, 6,  8);
   TESTINST5("blez", 5,  0xffffffff, 7,  8);
   TESTINST5("blez", 6,  0x5,        8,  9);
   TESTINST5("blez", 7,  -3,         9,  10);
   TESTINST5("blez", 8,  125,        10, 11);
   TESTINST5("blez", 9,  0x80000000, 11, 12);
   TESTINST5("blez", 10, 0xffffffff, 12, 13);
   TESTINST5("blez", 11, 0x256,      13, 14);
   TESTINST5("blez", 12, 0x55,       14, 15);
   TESTINST5("blez", 13, 0xfff,      16, 17);
   TESTINST5("blez", 14, -1,         2,  25);
   TESTINST5("blez", 15, -1,         25, 24);

   printf("--- BLTZ ---  if RSval < 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("bltz", 0,  0,          2,  3);
   TESTINST5("bltz", 1,  1,          3,  4);
   TESTINST5("bltz", 2,  0xffffffff, 4,  5);
   TESTINST5("bltz", 3,  0xffffffff, 5,  6);
   TESTINST5("bltz", 4,  0xfffffffe, 6,  8);
   TESTINST5("bltz", 5,  0xffffffff, 7,  8);
   TESTINST5("bltz", 6,  0x5,        8,  9);
   TESTINST5("bltz", 7,  -3,         9,  10);
   TESTINST5("bltz", 8,  125,        10, 11);
   TESTINST5("bltz", 9,  0x80000000, 11, 12);
   TESTINST5("bltz", 10, 0xffffffff, 12, 13);
   TESTINST5("bltz", 11, 0x256,      13, 14);
   TESTINST5("bltz", 12, 0x55,       14, 15);
   TESTINST5("bltz", 13, 0xfff,      16, 17);
   TESTINST5("bltz", 14, -1,         2,  25);
   TESTINST5("bltz", 15, -1,         25, 24);
#if (__mips_isa_rev < 6)
   printf("--- BGEZAL ---  if RSval >= 0 then " \
          "out = RDval + 6 else out = RDval + 5\n");
   TESTINST6("bgezal", 0,  0,          2,  3);
   TESTINST6("bgezal", 1,  1,          3,  4);
   TESTINST6("bgezal", 2,  0xffffffff, 4,  5);
   TESTINST6("bgezal", 3,  0xffffffff, 5,  6);
   TESTINST6("bgezal", 4,  0xfffffffe, 6,  8);
   TESTINST6("bgezal", 5,  0xffffffff, 7,  8);
   TESTINST6("bgezal", 6,  0x5,        8,  9);
   TESTINST6("bgezal", 7,  -3,         9,  10);
   TESTINST6("bgezal", 8,  125,        10, 11);
   TESTINST6("bgezal", 9,  0x80000000, 11, 12);
   TESTINST6("bgezal", 10, 0xffffffff, 12, 13);
   TESTINST6("bgezal", 11, 0x256,      13, 14);
   TESTINST6("bgezal", 12, 0x55,       14, 15);
   TESTINST6("bgezal", 13, 0xfff,      16, 17);
   TESTINST6("bgezal", 14, -1,         2,  25);
   TESTINST6("bgezal", 15, -1,         25, 24);

   printf("--- BLTZAL ---  if RSval < 0 then " \
          "out = RDval + 6 else out = RDval + 5\n");
   TESTINST6("bltzal", 0,  0,          2,  3);
   TESTINST6("bltzal", 1,  1,          3,  4);
   TESTINST6("bltzal", 2,  0xffffffff, 4,  5);
   TESTINST6("bltzal", 3,  0xffffffff, 5,  6);
   TESTINST6("bltzal", 4,  0xfffffffe, 6,  8);
   TESTINST6("bltzal", 5,  0xffffffff, 7,  8);
   TESTINST6("bltzal", 6,  0x5,        8,  9);
   TESTINST6("bltzal", 7,  -3,         9,  10);
   TESTINST6("bltzal", 8,  125,        10, 11);
   TESTINST6("bltzal", 9,  0x80000000, 11, 12);
   TESTINST6("bltzal", 10, 0xffffffff, 12, 13);
   TESTINST6("bltzal", 11, 0x256,      13, 14);
   TESTINST6("bltzal", 12, 0x55,       14, 15);
   TESTINST6("bltzal", 13, 0xfff,      16, 17);
   TESTINST6("bltzal", 14, -1,         2,  25);
   TESTINST6("bltzal", 15, -1,         25, 24);
#endif
   printf("--- BNEZ ---  if RSval != 0 then " \
          "out = RDval + 1 else out = RDval + 6\n");
   TESTINST5("bnez", 0,  0,          2,  3);
   TESTINST5("bnez", 1,  1,          3,  4);
   TESTINST5("bnez", 2,  0xffffffff, 4,  5);
   TESTINST5("bnez", 3,  0xffffffff, 5,  6);
   TESTINST5("bnez", 4,  0xfffffffe, 6,  8);
   TESTINST5("bnez", 5,  0xffffffff, 7,  8);
   TESTINST5("bnez", 6,  0x5,        8,  9);
   TESTINST5("bnez", 7,  -3,         9,  10);
   TESTINST5("bnez", 8,  125,        10, 11);
   TESTINST5("bnez", 9,  0x80000000, 11, 12);
   TESTINST5("bnez", 10, 0xffffffff, 12, 13);
   TESTINST5("bnez", 11, 0x256,      13, 14);
   TESTINST5("bnez", 12, 0x55,       14, 15);
   TESTINST5("bnez", 13, 0xfff,      16, 17);
   TESTINST5("bnez", 14, -1,         2,  25);
   TESTINST5("bnez", 15, -1,         25, 24);
#if (__mips_isa_rev < 6)
   printf("--- BEQL ---  if RSval == RTval then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST4l("beql", 0,  0,          1,          2,  3,  4);
   TESTINST4l("beql", 1,  1,          1,          3,  4,  5);
   TESTINST4l("beql", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TESTINST4l("beql", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TESTINST4l("beql", 4,  0xfffffffe, 0xffffffff, 6,  8,  9);
   TESTINST4l("beql", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TESTINST4l("beql", 6,  0x5,        0x5,        8,  9,  10);
   TESTINST4l("beql", 7,  -3,         -4,         9,  10, 11);
   TESTINST4l("beql", 8,  125,        125,        10, 11, 12);
   TESTINST4l("beql", 9,  0x80000000, 0x80000000, 11, 12, 13);
   TESTINST4l("beql", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4l("beql", 11, 0x256,      0x256,      13, 14, 15);
   TESTINST4l("beql", 12, 0x55,       0x55,       14, 15, 16);
   TESTINST4l("beql", 13, 0xfff,      0xdd,       16, 17, 18);
   TESTINST4l("beql", 14, -1,         0x5,        2,  25, 24);
   TESTINST4l("beql", 15, -1,         -1,         25, 24, 7);

   printf("--- BGEZALL ---  if RSval >= 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("bgezall", 0,  0,          2,  3);
   TESTINST5l("bgezall", 1,  1,          3,  4);
   TESTINST5l("bgezall", 2,  0xffffffff, 4,  5);
   TESTINST5l("bgezall", 3,  0xffffffff, 5,  6);
   TESTINST5l("bgezall", 4,  0xfffffffe, 6,  8);
   TESTINST5l("bgezall", 5,  0xffffffff, 7,  8);
   TESTINST5l("bgezall", 6,  0x5,        8,  9);
   TESTINST5l("bgezall", 7,  -3,         9,  10);
   TESTINST5l("bgezall", 8,  125,        10, 11);
   TESTINST5l("bgezall", 9,  0x80000000, 11, 12);
   TESTINST5l("bgezall", 10, 0xffffffff, 12, 13);
   TESTINST5l("bgezall", 11, 0x256,      13, 14);
   TESTINST5l("bgezall", 12, 0x55,       14, 15);
   TESTINST5l("bgezall", 13, 0xfff,      16, 17);
   TESTINST5l("bgezall", 14, -1,         2,  25);
   TESTINST5l("bgezall", 15, -1,         25, 24);

   printf("--- BLTZALL ---  if RSval < 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("bltzall", 0,  0,          2,  3);
   TESTINST5l("bltzall", 1,  1,          3,  4);
   TESTINST5l("bltzall", 2,  0xffffffff, 4,  5);
   TESTINST5l("bltzall", 3,  0xffffffff, 5,  6);
   TESTINST5l("bltzall", 4,  0xfffffffe, 6,  8);
   TESTINST5l("bltzall", 5,  0xffffffff, 7,  8);
   TESTINST5l("bltzall", 6,  0x5,        8,  9);
   TESTINST5l("bltzall", 7,  -3,         9,  10);
   TESTINST5l("bltzall", 8,  125,        10, 11);
   TESTINST5l("bltzall", 9,  0x80000000, 11, 12);
   TESTINST5l("bltzall", 10, 0xffffffff, 12, 13);
   TESTINST5l("bltzall", 11, 0x256,      13, 14);
   TESTINST5l("bltzall", 12, 0x55,       14, 15);
   TESTINST5l("bltzall", 13, 0xfff,      16, 17);
   TESTINST5l("bltzall", 14, -1,         2,  25);
   TESTINST5l("bltzall", 15, -1,         25, 24);

   printf("--- BGEZL ---  if RSval >= 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("bgezl", 0,  0,          2,  3);
   TESTINST5l("bgezl", 1,  1,          3,  4);
   TESTINST5l("bgezl", 2,  0xffffffff, 4,  5);
   TESTINST5l("bgezl", 3,  0xffffffff, 5,  6);
   TESTINST5l("bgezl", 4,  0xfffffffe, 6,  8);
   TESTINST5l("bgezl", 5,  0xffffffff, 7,  8);
   TESTINST5l("bgezl", 6,  0x5,        8,  9);
   TESTINST5l("bgezl", 7,  -3,         9,  10);
   TESTINST5l("bgezl", 8,  125,        10, 11);
   TESTINST5l("bgezl", 9,  0x80000000, 11, 12);
   TESTINST5l("bgezl", 10, 0xffffffff, 12, 13);
   TESTINST5l("bgezl", 11, 0x256,      13, 14);
   TESTINST5l("bgezl", 12, 0x55,       14, 15);
   TESTINST5l("bgezl", 13, 0xfff,      16, 17);
   TESTINST5l("bgezl", 14, -1,         2,  25);
   TESTINST5l("bgezl", 15, -1,         25, 24);

   printf("--- BGTZL ---  if RSval > 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("bgtzl", 0,  0,          2,  3);
   TESTINST5l("bgtzl", 1,  1,          3,  4);
   TESTINST5l("bgtzl", 2,  0xffffffff, 4,  5);
   TESTINST5l("bgtzl", 3,  0xffffffff, 5,  6);
   TESTINST5l("bgtzl", 4,  0xfffffffe, 6,  8);
   TESTINST5l("bgtzl", 5,  0xffffffff, 7,  8);
   TESTINST5l("bgtzl", 6,  0x5,        8,  9);
   TESTINST5l("bgtzl", 7,  -3,         9,  10);
   TESTINST5l("bgtzl", 8,  125,        10, 11);
   TESTINST5l("bgtzl", 9,  0x80000000, 11, 12);
   TESTINST5l("bgtzl", 10, 0xffffffff, 12, 13);
   TESTINST5l("bgtzl", 11, 0x256,      13, 14);
   TESTINST5l("bgtzl", 12, 0x55,       14, 15);
   TESTINST5l("bgtzl", 13, 0xfff,      16, 17);
   TESTINST5l("bgtzl", 14, -1,         2,  25);
   TESTINST5l("bgtzl", 15, -1,         25, 24);

   printf("--- BLEZL ---  if RSval <= 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("blezl", 0,  0,          2,  3);
   TESTINST5l("blezl", 1,  1,          3,  4);
   TESTINST5l("blezl", 2,  0xffffffff, 4,  5);
   TESTINST5l("blezl", 3,  0xffffffff, 5,  6);
   TESTINST5l("blezl", 4,  0xfffffffe, 6,  8);
   TESTINST5l("blezl", 5,  0xffffffff, 7,  8);
   TESTINST5l("blezl", 6,  0x5,        8,  9);
   TESTINST5l("blezl", 7,  -3,         9,  10);
   TESTINST5l("blezl", 8,  125,        10, 11);
   TESTINST5l("blezl", 9,  0x80000000, 11, 12);
   TESTINST5l("blezl", 10, 0xffffffff, 12, 13);
   TESTINST5l("blezl", 11, 0x256,      13, 14);
   TESTINST5l("blezl", 12, 0x55,       14, 15);
   TESTINST5l("blezl", 13, 0xfff,      16, 17);
   TESTINST5l("blezl", 14, -1,         2,  25);
   TESTINST5l("blezl", 15, -1,         25, 24);

   printf("--- BGEZALL ---  if RSval >= 0 then " \
          "out = RDval + 9 else out = RDval + 5\n");
   TESTINST6l("bgezall", 0,  0,          2,  3);
   TESTINST6l("bgezall", 1,  1,          3,  4);
   TESTINST6l("bgezall", 2,  0xffffffff, 4,  5);
   TESTINST6l("bgezall", 3,  0xffffffff, 5,  6);
   TESTINST6l("bgezall", 4,  0xfffffffe, 6,  8);
   TESTINST6l("bgezall", 5,  0xffffffff, 7,  8);
   TESTINST6l("bgezall", 6,  0x5,        8,  9);
   TESTINST6l("bgezall", 7,  -3,         9,  10);
   TESTINST6l("bgezall", 8,  125,        10, 11);
   TESTINST6l("bgezall", 9,  0x80000000, 11, 12);
   TESTINST6l("bgezall", 10, 0xffffffff, 12, 13);
   TESTINST6l("bgezall", 11, 0x256,      13, 14);
   TESTINST6l("bgezall", 12, 0x55,       14, 15);
   TESTINST6l("bgezall", 13, 0xfff,      16, 17);
   TESTINST6l("bgezall", 14, -1,         2,  25);
   TESTINST6l("bgezall", 15, -1,         25, 24);

   printf("--- BLTZL ---  if RSval < 0 then " \
          "out = RDval + 4 else out = RDval + 6\n");
   TESTINST5l("bltzl", 0,  0,          2,  3);
   TESTINST5l("bltzl", 1,  1,          3,  4);
   TESTINST5l("bltzl", 2,  0xffffffff, 4,  5);
   TESTINST5l("bltzl", 3,  0xffffffff, 5,  6);
   TESTINST5l("bltzl", 4,  0xfffffffe, 6,  8);
   TESTINST5l("bltzl", 5,  0xffffffff, 7,  8);
   TESTINST5l("bltzl", 6,  0x5,        8,  9);
   TESTINST5l("bltzl", 7,  -3,         9,  10);
   TESTINST5l("bltzl", 8,  125,        10, 11);
   TESTINST5l("bltzl", 9,  0x80000000, 11, 12);
   TESTINST5l("bltzl", 10, 0xffffffff, 12, 13);
   TESTINST5l("bltzl", 11, 0x256,      13, 14);
   TESTINST5l("bltzl", 12, 0x55,       14, 15);
   TESTINST5l("bltzl", 13, 0xfff,      16, 17);
   TESTINST5l("bltzl", 14, -1,         2,  25);
   TESTINST5l("bltzl", 15, -1,         25, 24);

   printf("--- BNEL ---  if RSval != RTval then " \
          "out = RDval + 4 else out = RDval + 5\n");
   TESTINST4l("bnel", 0,  0,          1,          2,  3,  4);
   TESTINST4l("bnel", 1,  1,          1,          3,  4,  5);
   TESTINST4l("bnel", 2,  0xffffffff, 0xffffffff, 4,  5,  6);
   TESTINST4l("bnel", 3,  0xffffffff, 0xfffffffe, 5,  6,  7);
   TESTINST4l("bnel", 4,  0xfffffffe, 0xffffffff, 6,  8,  9);
   TESTINST4l("bnel", 5,  0xffffffff, 0xffffffff, 7,  8,  9);
   TESTINST4l("bnel", 6,  0x5,        0x5,        8,  9,  10);
   TESTINST4l("bnel", 7,  -3,         -4,         9,  10, 11);
   TESTINST4l("bnel", 8,  125,        125,        10, 11, 12);
   TESTINST4l("bnel", 9,  0x80000000, 0x80000000, 11, 12, 13);
   TESTINST4l("bnel", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4l("bnel", 11, 0x256,      0x256,      13, 14, 15);
   TESTINST4l("bnel", 12, 0x55,       0x55,       14, 15, 16);
   TESTINST4l("bnel", 13, 0xfff,      0xdd,       16, 17, 18);
   TESTINST4l("bnel", 14, -1,         0x5,        2,  25, 24);
   TESTINST4l("bnel", 15, -1,         -1,         25, 24, 7);
#endif
   printf("j, jal, jr\n");
   TESTINST3j(0,  2);
   TESTINST3j(1,  3);
   TESTINST3j(2,  4);
   TESTINST3j(3,  5);
   TESTINST3j(4,  6);
   TESTINST3j(5,  7);
   TESTINST3j(6,  4);
   TESTINST3j(7,  9);
   TESTINST3j(8,  10);
   TESTINST3j(9,  11);
   TESTINST3j(10, 12);
   TESTINST3j(11, 13);
   TESTINST3j(12, 14);
   TESTINST3j(13, 15);
   TESTINST3j(14, 16);
   TESTINST3j(15, 17);
   TESTINST3j(16, 18);
   TESTINST3j(17, 19);
   TESTINST3j(18, 20);
   TESTINST3j(19, 21);
   TESTINST3j(20, 22);
   TESTINST3j(21, 23);
   TESTINST3j(22, 24);
   TESTINST3j(23, 24);

   printf("j, jalr, jr\n");
   TESTINST3ja(0,  2);
   TESTINST3ja(1,  3);
   TESTINST3ja(2,  4);
   TESTINST3ja(3,  5);
   TESTINST3ja(4,  6);
   TESTINST3ja(5,  7);
   TESTINST3ja(6,  4);
   TESTINST3ja(7,  7);
   TESTINST3ja(8,  10);
   TESTINST3ja(9,  11);
   TESTINST3ja(11, 13);
   TESTINST3ja(12, 14);
   TESTINST3ja(13, 15);
   TESTINST3ja(14, 16);
   TESTINST3ja(15, 17);
   TESTINST3ja(16, 18);
   TESTINST3ja(17, 19);
   TESTINST3ja(18, 20);
   TESTINST3ja(19, 21);
   TESTINST3ja(20, 22);
   TESTINST3ja(21, 23);
   TESTINST3ja(22, 24);
   TESTINST3ja(23, 24);

   return 0;
}
