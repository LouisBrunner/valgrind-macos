#define TEST1(instruction, RSval, RTval, RD, RS, RT)  \
{                                                     \
   unsigned long long out;                            \
   __asm__ __volatile__(                              \
      "move $"#RS", %1"     "\n\t"                    \
      "move $"#RT", %2"     "\n\t"                    \
      "move $"#RD", $zero"  "\n\t"                    \
      instruction           "\n\t"                    \
      "move %0,     $"#RD   "\n\t"                    \
      : "=r" (out)                                    \
      : "r" (RSval), "r" (RTval)                      \
      : #RD, #RS, #RT                                 \
   );                                                 \
   printf("%s :: rd 0x%llx, rs 0x%llx, rt 0x%llx\n",  \
          instruction, out, (long long) RSval,        \
          (long long) RTval);                         \
}

#define TEST2(instruction, RSval, imm, RT, RS)         \
{                                                      \
   unsigned long long out;                             \
   __asm__ __volatile__(                               \
      "move $"#RS", %1"     "\n\t"                     \
      "move $"#RT", $zero"  "\n\t"                     \
      instruction           "\n\t"                     \
      "move %0,     $"#RT   "\n\t"                     \
      : "=r" (out)                                     \
      : "r" (RSval)                                    \
      : #RT, #RS                                       \
   );                                                  \
   printf("%s :: rt 0x%llx, rs 0x%llx, imm 0x%04x\n",  \
          instruction, out, (long long) RSval, imm);   \
}

#define TEST3(instruction, RSval, RD, RS)        \
{                                                \
   unsigned long long out;                       \
   __asm__ __volatile__(                         \
      "move $"#RS", %1"     "\n\t"               \
      "move $"#RD", $zero"  "\n\t"               \
      instruction           "\n\t"               \
      "move %0,     $"#RD   "\n\t"               \
      : "=r" (out)                               \
      : "r" (RSval)                              \
      : #RD, #RS                                 \
   );                                            \
   printf("%s :: rd 0x%llx, rs 0x%llx\n",        \
          instruction, out, (long long) RSval);  \
}

#define TEST4(instruction, RSval, RTval, RS, RT)                       \
{                                                                      \
   unsigned long long HI;                                              \
   unsigned long long LO;                                              \
   __asm__ __volatile__(                                               \
      "move $"#RS", %2"  "\n\t"                                        \
      "move $"#RT", %3"  "\n\t"                                        \
      "mthi $zero"       "\n\t"                                        \
      "mtlo $zero"       "\n\t"                                        \
      instruction        "\n\t"                                        \
      "mfhi %0"          "\n\t"                                        \
      "mflo %1"          "\n\t"                                        \
      : "=r" (HI), "=r" (LO)                                           \
      : "r" (RSval), "r"(RTval)                                        \
      : #RS, #RT                                                       \
   );                                                                  \
   printf("%s :: rs 0x%llx, rt 0x%llx, HI 0x%llx, LO 0x%llx\n",        \
          instruction, (long long) RSval, (long long) RTval, HI, LO);  \
}

#define TEST5(instruction, RSval, RTval, RS, RT)                       \
{                                                                      \
   unsigned long long HI;                                              \
   unsigned long long LO;                                              \
   __asm__ __volatile__(                                               \
      "move $"#RS", %2"  "\n\t"                                        \
      "move $"#RT", %3"  "\n\t"                                        \
      "mthi $"#RS        "\n\t"                                        \
      "mtlo $"#RT        "\n\t"                                        \
      instruction        "\n\t"                                        \
      "mfhi %0"          "\n\t"                                        \
      "mflo %1"          "\n\t"                                        \
      : "=r" (HI), "=r" (LO)                                           \
      : "r" (RSval), "r"(RTval)                                        \
      : #RS, #RT                                                       \
   );                                                                  \
   printf("%s :: rs 0x%llx, rt 0x%llx, HI 0x%llx, LO 0x%llx\n",        \
          instruction, (long long) RSval, (long long) RTval, HI, LO);  \
}

#define TEST6(instruction, imm, RT)         \
{                                           \
   unsigned long long out;                  \
   __asm__ __volatile__(                    \
      "move $"#RT", $zero"  "\n\t"          \
      instruction           "\n\t"          \
      "move %0, $"#RT       "\n\t"          \
      : "=r" (out) :                        \
      : #RT                                 \
   );                                       \
   printf("%s :: rt 0x%llx, imm 0x%04x\n",  \
          instruction, out, imm);           \
}
