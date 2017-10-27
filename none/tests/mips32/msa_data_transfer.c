#include <stdint.h>
#include <stdio.h>

char data[1040];
char* base;
unsigned long long out[2];

#define TEST_LD(instruction, offset, WD, RS)                          \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $"#RS", %0\n\t"                                           \
      instruction" $"#WD", "#offset"($"#RS")\n\t"                     \
      "move $t0, %1\n\t"                                              \
      "st.d $"#WD", 0($t0)\n\t"                                       \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (out)                                         \
      : "t0", #RS, "memory"                                           \
   );                                                                 \
   printf(instruction" $"#WD", %d($"#RS") out: %016llx%016llx\n",     \
            offset, out[1], out[0]);                                  \
}

#define TEST_ST(instruction, offset, WD, RS)                          \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $t0, %0\n\t"                                              \
      "ld.b $"#WD", "#offset"($t0)\n\t"                               \
      "move $"#RS", %1\n\t"                                           \
      instruction" $"#WD", "#offset"($"#RS")\n\t"                     \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (((char*)out) - offset)                       \
      : "t0", #RS, "memory"                                           \
   );                                                                 \
   printf(instruction" $"#WD", %d($"#RS") out: %016llx%016llx\n",     \
            offset, out[1], out[0]);                                  \
}

#define TEST_LDI(instruction, imm, WD)                                \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      instruction" $"#WD", "#imm"\n\t"                                \
      "move $t0, %0\n\t"                                              \
      "st.d $"#WD", 0($t0)\n\t"                                       \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (out)                                                     \
      : "t0", "memory"                                                \
   );                                                                 \
   printf(instruction" $"#WD", "#imm" out: %016llx%016llx\n",         \
            out[1], out[0]);                                          \
}

#define TEST_MOVE(instruction, offset, WD, WS)                        \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $t0, %0\n\t"                                              \
      "ld.w $"#WS", "#offset"($t0)\n\t"                               \
      instruction" $"#WD", $"#WS"\n\t"                                \
      "move $t0, %1\n\t"                                              \
      "st.d $"#WD", 0($t0)\n\t"                                       \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (out)                                         \
      : "t0", "memory"                                                \
   );                                                                 \
   printf(instruction" $"#WD", $"#WS" out: %016llx%016llx\n",         \
            out[1], out[0]);                                          \
}

#define TEST_INSERT(instruction, offset1, offset2, WD, pos, RS)       \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $t0, %0\n\t"                                              \
      "ld.b $"#WD", "#offset1"($t0)\n\t"                              \
      "lw $"#RS", "#offset2"($t0)\n\t"                                \
      instruction" $"#WD"["#pos"], $"#RS"\n\t"                        \
      "move $t0, %1\n\t"                                              \
      "st.d $"#WD", 0($t0)\n\t"                                       \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (out)                                         \
      : "t0", #RS, "memory"                                           \
   );                                                                 \
   printf(instruction" $"#WD"[%u], $"#RS" out: %016llx%016llx\n",     \
            pos, out[1], out[0]);                                     \
}

#define TEST_FILL(instruction, offset, WD, RS)                        \
{                                                                     \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $t0, %0\n\t"                                              \
      "lw $"#RS", "#offset"($t0)\n\t"                                 \
      instruction" $"#WD", $"#RS"\n\t"                                \
      "move $t0, %1\n\t"                                              \
      "st.d $"#WD", 0($t0)\n\t"                                       \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (out)                                         \
      : "t0", #RS, "memory"                                           \
   );                                                                 \
   printf(instruction" $"#WD", $"#RS" out: %016llx%016llx\n",         \
            out[1], out[0]);                                          \
}

#define TEST_COPY(instruction, offset, WS, pos, RD)                   \
{                                                                     \
   unsigned int outi;                                                 \
   __asm__ volatile (                                                 \
      ".set push;\n\t"                                                \
      ".set noreorder\n\t"                                            \
      "move $t0, %0\n\t"                                              \
      "ld.b $"#WS", "#offset"($t0)\n\t"                               \
      instruction" $"#RD", $"#WS"["#pos"]\n\t"                        \
      "move $t5, %1\n\t"                                              \
      "sw $"#RD", 0($t5)\n\t"                                         \
      ".set pop;\n\t"                                                 \
      :                                                               \
      : "r" (base), "r" (&outi)                                       \
      : "t0", #RD, "memory"                                           \
   );                                                                 \
   printf(instruction" $"#RD", $"#WS"[%u] out: %x\n", pos, outi);     \
}

int main(int argc, char **argv) {
#if defined(__mips_msa)
   int i;
   base = data + 512;

   for (i = 0; i < sizeof(data); i++)
      data[i] = (char)i + 0xa0;

   TEST_LD("ld.d", -352, w0,  t3);
   TEST_LD("ld.d", -200, w1,  t0);
   TEST_LD("ld.d",  408, w2,  t1);
   TEST_LD("ld.d",  -40, w3,  t2);
   TEST_LD("ld.d", -200, w4,  t3);
   TEST_LD("ld.d", -472, w5,  t0);
   TEST_LD("ld.d",   48, w6,  t1);
   TEST_LD("ld.d", -272, w7,  t2);
   TEST_LD("ld.d", -184, w8,  t3);
   TEST_LD("ld.d",  448, w9,  t0);
   TEST_LD("ld.d",  -64, w10, t1);
   TEST_LD("ld.d",   -8, w11, t2);
   TEST_LD("ld.d",  368, w12, t3);
   TEST_LD("ld.d", -368, w13, t0);
   TEST_LD("ld.d",  216, w14, t1);
   TEST_LD("ld.d",   80, w15, t2);
   TEST_LD("ld.d",  296, w16, t3);
   TEST_LD("ld.d",  328, w17, t0);
   TEST_LD("ld.d",  416, w18, t1);
   TEST_LD("ld.d",  248, w19, t2);
   TEST_LD("ld.d", -424, w20, t3);
   TEST_LD("ld.d",  384, w21, t0);
   TEST_LD("ld.d",  384, w22, t1);
   TEST_LD("ld.d",   64, w23, t2);
   TEST_LD("ld.d", -512, w24, t3);
   TEST_LD("ld.d", -200, w25, t0);
   TEST_LD("ld.d",  184, w26, t1);
   TEST_LD("ld.d",  216, w27, t2);
   TEST_LD("ld.d", -376, w28, t3);
   TEST_LD("ld.d",   64, w29, t0);
   TEST_LD("ld.d", -368, w30, t1);
   TEST_LD("ld.d", -216, w31, t2);
   TEST_LD("ld.w",  380, w0,  t2);
   TEST_LD("ld.w", -468, w1,  t3);
   TEST_LD("ld.w",  260, w2,  t0);
   TEST_LD("ld.w", -324, w3,  t1);
   TEST_LD("ld.w", -428, w4,  t2);
   TEST_LD("ld.w", -200, w5,  t3);
   TEST_LD("ld.w",  -80, w6,  t0);
   TEST_LD("ld.w",  -96, w7,  t1);
   TEST_LD("ld.w", -256, w8,  t2);
   TEST_LD("ld.w",  376, w9,  t3);
   TEST_LD("ld.w",  416, w10, t0);
   TEST_LD("ld.w", -396, w11, t1);
   TEST_LD("ld.w", -500, w12, t2);
   TEST_LD("ld.w",  128, w13, t3);
   TEST_LD("ld.w",  200, w14, t0);
   TEST_LD("ld.w",  316, w15, t1);
   TEST_LD("ld.w",  -52, w16, t2);
   TEST_LD("ld.w",  104, w17, t3);
   TEST_LD("ld.w",   52, w18, t0);
   TEST_LD("ld.w",   40, w19, t1);
   TEST_LD("ld.w",  -20, w20, t2);
   TEST_LD("ld.w",  -72, w21, t3);
   TEST_LD("ld.w", -408, w22, t0);
   TEST_LD("ld.w",  -16, w23, t1);
   TEST_LD("ld.w",  244, w24, t2);
   TEST_LD("ld.w",  292, w25, t3);
   TEST_LD("ld.w", -308, w26, t0);
   TEST_LD("ld.w",  380, w27, t1);
   TEST_LD("ld.w", -152, w28, t2);
   TEST_LD("ld.w", -164, w29, t3);
   TEST_LD("ld.w", -348, w30, t0);
   TEST_LD("ld.w", -280, w31, t1);
   TEST_LD("ld.h", -118, w0,  t1);
   TEST_LD("ld.h",  428, w1,  t2);
   TEST_LD("ld.h",  -90, w2,  t3);
   TEST_LD("ld.h",  -32, w3,  t0);
   TEST_LD("ld.h", -282, w4,  t1);
   TEST_LD("ld.h",  346, w5,  t2);
   TEST_LD("ld.h",  386, w6,  t3);
   TEST_LD("ld.h",  -26, w7,  t0);
   TEST_LD("ld.h",  212, w8,  t1);
   TEST_LD("ld.h",  292, w9,  t2);
   TEST_LD("ld.h",   94, w10, t3);
   TEST_LD("ld.h",  226, w11, t0);
   TEST_LD("ld.h",  -92, w12, t1);
   TEST_LD("ld.h", -216, w13, t2);
   TEST_LD("ld.h",   32, w14, t3);
   TEST_LD("ld.h",  368, w15, t0);
   TEST_LD("ld.h",  404, w16, t1);
   TEST_LD("ld.h", -426, w17, t2);
   TEST_LD("ld.h", -102, w18, t3);
   TEST_LD("ld.h", -128, w19, t0);
   TEST_LD("ld.h",   16, w20, t1);
   TEST_LD("ld.h",    4, w21, t2);
   TEST_LD("ld.h",  370, w22, t3);
   TEST_LD("ld.h", -252, w23, t0);
   TEST_LD("ld.h", -216, w24, t1);
   TEST_LD("ld.h", -448, w25, t2);
   TEST_LD("ld.h", -382, w26, t3);
   TEST_LD("ld.h",  148, w27, t0);
   TEST_LD("ld.h",  -98, w28, t1);
   TEST_LD("ld.h", -216, w29, t2);
   TEST_LD("ld.h",  382, w30, t3);
   TEST_LD("ld.h",  298, w31, t0);
   TEST_LD("ld.b", -298, w0,  t0);
   TEST_LD("ld.b", -217, w1,  t1);
   TEST_LD("ld.b", -245, w2,  t2);
   TEST_LD("ld.b",  -68, w3,  t3);
   TEST_LD("ld.b", -383, w4,  t0);
   TEST_LD("ld.b", -369, w5,  t1);
   TEST_LD("ld.b",  420, w6,  t2);
   TEST_LD("ld.b",  341, w7,  t3);
   TEST_LD("ld.b",  435, w8,  t0);
   TEST_LD("ld.b",    2, w9,  t1);
   TEST_LD("ld.b",   55, w10, t2);
   TEST_LD("ld.b", -168, w11, t3);
   TEST_LD("ld.b",  299, w12, t0);
   TEST_LD("ld.b", -425, w13, t1);
   TEST_LD("ld.b", -312, w14, t2);
   TEST_LD("ld.b",  191, w15, t3);
   TEST_LD("ld.b", -337, w16, t0);
   TEST_LD("ld.b",   98, w17, t1);
   TEST_LD("ld.b", -449, w18, t2);
   TEST_LD("ld.b",  191, w19, t3);
   TEST_LD("ld.b", -410, w20, t0);
   TEST_LD("ld.b",  435, w21, t1);
   TEST_LD("ld.b",  452, w22, t2);
   TEST_LD("ld.b", -112, w23, t3);
   TEST_LD("ld.b",  500, w24, t0);
   TEST_LD("ld.b", -442, w25, t1);
   TEST_LD("ld.b", -475, w26, t2);
   TEST_LD("ld.b", -110, w27, t3);
   TEST_LD("ld.b", -145, w28, t0);
   TEST_LD("ld.b",  420, w29, t1);
   TEST_LD("ld.b", -324, w30, t2);
   TEST_LD("ld.b",   69, w31, t3);
   TEST_ST("st.d", -184, w0,  t3);
   TEST_ST("st.d",    8, w1,  t0);
   TEST_ST("st.d",   88, w2,  t1);
   TEST_ST("st.d",  160, w3,  t2);
   TEST_ST("st.d",  104, w4,  t3);
   TEST_ST("st.d",  344, w5,  t0);
   TEST_ST("st.d", -144, w6,  t1);
   TEST_ST("st.d", -448, w7,  t2);
   TEST_ST("st.d", -336, w8,  t3);
   TEST_ST("st.d",  496, w9,  t0);
   TEST_ST("st.d", -248, w10, t1);
   TEST_ST("st.d", -464, w11, t2);
   TEST_ST("st.d", -256, w12, t3);
   TEST_ST("st.d", -144, w13, t0);
   TEST_ST("st.d", -480, w14, t1);
   TEST_ST("st.d",  -48, w15, t2);
   TEST_ST("st.d",  288, w16, t3);
   TEST_ST("st.d",    0, w17, t0);
   TEST_ST("st.d",  472, w18, t1);
   TEST_ST("st.d", -160, w19, t2);
   TEST_ST("st.d",  424, w20, t3);
   TEST_ST("st.d", -184, w21, t0);
   TEST_ST("st.d", -488, w22, t1);
   TEST_ST("st.d", -192, w23, t2);
   TEST_ST("st.d", -480, w24, t3);
   TEST_ST("st.d", -176, w25, t0);
   TEST_ST("st.d",  456, w26, t1);
   TEST_ST("st.d", -216, w27, t2);
   TEST_ST("st.d", -152, w28, t3);
   TEST_ST("st.d", -120, w29, t0);
   TEST_ST("st.d",    0, w30, t1);
   TEST_ST("st.d",  184, w31, t2);
   TEST_ST("st.w",  408, w0,  t2);
   TEST_ST("st.w", -420, w1,  t3);
   TEST_ST("st.w", -164, w2,  t0);
   TEST_ST("st.w",    8, w3,  t1);
   TEST_ST("st.w",  444, w4,  t2);
   TEST_ST("st.w",  212, w5,  t3);
   TEST_ST("st.w",   76, w6,  t0);
   TEST_ST("st.w", -400, w7,  t1);
   TEST_ST("st.w",  204, w8,  t2);
   TEST_ST("st.w",  348, w9,  t3);
   TEST_ST("st.w", -348, w10, t0);
   TEST_ST("st.w",  464, w11, t1);
   TEST_ST("st.w", -304, w12, t2);
   TEST_ST("st.w", -312, w13, t3);
   TEST_ST("st.w",  -88, w14, t0);
   TEST_ST("st.w",  500, w15, t1);
   TEST_ST("st.w",  204, w16, t2);
   TEST_ST("st.w", -124, w17, t3);
   TEST_ST("st.w", -168, w18, t0);
   TEST_ST("st.w",  116, w19, t1);
   TEST_ST("st.w",  212, w20, t2);
   TEST_ST("st.w", -144, w21, t3);
   TEST_ST("st.w",  444, w22, t0);
   TEST_ST("st.w",  244, w23, t1);
   TEST_ST("st.w",  196, w24, t2);
   TEST_ST("st.w",  392, w25, t3);
   TEST_ST("st.w", -480, w26, t0);
   TEST_ST("st.w", -468, w27, t1);
   TEST_ST("st.w", -232, w28, t2);
   TEST_ST("st.w",   36, w29, t3);
   TEST_ST("st.w",  228, w30, t0);
   TEST_ST("st.w", -332, w31, t1);
   TEST_ST("st.h",  134, w0,  t1);
   TEST_ST("st.h", -444, w1,  t2);
   TEST_ST("st.h",  190, w2,  t3);
   TEST_ST("st.h",   68, w3,  t0);
   TEST_ST("st.h",  282, w4,  t1);
   TEST_ST("st.h", -244, w5,  t2);
   TEST_ST("st.h",  184, w6,  t3);
   TEST_ST("st.h",  -26, w7,  t0);
   TEST_ST("st.h", -408, w8,  t1);
   TEST_ST("st.h",  348, w9,  t2);
   TEST_ST("st.h",  -70, w10, t3);
   TEST_ST("st.h", -198, w11, t0);
   TEST_ST("st.h", -474, w12, t1);
   TEST_ST("st.h",  354, w13, t2);
   TEST_ST("st.h", -206, w14, t3);
   TEST_ST("st.h",  242, w15, t0);
   TEST_ST("st.h", -278, w16, t1);
   TEST_ST("st.h",  140, w17, t2);
   TEST_ST("st.h", -152, w18, t3);
   TEST_ST("st.h",  448, w19, t0);
   TEST_ST("st.h",  510, w20, t1);
   TEST_ST("st.h", -218, w21, t2);
   TEST_ST("st.h",  182, w22, t3);
   TEST_ST("st.h",  196, w23, t0);
   TEST_ST("st.h", -334, w24, t1);
   TEST_ST("st.h",  218, w25, t2);
   TEST_ST("st.h",  242, w26, t3);
   TEST_ST("st.h",  -52, w27, t0);
   TEST_ST("st.h", -256, w28, t1);
   TEST_ST("st.h",  -40, w29, t2);
   TEST_ST("st.h",  130, w30, t3);
   TEST_ST("st.h",  390, w31, t0);
   TEST_ST("st.b",   30, w0,  t0);
   TEST_ST("st.b", -192, w1,  t1);
   TEST_ST("st.b",  -52, w2,  t2);
   TEST_ST("st.b", -198, w3,  t3);
   TEST_ST("st.b",   77, w4,  t0);
   TEST_ST("st.b", -380, w5,  t1);
   TEST_ST("st.b",  289, w6,  t2);
   TEST_ST("st.b",  182, w7,  t3);
   TEST_ST("st.b",  480, w8,  t0);
   TEST_ST("st.b", -292, w9,  t1);
   TEST_ST("st.b",  497, w10, t2);
   TEST_ST("st.b", -505, w11, t3);
   TEST_ST("st.b", -449, w12, t0);
   TEST_ST("st.b", -220, w13, t1);
   TEST_ST("st.b",  249, w14, t2);
   TEST_ST("st.b", -215, w15, t3);
   TEST_ST("st.b",  432, w16, t0);
   TEST_ST("st.b", -413, w17, t1);
   TEST_ST("st.b", -279, w18, t2);
   TEST_ST("st.b",  431, w19, t3);
   TEST_ST("st.b", -118, w20, t0);
   TEST_ST("st.b",  416, w21, t1);
   TEST_ST("st.b",  115, w22, t2);
   TEST_ST("st.b",   60, w23, t3);
   TEST_ST("st.b",  122, w24, t0);
   TEST_ST("st.b", -155, w25, t1);
   TEST_ST("st.b", -504, w26, t2);
   TEST_ST("st.b",  379, w27, t3);
   TEST_ST("st.b",  318, w28, t0);
   TEST_ST("st.b",  139, w29, t1);
   TEST_ST("st.b",  258, w30, t2);
   TEST_ST("st.b", -163, w31, t3);
   TEST_LDI("ldi.d", -424, w0);
   TEST_LDI("ldi.d",  -80, w1);
   TEST_LDI("ldi.d",  456, w2);
   TEST_LDI("ldi.d", -352, w3);
   TEST_LDI("ldi.d",  448, w4);
   TEST_LDI("ldi.d",  -24, w5);
   TEST_LDI("ldi.d", -344, w6);
   TEST_LDI("ldi.d",   56, w7);
   TEST_LDI("ldi.d", -136, w8);
   TEST_LDI("ldi.d", -184, w9);
   TEST_LDI("ldi.d", -184, w10);
   TEST_LDI("ldi.d",   40, w11);
   TEST_LDI("ldi.d",   16, w12);
   TEST_LDI("ldi.d", -376, w13);
   TEST_LDI("ldi.d", -328, w14);
   TEST_LDI("ldi.d",  440, w15);
   TEST_LDI("ldi.d", -304, w16);
   TEST_LDI("ldi.d",  -64, w17);
   TEST_LDI("ldi.d",   48, w18);
   TEST_LDI("ldi.d",    0, w19);
   TEST_LDI("ldi.d",  112, w20);
   TEST_LDI("ldi.d",  224, w21);
   TEST_LDI("ldi.d",  -88, w22);
   TEST_LDI("ldi.d",  -80, w23);
   TEST_LDI("ldi.d", -376, w24);
   TEST_LDI("ldi.d",  152, w25);
   TEST_LDI("ldi.d",  400, w26);
   TEST_LDI("ldi.d",  400, w27);
   TEST_LDI("ldi.d",   80, w28);
   TEST_LDI("ldi.d", -304, w29);
   TEST_LDI("ldi.d",  352, w30);
   TEST_LDI("ldi.d",  176, w31);
   TEST_LDI("ldi.w",  128, w0);
   TEST_LDI("ldi.w",  308, w1);
   TEST_LDI("ldi.w",  340, w2);
   TEST_LDI("ldi.w",   64, w3);
   TEST_LDI("ldi.w", -224, w4);
   TEST_LDI("ldi.w", -508, w5);
   TEST_LDI("ldi.w", -388, w6);
   TEST_LDI("ldi.w",  156, w7);
   TEST_LDI("ldi.w", -180, w8);
   TEST_LDI("ldi.w",  -56, w9);
   TEST_LDI("ldi.w", -312, w10);
   TEST_LDI("ldi.w",  356, w11);
   TEST_LDI("ldi.w",   80, w12);
   TEST_LDI("ldi.w", -120, w13);
   TEST_LDI("ldi.w",  292, w14);
   TEST_LDI("ldi.w",  288, w15);
   TEST_LDI("ldi.w",  336, w16);
   TEST_LDI("ldi.w", -164, w17);
   TEST_LDI("ldi.w", -220, w18);
   TEST_LDI("ldi.w",  -60, w19);
   TEST_LDI("ldi.w", -448, w20);
   TEST_LDI("ldi.w",  204, w21);
   TEST_LDI("ldi.w",  380, w22);
   TEST_LDI("ldi.w", -308, w23);
   TEST_LDI("ldi.w", -152, w24);
   TEST_LDI("ldi.w",  268, w25);
   TEST_LDI("ldi.w", -420, w26);
   TEST_LDI("ldi.w",  444, w27);
   TEST_LDI("ldi.w",  476, w28);
   TEST_LDI("ldi.w",  448, w29);
   TEST_LDI("ldi.w",  116, w30);
   TEST_LDI("ldi.w",   96, w31);
   TEST_LDI("ldi.h",  246, w0);
   TEST_LDI("ldi.h",  -52, w1);
   TEST_LDI("ldi.h", -350, w2);
   TEST_LDI("ldi.h", -488, w3);
   TEST_LDI("ldi.h",  -46, w4);
   TEST_LDI("ldi.h", -224, w5);
   TEST_LDI("ldi.h",  180, w6);
   TEST_LDI("ldi.h",  288, w7);
   TEST_LDI("ldi.h",  232, w8);
   TEST_LDI("ldi.h",  384, w9);
   TEST_LDI("ldi.h",  136, w10);
   TEST_LDI("ldi.h", -196, w11);
   TEST_LDI("ldi.h", -248, w12);
   TEST_LDI("ldi.h",  -82, w13);
   TEST_LDI("ldi.h", -418, w14);
   TEST_LDI("ldi.h", -422, w15);
   TEST_LDI("ldi.h",  266, w16);
   TEST_LDI("ldi.h", -126, w17);
   TEST_LDI("ldi.h",   30, w18);
   TEST_LDI("ldi.h",  332, w19);
   TEST_LDI("ldi.h", -432, w20);
   TEST_LDI("ldi.h", -100, w21);
   TEST_LDI("ldi.h", -486, w22);
   TEST_LDI("ldi.h",  -70, w23);
   TEST_LDI("ldi.h", -344, w24);
   TEST_LDI("ldi.h", -392, w25);
   TEST_LDI("ldi.h", -136, w26);
   TEST_LDI("ldi.h", -378, w27);
   TEST_LDI("ldi.h", -454, w28);
   TEST_LDI("ldi.h",  492, w29);
   TEST_LDI("ldi.h",  230, w30);
   TEST_LDI("ldi.h",  306, w31);
   TEST_LDI("ldi.b",  -71, w0);
   TEST_LDI("ldi.b",  394, w1);
   TEST_LDI("ldi.b",  330, w2);
   TEST_LDI("ldi.b",  395, w3);
   TEST_LDI("ldi.b", -342, w4);
   TEST_LDI("ldi.b",   -1, w5);
   TEST_LDI("ldi.b",  173, w6);
   TEST_LDI("ldi.b",  403, w7);
   TEST_LDI("ldi.b", -129, w8);
   TEST_LDI("ldi.b", -203, w9);
   TEST_LDI("ldi.b", -305, w10);
   TEST_LDI("ldi.b",  137, w11);
   TEST_LDI("ldi.b",  228, w12);
   TEST_LDI("ldi.b", -210, w13);
   TEST_LDI("ldi.b",  227, w14);
   TEST_LDI("ldi.b",  -17, w15);
   TEST_LDI("ldi.b",  177, w16);
   TEST_LDI("ldi.b", -253, w17);
   TEST_LDI("ldi.b", -196, w18);
   TEST_LDI("ldi.b",  258, w19);
   TEST_LDI("ldi.b",  159, w20);
   TEST_LDI("ldi.b", -169, w21);
   TEST_LDI("ldi.b", -324, w22);
   TEST_LDI("ldi.b",  327, w23);
   TEST_LDI("ldi.b",  -49, w24);
   TEST_LDI("ldi.b",   53, w25);
   TEST_LDI("ldi.b",  462, w26);
   TEST_LDI("ldi.b",   10, w27);
   TEST_LDI("ldi.b",   35, w28);
   TEST_LDI("ldi.b",  182, w29);
   TEST_LDI("ldi.b", -196, w30);
   TEST_LDI("ldi.b",  476, w31);
   TEST_MOVE("move.v", -224, w0,  w24);
   TEST_MOVE("move.v", -368, w1,  w12);
   TEST_MOVE("move.v",  -48, w2,  w28);
   TEST_MOVE("move.v",  -48, w3,  w21);
   TEST_MOVE("move.v",  320, w4,  w26);
   TEST_MOVE("move.v",  104, w5,  w18);
   TEST_MOVE("move.v",  -56, w6,  w30);
   TEST_MOVE("move.v",  -24, w7,  w7);
   TEST_MOVE("move.v",  176, w8,  w11);
   TEST_MOVE("move.v",  288, w9,  w18);
   TEST_MOVE("move.v", -256, w10, w12);
   TEST_MOVE("move.v", -408, w11, w5);
   TEST_MOVE("move.v",  232, w12, w9);
   TEST_MOVE("move.v",  -32, w13, w27);
   TEST_MOVE("move.v",  416, w14, w22);
   TEST_MOVE("move.v", -184, w15, w20);
   TEST_MOVE("move.v",  352, w16, w5);
   TEST_MOVE("move.v",  128, w17, w31);
   TEST_MOVE("move.v",   48, w18, w22);
   TEST_MOVE("move.v",  -80, w19, w23);
   TEST_MOVE("move.v", -408, w20, w27);
   TEST_MOVE("move.v", -424, w21, w25);
   TEST_MOVE("move.v",  392, w22, w24);
   TEST_MOVE("move.v",   32, w23, w6);
   TEST_MOVE("move.v", -488, w24, w0);
   TEST_MOVE("move.v", -440, w25, w10);
   TEST_MOVE("move.v",  360, w26, w28);
   TEST_MOVE("move.v", -448, w27, w24);
   TEST_MOVE("move.v",  -16, w28, w23);
   TEST_MOVE("move.v",  296, w29, w5);
   TEST_MOVE("move.v", -424, w30, w24);
   TEST_MOVE("move.v",   88, w31, w27);
   TEST_INSERT("insert.w", -472,  120, w0,  3, t1);
   TEST_INSERT("insert.w",  112,  160, w1,  0, t2);
   TEST_INSERT("insert.w",  400, -424, w2,  1, t3);
   TEST_INSERT("insert.w", -512,  480, w3,  2, t0);
   TEST_INSERT("insert.w", -408,  104, w4,  3, t1);
   TEST_INSERT("insert.w", -248,  296, w5,  0, t2);
   TEST_INSERT("insert.w",  128,  440, w6,  1, t3);
   TEST_INSERT("insert.w",    8,  152, w7,  2, t0);
   TEST_INSERT("insert.w",  192,  248, w8,  3, t1);
   TEST_INSERT("insert.w",  216,  112, w9,  0, t2);
   TEST_INSERT("insert.w",  120, -408, w10, 1, t3);
   TEST_INSERT("insert.w",  440,  440, w11, 2, t0);
   TEST_INSERT("insert.w",  176,  352, w12, 3, t1);
   TEST_INSERT("insert.w",  -64,  496, w13, 0, t2);
   TEST_INSERT("insert.w", -280, -192, w14, 1, t3);
   TEST_INSERT("insert.w",  440,  272, w15, 2, t0);
   TEST_INSERT("insert.w",  -80, -240, w16, 3, t1);
   TEST_INSERT("insert.w", -144,   40, w17, 0, t2);
   TEST_INSERT("insert.w",   16, -192, w18, 1, t3);
   TEST_INSERT("insert.w",  432, -144, w19, 2, t0);
   TEST_INSERT("insert.w", -352,  120, w20, 3, t1);
   TEST_INSERT("insert.w",   56, -328, w21, 0, t2);
   TEST_INSERT("insert.w",  336,  296, w22, 1, t3);
   TEST_INSERT("insert.w",   32, -448, w23, 2, t0);
   TEST_INSERT("insert.w",  184,   16, w24, 3, t1);
   TEST_INSERT("insert.w",  128, -256, w25, 0, t2);
   TEST_INSERT("insert.w", -328, -208, w26, 1, t3);
   TEST_INSERT("insert.w", -368,   56, w27, 2, t0);
   TEST_INSERT("insert.w",   40,  360, w28, 3, t1);
   TEST_INSERT("insert.w", -336,   80, w29, 0, t2);
   TEST_INSERT("insert.w", -160,  280, w30, 1, t3);
   TEST_INSERT("insert.w", -472, -408, w31, 2, t0);
   TEST_INSERT("insert.h",  152,  280, w0,  1, t2);
   TEST_INSERT("insert.h", -416, -104, w1,  2, t3);
   TEST_INSERT("insert.h",  272, -344, w2,  3, t0);
   TEST_INSERT("insert.h",  296, -496, w3,  4, t1);
   TEST_INSERT("insert.h", -304,  440, w4,  5, t2);
   TEST_INSERT("insert.h",  232, -160, w5,  6, t3);
   TEST_INSERT("insert.h",  -96, -480, w6,  7, t0);
   TEST_INSERT("insert.h",   56, -240, w7,  0, t1);
   TEST_INSERT("insert.h",   16, -416, w8,  1, t2);
   TEST_INSERT("insert.h", -112, -328, w9,  2, t3);
   TEST_INSERT("insert.h",  240,  200, w10, 3, t0);
   TEST_INSERT("insert.h",   56,   32, w11, 4, t1);
   TEST_INSERT("insert.h", -392, -224, w12, 5, t2);
   TEST_INSERT("insert.h", -456,  232, w13, 6, t3);
   TEST_INSERT("insert.h",  344,  -32, w14, 7, t0);
   TEST_INSERT("insert.h", -264, -408, w15, 0, t1);
   TEST_INSERT("insert.h",    8,  -16, w16, 1, t2);
   TEST_INSERT("insert.h",  160, -168, w17, 2, t3);
   TEST_INSERT("insert.h", -144, -224, w18, 3, t0);
   TEST_INSERT("insert.h", -296,  -56, w19, 4, t1);
   TEST_INSERT("insert.h",  304,   64, w20, 5, t2);
   TEST_INSERT("insert.h",  104,  448, w21, 6, t3);
   TEST_INSERT("insert.h", -304, -304, w22, 7, t0);
   TEST_INSERT("insert.h", -200, -352, w23, 0, t1);
   TEST_INSERT("insert.h", -168,  232, w24, 1, t2);
   TEST_INSERT("insert.h",  -72,  200, w25, 2, t3);
   TEST_INSERT("insert.h", -272, -432, w26, 3, t0);
   TEST_INSERT("insert.h", -136,  496, w27, 4, t1);
   TEST_INSERT("insert.h",  224, -152, w28, 5, t2);
   TEST_INSERT("insert.h",  336,  -72, w29, 6, t3);
   TEST_INSERT("insert.h",   32,   56, w30, 7, t0);
   TEST_INSERT("insert.h", -464, -432, w31, 0, t1);
   TEST_INSERT("insert.b",  272,  368, w0,   2, t3);
   TEST_INSERT("insert.b",  336,   72, w1,   3, t0);
   TEST_INSERT("insert.b", -128,  -48, w2,   4, t1);
   TEST_INSERT("insert.b", -384, -408, w3,   5, t2);
   TEST_INSERT("insert.b",  232, -112, w4,   6, t3);
   TEST_INSERT("insert.b",   88, -200, w5,   7, t0);
   TEST_INSERT("insert.b",  296,   96, w6,   8, t1);
   TEST_INSERT("insert.b", -208, -448, w7,   9, t2);
   TEST_INSERT("insert.b", -176,  320, w8,  10, t3);
   TEST_INSERT("insert.b",   96,  448, w9,  11, t0);
   TEST_INSERT("insert.b",  336,  472, w10, 12, t1);
   TEST_INSERT("insert.b", -104, -120, w11, 13, t2);
   TEST_INSERT("insert.b",  224,  472, w12, 14, t3);
   TEST_INSERT("insert.b", -168, -248, w13, 15, t0);
   TEST_INSERT("insert.b", -104,  120, w14,  0, t1);
   TEST_INSERT("insert.b",  -16, -104, w15,  1, t2);
   TEST_INSERT("insert.b",  488, -344, w16,  2, t3);
   TEST_INSERT("insert.b",  120, -192, w17,  3, t0);
   TEST_INSERT("insert.b",  -88, -152, w18,  4, t1);
   TEST_INSERT("insert.b",  248,  248, w19,  5, t2);
   TEST_INSERT("insert.b", -456, -368, w20,  6, t3);
   TEST_INSERT("insert.b",  248, -168, w21,  7, t0);
   TEST_INSERT("insert.b", -104,  352, w22,  8, t1);
   TEST_INSERT("insert.b",  168, -472, w23,  9, t2);
   TEST_INSERT("insert.b",  496,  240, w24, 10, t3);
   TEST_INSERT("insert.b",  200, -240, w25, 11, t0);
   TEST_INSERT("insert.b",  152,  320, w26, 12, t1);
   TEST_INSERT("insert.b",  288, -408, w27, 13, t2);
   TEST_INSERT("insert.b", -136, -136, w28, 14, t3);
   TEST_INSERT("insert.b",  504, -392, w29, 15, t0);
   TEST_INSERT("insert.b",   24,  272, w30,  0, t1);
   TEST_INSERT("insert.b",  440,  -24, w31,  1, t2);
   TEST_FILL("fill.w", -264, w0,  t1);
   TEST_FILL("fill.w",  216, w1,  t2);
   TEST_FILL("fill.w",  360, w2,  t3);
   TEST_FILL("fill.w",  -40, w3,  t0);
   TEST_FILL("fill.w", -352, w4,  t1);
   TEST_FILL("fill.w", -304, w5,  t2);
   TEST_FILL("fill.w",  352, w6,  t3);
   TEST_FILL("fill.w",  216, w7,  t0);
   TEST_FILL("fill.w", -448, w8,  t1);
   TEST_FILL("fill.w", -488, w9,  t2);
   TEST_FILL("fill.w", -320, w10, t3);
   TEST_FILL("fill.w",  488, w11, t0);
   TEST_FILL("fill.w", -464, w12, t1);
   TEST_FILL("fill.w",  176, w13, t2);
   TEST_FILL("fill.w",  184, w14, t3);
   TEST_FILL("fill.w",  176, w15, t0);
   TEST_FILL("fill.w",  176, w16, t1);
   TEST_FILL("fill.w",  344, w17, t2);
   TEST_FILL("fill.w",  424, w18, t3);
   TEST_FILL("fill.w",  168, w19, t0);
   TEST_FILL("fill.w",  -96, w20, t1);
   TEST_FILL("fill.w", -280, w21, t2);
   TEST_FILL("fill.w", -232, w22, t3);
   TEST_FILL("fill.w",   24, w23, t0);
   TEST_FILL("fill.w",  -88, w24, t1);
   TEST_FILL("fill.w", -448, w25, t2);
   TEST_FILL("fill.w",  488, w26, t3);
   TEST_FILL("fill.w",  400, w27, t0);
   TEST_FILL("fill.w", -200, w28, t1);
   TEST_FILL("fill.w", -472, w29, t2);
   TEST_FILL("fill.w", -192, w30, t3);
   TEST_FILL("fill.w",   56, w31, t0);
   TEST_FILL("fill.h",  264, w0,  t3);
   TEST_FILL("fill.h", -344, w1,  t0);
   TEST_FILL("fill.h", -496, w2,  t1);
   TEST_FILL("fill.h",  424, w3,  t2);
   TEST_FILL("fill.h", -136, w4,  t3);
   TEST_FILL("fill.h",  368, w5,  t0);
   TEST_FILL("fill.h",  136, w6,  t1);
   TEST_FILL("fill.h",  -72, w7,  t2);
   TEST_FILL("fill.h",  400, w8,  t3);
   TEST_FILL("fill.h",  336, w9,  t0);
   TEST_FILL("fill.h",  -88, w10, t1);
   TEST_FILL("fill.h",  448, w11, t2);
   TEST_FILL("fill.h",    8, w12, t3);
   TEST_FILL("fill.h", -416, w13, t0);
   TEST_FILL("fill.h",  112, w14, t1);
   TEST_FILL("fill.h", -328, w15, t2);
   TEST_FILL("fill.h",  448, w16, t3);
   TEST_FILL("fill.h",   32, w17, t0);
   TEST_FILL("fill.h",  352, w18, t1);
   TEST_FILL("fill.h", -160, w19, t2);
   TEST_FILL("fill.h",  264, w20, t3);
   TEST_FILL("fill.h", -384, w21, t0);
   TEST_FILL("fill.h",  384, w22, t1);
   TEST_FILL("fill.h", -328, w23, t2);
   TEST_FILL("fill.h", -312, w24, t3);
   TEST_FILL("fill.h",  360, w25, t0);
   TEST_FILL("fill.h", -440, w26, t1);
   TEST_FILL("fill.h",    0, w27, t2);
   TEST_FILL("fill.h",  400, w28, t3);
   TEST_FILL("fill.h", -120, w29, t0);
   TEST_FILL("fill.h", -448, w30, t1);
   TEST_FILL("fill.h",  160, w31, t2);
   TEST_FILL("fill.b",   56, w0,  t2);
   TEST_FILL("fill.b", -432, w1,  t3);
   TEST_FILL("fill.b",   72, w2,  t0);
   TEST_FILL("fill.b",  432, w3,  t1);
   TEST_FILL("fill.b",  456, w4,  t2);
   TEST_FILL("fill.b", -296, w5,  t3);
   TEST_FILL("fill.b", -144, w6,  t0);
   TEST_FILL("fill.b",  352, w7,  t1);
   TEST_FILL("fill.b", -464, w8,  t2);
   TEST_FILL("fill.b",  280, w9,  t3);
   TEST_FILL("fill.b",  288, w10, t0);
   TEST_FILL("fill.b",   56, w11, t1);
   TEST_FILL("fill.b",  384, w12, t2);
   TEST_FILL("fill.b", -104, w13, t3);
   TEST_FILL("fill.b",  248, w14, t0);
   TEST_FILL("fill.b",  320, w15, t1);
   TEST_FILL("fill.b",  440, w16, t2);
   TEST_FILL("fill.b",   88, w17, t3);
   TEST_FILL("fill.b", -344, w18, t0);
   TEST_FILL("fill.b",  200, w19, t1);
   TEST_FILL("fill.b",  224, w20, t2);
   TEST_FILL("fill.b", -472, w21, t3);
   TEST_FILL("fill.b",  384, w22, t0);
   TEST_FILL("fill.b",  424, w23, t1);
   TEST_FILL("fill.b",  400, w24, t2);
   TEST_FILL("fill.b",  464, w25, t3);
   TEST_FILL("fill.b",  -80, w26, t0);
   TEST_FILL("fill.b",  296, w27, t1);
   TEST_FILL("fill.b", -168, w28, t2);
   TEST_FILL("fill.b",  -16, w29, t3);
   TEST_FILL("fill.b",  -56, w30, t0);
   TEST_FILL("fill.b",  400, w31, t1);
   TEST_COPY("copy_u.h",   32, w0,  2, t3);
   TEST_COPY("copy_u.h",  160, w1,  3, t0);
   TEST_COPY("copy_u.h", -456, w2,  4, t1);
   TEST_COPY("copy_u.h",   32, w3,  5, t2);
   TEST_COPY("copy_u.h",   72, w4,  6, t3);
   TEST_COPY("copy_u.h",  240, w5,  7, t0);
   TEST_COPY("copy_u.h",  -64, w6,  0, t1);
   TEST_COPY("copy_u.h",  -48, w7,  1, t2);
   TEST_COPY("copy_u.h", -280, w8,  2, t3);
   TEST_COPY("copy_u.h",   48, w9,  3, t0);
   TEST_COPY("copy_u.h",  264, w10, 4, t1);
   TEST_COPY("copy_u.h", -104, w11, 5, t2);
   TEST_COPY("copy_u.h",   -8, w12, 6, t3);
   TEST_COPY("copy_u.h",  320, w13, 7, t0);
   TEST_COPY("copy_u.h", -400, w14, 0, t1);
   TEST_COPY("copy_u.h", -504, w15, 1, t2);
   TEST_COPY("copy_u.h",  328, w16, 2, t3);
   TEST_COPY("copy_u.h",   -8, w17, 3, t0);
   TEST_COPY("copy_u.h", -144, w18, 4, t1);
   TEST_COPY("copy_u.h", -208, w19, 5, t2);
   TEST_COPY("copy_u.h",  192, w20, 6, t3);
   TEST_COPY("copy_u.h", -512, w21, 7, t0);
   TEST_COPY("copy_u.h", -136, w22, 0, t1);
   TEST_COPY("copy_u.h", -352, w23, 1, t2);
   TEST_COPY("copy_u.h", -416, w24, 2, t3);
   TEST_COPY("copy_u.h", -392, w25, 3, t0);
   TEST_COPY("copy_u.h",  160, w26, 4, t1);
   TEST_COPY("copy_u.h", -280, w27, 5, t2);
   TEST_COPY("copy_u.h", -232, w28, 6, t3);
   TEST_COPY("copy_u.h", -456, w29, 7, t0);
   TEST_COPY("copy_u.h",  328, w30, 0, t1);
   TEST_COPY("copy_u.h",  312, w31, 1, t2);
   TEST_COPY("copy_u.b",  216, w0,   3, t1);
   TEST_COPY("copy_u.b",  384, w1,   4, t2);
   TEST_COPY("copy_u.b", -160, w2,   5, t3);
   TEST_COPY("copy_u.b", -216, w3,   6, t0);
   TEST_COPY("copy_u.b",  112, w4,   7, t1);
   TEST_COPY("copy_u.b",  296, w5,   8, t2);
   TEST_COPY("copy_u.b",  248, w6,   9, t3);
   TEST_COPY("copy_u.b",  352, w7,  10, t0);
   TEST_COPY("copy_u.b", -168, w8,  11, t1);
   TEST_COPY("copy_u.b",    8, w9,  12, t2);
   TEST_COPY("copy_u.b", -264, w10, 13, t3);
   TEST_COPY("copy_u.b",  344, w11, 14, t0);
   TEST_COPY("copy_u.b", -184, w12, 15, t1);
   TEST_COPY("copy_u.b", -152, w13,  0, t2);
   TEST_COPY("copy_u.b",  352, w14,  1, t3);
   TEST_COPY("copy_u.b", -368, w15,  2, t0);
   TEST_COPY("copy_u.b",  360, w16,  3, t1);
   TEST_COPY("copy_u.b", -296, w17,  4, t2);
   TEST_COPY("copy_u.b",  -56, w18,  5, t3);
   TEST_COPY("copy_u.b",   48, w19,  6, t0);
   TEST_COPY("copy_u.b", -288, w20,  7, t1);
   TEST_COPY("copy_u.b",  328, w21,  8, t2);
   TEST_COPY("copy_u.b",  216, w22,  9, t3);
   TEST_COPY("copy_u.b", -184, w23, 10, t0);
   TEST_COPY("copy_u.b",  456, w24, 11, t1);
   TEST_COPY("copy_u.b", -128, w25, 12, t2);
   TEST_COPY("copy_u.b",   48, w26, 13, t3);
   TEST_COPY("copy_u.b", -288, w27, 14, t0);
   TEST_COPY("copy_u.b",  -72, w28, 15, t1);
   TEST_COPY("copy_u.b", -136, w29,  0, t2);
   TEST_COPY("copy_u.b", -488, w30,  1, t3);
   TEST_COPY("copy_u.b", -360, w31,  2, t0);
   TEST_COPY("copy_s.w", -488, w0,  1, t2);
   TEST_COPY("copy_s.w",  336, w1,  2, t3);
   TEST_COPY("copy_s.w",  496, w2,  3, t0);
   TEST_COPY("copy_s.w", -312, w3,  0, t1);
   TEST_COPY("copy_s.w", -496, w4,  1, t2);
   TEST_COPY("copy_s.w",  192, w5,  2, t3);
   TEST_COPY("copy_s.w",  216, w6,  3, t0);
   TEST_COPY("copy_s.w",  224, w7,  0, t1);
   TEST_COPY("copy_s.w",  144, w8,  1, t2);
   TEST_COPY("copy_s.w", -440, w9,  2, t3);
   TEST_COPY("copy_s.w",  248, w10, 3, t0);
   TEST_COPY("copy_s.w",  464, w11, 0, t1);
   TEST_COPY("copy_s.w", -224, w12, 1, t2);
   TEST_COPY("copy_s.w",  424, w13, 2, t3);
   TEST_COPY("copy_s.w", -384, w14, 3, t0);
   TEST_COPY("copy_s.w",  208, w15, 0, t1);
   TEST_COPY("copy_s.w",   32, w16, 1, t2);
   TEST_COPY("copy_s.w",  352, w17, 2, t3);
   TEST_COPY("copy_s.w", -160, w18, 3, t0);
   TEST_COPY("copy_s.w",  496, w19, 0, t1);
   TEST_COPY("copy_s.w", -232, w20, 1, t2);
   TEST_COPY("copy_s.w", -192, w21, 2, t3);
   TEST_COPY("copy_s.w", -128, w22, 3, t0);
   TEST_COPY("copy_s.w", -432, w23, 0, t1);
   TEST_COPY("copy_s.w",  336, w24, 1, t2);
   TEST_COPY("copy_s.w",  504, w25, 2, t3);
   TEST_COPY("copy_s.w", -192, w26, 3, t0);
   TEST_COPY("copy_s.w",  216, w27, 0, t1);
   TEST_COPY("copy_s.w",  136, w28, 1, t2);
   TEST_COPY("copy_s.w",  208, w29, 2, t3);
   TEST_COPY("copy_s.w",  352, w30, 3, t0);
   TEST_COPY("copy_s.w",  160, w31, 0, t1);
   TEST_COPY("copy_s.h",   32, w0,  2, t3);
   TEST_COPY("copy_s.h",  344, w1,  3, t0);
   TEST_COPY("copy_s.h",  360, w2,  4, t1);
   TEST_COPY("copy_s.h",   56, w3,  5, t2);
   TEST_COPY("copy_s.h",   24, w4,  6, t3);
   TEST_COPY("copy_s.h",   72, w5,  7, t0);
   TEST_COPY("copy_s.h", -232, w6,  0, t1);
   TEST_COPY("copy_s.h", -336, w7,  1, t2);
   TEST_COPY("copy_s.h",  152, w8,  2, t3);
   TEST_COPY("copy_s.h", -496, w9,  3, t0);
   TEST_COPY("copy_s.h", -376, w10, 4, t1);
   TEST_COPY("copy_s.h",  440, w11, 5, t2);
   TEST_COPY("copy_s.h",  440, w12, 6, t3);
   TEST_COPY("copy_s.h", -248, w13, 7, t0);
   TEST_COPY("copy_s.h",  136, w14, 0, t1);
   TEST_COPY("copy_s.h",  -32, w15, 1, t2);
   TEST_COPY("copy_s.h", -408, w16, 2, t3);
   TEST_COPY("copy_s.h",  488, w17, 3, t0);
   TEST_COPY("copy_s.h",  -48, w18, 4, t1);
   TEST_COPY("copy_s.h", -128, w19, 5, t2);
   TEST_COPY("copy_s.h", -208, w20, 6, t3);
   TEST_COPY("copy_s.h",  344, w21, 7, t0);
   TEST_COPY("copy_s.h",  -48, w22, 0, t1);
   TEST_COPY("copy_s.h", -384, w23, 1, t2);
   TEST_COPY("copy_s.h",  336, w24, 2, t3);
   TEST_COPY("copy_s.h",  280, w25, 3, t0);
   TEST_COPY("copy_s.h",  352, w26, 4, t1);
   TEST_COPY("copy_s.h",  -40, w27, 5, t2);
   TEST_COPY("copy_s.h",  -24, w28, 6, t3);
   TEST_COPY("copy_s.h",  192, w29, 7, t0);
   TEST_COPY("copy_s.h", -384, w30, 0, t1);
   TEST_COPY("copy_s.h", -496, w31, 1, t2);
   TEST_COPY("copy_s.b",   32, w0,   3, t1);
   TEST_COPY("copy_s.b",  496, w1,   4, t2);
   TEST_COPY("copy_s.b",   72, w2,   5, t3);
   TEST_COPY("copy_s.b", -448, w3,   6, t0);
   TEST_COPY("copy_s.b",   56, w4,   7, t1);
   TEST_COPY("copy_s.b",  360, w5,   8, t2);
   TEST_COPY("copy_s.b", -272, w6,   9, t3);
   TEST_COPY("copy_s.b", -304, w7,  10, t0);
   TEST_COPY("copy_s.b",  384, w8,  11, t1);
   TEST_COPY("copy_s.b", -136, w9,  12, t2);
   TEST_COPY("copy_s.b", -376, w10, 13, t3);
   TEST_COPY("copy_s.b",  320, w11, 14, t0);
   TEST_COPY("copy_s.b",  136, w12, 15, t1);
   TEST_COPY("copy_s.b",  280, w13,  0, t2);
   TEST_COPY("copy_s.b", -224, w14,  1, t3);
   TEST_COPY("copy_s.b",  240, w15,  2, t0);
   TEST_COPY("copy_s.b",  256, w16,  3, t1);
   TEST_COPY("copy_s.b",  248, w17,  4, t2);
   TEST_COPY("copy_s.b", -392, w18,  5, t3);
   TEST_COPY("copy_s.b", -456, w19,  6, t0);
   TEST_COPY("copy_s.b",   88, w20,  7, t1);
   TEST_COPY("copy_s.b",   72, w21,  8, t2);
   TEST_COPY("copy_s.b", -328, w22,  9, t3);
   TEST_COPY("copy_s.b",  -88, w23, 10, t0);
   TEST_COPY("copy_s.b", -160, w24, 11, t1);
   TEST_COPY("copy_s.b", -480, w25, 12, t2);
   TEST_COPY("copy_s.b",  392, w26, 13, t3);
   TEST_COPY("copy_s.b",  328, w27, 14, t0);
   TEST_COPY("copy_s.b",  232, w28, 15, t1);
   TEST_COPY("copy_s.b", -496, w29,  0, t2);
   TEST_COPY("copy_s.b",  344, w30,  1, t3);
   TEST_COPY("copy_s.b", -248, w31,  2, t0);
#else
   printf("This test requires MSA extension.\n");
#endif
   return 0;
}
