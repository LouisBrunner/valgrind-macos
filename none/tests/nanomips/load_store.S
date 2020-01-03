   .data
   test_data:
       .word 0x01234567
       .word 0x89ABCDEF
   test_data2:
       .word 0x0
       .word 0x1
       .word 0x2
       .word 0x3
       .word 0x4
       .word 0x5
       .word 0x6
       .word 0x7
       .word 0x8
       .word 0x9
       .word 0xA
       .word 0xB
       .word 0xC
       .word 0xD
       .word 0xE
       .word 0xF
   test_ints: #12
       .word 0x0
       .word 0xFFFFFFFF
       .word 0x55555555
       .word 0x00000002
       .word 0xAAAAAAAA
       .word 0x33333333
       .word 0xFFFFFFFE
       .word 0x00000001
       .word 0xCCCCCCCC
       .word 0x0000000A
       .word 0x12345678
       .word 0x9ABCDEF
   reg_name_0:
       .ascii "$r0 %08x\n"
       .byte 0
   reg_name_1:
       .ascii "$at %08x\n"
       .byte 0
   reg_name_2:
       .ascii "$t4 %08x\n"
       .byte 0
   reg_name_3:
       .ascii "$t5 %08x\n"
       .byte 0
   reg_name_4:
       .ascii "$a0 %08x\n"
       .byte 0
   reg_name_5:
       .ascii "$a1 %08x\n"
       .byte 0
   reg_name_6:
       .ascii "$a2 %08x\n"
       .byte 0
   reg_name_7:
       .ascii "$a3 %08x\n"
       .byte 0
   reg_name_8:
       .ascii "$a4 %08x\n"
       .byte 0
   reg_name_9:
       .ascii "$a5 %08x\n"
       .byte 0
   reg_name_10:
       .ascii "$a6 %08x\n"
       .byte 0
   reg_name_11:
       .ascii "$a7 %08x\n"
       .byte 0
   reg_name_12:
       .ascii "$t0 %08x\n"
       .byte 0
   reg_name_13:
       .ascii "$t1 %08x\n"
       .byte 0
   reg_name_14:
       .ascii "$t2 %08x\n"
       .byte 0
   reg_name_15:
       .ascii "$t3 %08x\n"
       .byte 0
   reg_name_16:
       .ascii "$s0 %08x\n"
       .byte 0
   reg_name_17:
       .ascii "$s1 %08x\n"
       .byte 0
   reg_name_18:
       .ascii "$s2 %08x\n"
       .byte 0
   reg_name_19:
       .ascii "$s3 %08x\n"
       .byte 0
   reg_name_20:
       .ascii "$s4 %08x\n"
       .byte 0
   reg_name_21:
       .ascii "$s5 %08x\n"
       .byte 0
   reg_name_22:
       .ascii "$s6 %08x\n"
       .byte 0
   reg_name_23:
       .ascii "$s7 %08x\n"
       .byte 0
   reg_name_24:
       .ascii "$t8 %08x\n"
       .byte 0
   reg_name_25:
       .ascii "$t9 %08x\n"
       .byte 0
   reg_name_26:
       .ascii "$k0 %08x\n"
       .byte 0
   reg_name_27:
       .ascii "$k1 %08x\n"
       .byte 0
   reg_name_28:
       .ascii "$gp %08x\n"
       .byte 0
   reg_name_29:
       .ascii "$sp %08x\n"
       .byte 0
   reg_name_30:
       .ascii "$fp %08x\n"
       .byte 0
   reg_name_31:
       .ascii "$ra %08x\n"
       .byte 0
   print_4_hex_num:
       .ascii "%08x %08x %08x %08x\n"
       .byte 0

   .text
       .align   1
   .set  noreorder
   .set  nomacro

   .macro LOAD_ONE label, instruction, reg1, reg2, offset, offset2
       .data
       &label:
         .ascii "&instruction &reg1 %08x &offset(&reg2) &offset2\n"
         .byte 0
       .text
         li &reg2, test_data + &offset2
         &instruction &reg1, &offset(&reg2)
         move $a1, &reg1
         li $a0, &label
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro LOAD_ONE_INDEXED label, instruction, reg1, reg2, reg3, offset, offset2
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg3(&reg2) &offset2\n"
         .byte 0
       .text
         li &reg2, test_data + &offset2
         li &reg3, &offset
         &instruction &reg1, &reg3(&reg2)
         move $a1, &reg1
         li $a0, &label
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro STORE_ONE label, instruction, reg1, reg2, offset, offset2, val
       .data
       &label:
         .ascii "&instruction &reg1 &val &offset(&reg2) &offset &offset2 mem: %08x%08x\n"
         .byte 0
       .text
         li &reg2, test_data + &offset2
         li &reg1, &val
         &instruction &reg1, &offset(&reg2)
         li $a0, &label
         li $s0, test_data
         lw $a1, 0($s0)
         lw $a2, 4($s0)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
         li $t0, 0x01234567
         li $t1, 0x89ABCDEF
         sw $t0, 0($s0)
         sw $t1, 4($s0)
   .endm

   .macro STORE_ONE_INDEXED label, instruction, reg1, reg2, reg3, offset, offset2, val
       .data
       &label:
        .ascii "&instruction &reg1 &val &reg3(&reg2) &offset &offset2 mem: %08x%08x\n"
        .byte 0
       .text
         li &reg2, test_data + &offset2
         li &reg3, &offset
         li &reg1, &val
         &instruction &reg1, &reg3(&reg2)
         li $a0, &label
         li $s0, test_data
         lw $a1, 0($s0)
         lw $a2, 4($s0)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
         li $t0, 0x01234567
         li $t1, 0x89ABCDEF
         sw $t0, 0($s0)
         sw $t1, 4($s0)
   .endm

   .macro LOAD_ONE_SPECIAL label, instruction, special, reg, offset, offset2
       .data
       &label:
         .ascii "&instruction &reg %08x &offset(&special) &offset2\n"
         .byte 0
       .text
         move $ra, $sp
         sw &special, 4($ra)
         li &special, test_data + &offset2
         &instruction &reg, &offset(&special)
         move $a1, &reg
         li $a0, &label
         lw &special, 4($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro STORE_ONE_SPECIAL label, instruction, special, reg, offset, offset2, val
       .data
       &label:
         .ascii "&instruction &reg &val &offset(&special) &offset2 mem: %08x%08x\n"
         .byte 0
       .text
         move $ra, $sp
         sw &special, 4($ra)
         li &special, test_data + &offset2
         li &reg, &val
         &instruction &reg, &offset(&special)
         li $a0, &label
         li $s0, test_data
         lw $a1, 0($s0)
         lw $a2, 4($s0)
         lw &special, 4($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
         li $t0, 0x01234567
         li $t1, 0x89ABCDEF
         sw $t0, 0($s0)
         sw $t1, 4($s0)
   .endm

   .macro LOAD_IMMIDIATE label, instruction, reg, val
       .data
       &label:
         .ascii "&instruction &reg, &val %08x\n"
         .byte 0
       .text
         &instruction &reg, &val
         move $a1, &reg
         li $a0, &label
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro LOAD_IMMIDIATE_HI label, instruction, reg, val
       .data
       &label:
         .ascii "&instruction &reg, %%hi(&val) %08x\n"
         .byte 0
       .text
         &instruction &reg, %hi(&val)
         move $a1, &reg
         li $a0, &label
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro THREE_REG_1_IMM label, instruction, reg1, reg2, reg3, offset1, offset2, mem1, mem2, imm
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x &reg3 %08x, &imm\n"
         .byte 0
       .text
         li $ra, &mem1
         lw &reg2, &offset1($ra)
         li $ra, &mem2
         lw &reg3, &offset2($ra)
         &instruction &reg1, &reg2, &reg3, &imm
         move $a1, &reg1
         li $a0, &label
         lw $a3, &offset2($ra)
         li $ra, &mem1
         lw $a2, &offset1($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro LOAD_STORE_MULTIPLE instruction, label, regt, regs, offset, offset2, count
       .data
       &label:
         .ascii "&instruction &regt &offset(&regs) &offset2 &count\n"
         .byte 0
       .text
         li &regs, test_data2 + &offset2
         &instruction &regt, &offset(&regs), &count
   .endm


   .globl cleanup_test_data2
   .ent   cleanup_test_data2
   .type  cleanup_test_data2, @function
cleanup_test_data2:
   li $t0, test_data2
   li $t1, 0
loop:
   swxs $t1, $t1($t0)
   addiu $t1, $t1, 1
   bneic $t1, 16, loop
   jrc   $ra
   .end  cleanup_test_data2
   .size cleanup_test_data2, .-cleanup_test_data2

   .globl print_test_data2
   .ent   print_test_data2
   .type  print_test_data2, @function
print_test_data2:
   save  16, $ra

   li $s0, test_data2
   li $a0, print_4_hex_num
   lw $a1, 0($s0)
   lw $a2, 4($s0)
   lw $a3, 8($s0)
   lw $a4, 12($s0)
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, print_4_hex_num
   lw $a1, 16($s0)
   lw $a2, 20($s0)
   lw $a3, 24($s0)
   lw $a4, 28($s0)
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, print_4_hex_num
   lw $a1, 32($s0)
   lw $a2, 36($s0)
   lw $a3, 40($s0)
   lw $a4, 44($s0)
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, print_4_hex_num
   lw $a1, 48($s0)
   lw $a2, 52($s0)
   lw $a3, 56($s0)
   lw $a4, 60($s0)
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   restore 16, $ra
   jrc   $ra
   .set  macro
   .set  reorder
   .end  print_test_data2
   .size print_test_data2, .-print_test_data2


   .globl main
   .ent   main
   .type  main, @function
   .text
main:

   save  16, $ra

# lb[u12]
   LOAD_ONE message1, lb[u12], $t3, $s5, 3244, -3239
   LOAD_ONE message2, lb[u12], $t1, $s3, 2897, -2894
   LOAD_ONE message3, lb[u12], $a7, $s5, 2852, -2846
   LOAD_ONE message4, lb[u12], $a4, $t2, 3047, -3046
   LOAD_ONE message5, lb[u12], $t3, $t0, 633, -629
   LOAD_ONE message6, lb[u12], $s7, $a7, 704, -700
   LOAD_ONE message7, lb[u12], $s1, $t2, 940, -939
   LOAD_ONE message8, lb[u12], $t3, $a2, 2006, -2000
   LOAD_ONE message9, lb[u12], $s7, $a6, 1428, -1427
   LOAD_ONE message10, lb[u12], $t1, $t1, 308, -308
   LOAD_ONE message11, lb[u12], $s2, $a0, 623, -621
   LOAD_ONE message12, lb[u12], $s2, $a2, 539, -537
   LOAD_ONE message13, lb[u12], $t2, $s4, 2958, -2957
   LOAD_ONE message14, lb[u12], $s3, $a6, 1501, -1496
   LOAD_ONE message15, lb[u12], $t0, $s6, 438, -434

# lb[16]
   LOAD_ONE message16, lb[16], $s0, $s0, 2, 5
   LOAD_ONE message17, lb[16], $s2, $s2, 0, 0
   LOAD_ONE message18, lb[16], $a3, $s0, 0, 5
   LOAD_ONE message19, lb[16], $s1, $s3, 3, 0
   LOAD_ONE message20, lb[16], $s2, $a2, 1, -1
   LOAD_ONE message21, lb[16], $s2, $a3, 2, 0
   LOAD_ONE message22, lb[16], $a1, $s3, 3, -1
   LOAD_ONE message23, lb[16], $a1, $s1, 2, 3
   LOAD_ONE message24, lb[16], $a2, $a0, 1, -1
   LOAD_ONE message25, lb[16], $s3, $s1, 0, 2
   LOAD_ONE message26, lb[16], $a2, $a1, 0, 7
   LOAD_ONE message27, lb[16], $a0, $s3, 2, 4
   LOAD_ONE message28, lb[16], $a2, $a0, 3, -3
   LOAD_ONE message29, lb[16], $a3, $a1, 2, 3
   LOAD_ONE message30, lb[16], $a0, $a1, 3, -1

# lb[gp]
   LOAD_ONE_SPECIAL message31, lb[gp], $gp, $t3, 37027, -37026
   LOAD_ONE_SPECIAL message32, lb[gp], $gp, $s5, 55060, -55056
   LOAD_ONE_SPECIAL message33, lb[gp], $gp, $t2, 182942, -182938
   LOAD_ONE_SPECIAL message34, lb[gp], $gp, $s7, 42677, -42674
   LOAD_ONE_SPECIAL message35, lb[gp], $gp, $s2, 129018, -129018
   LOAD_ONE_SPECIAL message36, lb[gp], $gp, $t1, 68297, -68296
   LOAD_ONE_SPECIAL message37, lb[gp], $gp, $a6, 58298, -58296
   LOAD_ONE_SPECIAL message38, lb[gp], $gp, $a1, 261052, -261048
   LOAD_ONE_SPECIAL message39, lb[gp], $gp, $s2, 41804, -41797
   LOAD_ONE_SPECIAL message40, lb[gp], $gp, $s0, 206952, -206950
   LOAD_ONE_SPECIAL message41, lb[gp], $gp, $t0, 1754, -1751
   LOAD_ONE_SPECIAL message42, lb[gp], $gp, $t2, 79423, -79423
   LOAD_ONE_SPECIAL message43, lb[gp], $gp, $s7, 261977, -261973
   LOAD_ONE_SPECIAL message44, lb[gp], $gp, $a2, 224338, -224334
   LOAD_ONE_SPECIAL message45, lb[gp], $gp, $a5, 79898, -79896

# lb[s9]
   LOAD_ONE message46, lb[s9], $a0, $s7, 38, -36
   LOAD_ONE message47, lb[s9], $a7, $a7, 130, -123
   LOAD_ONE message48, lb[s9], $t2, $a6, -166, 168
   LOAD_ONE message49, lb[s9], $t1, $a4, 22, -19
   LOAD_ONE message50, lb[s9], $s2, $a1, 243, -242
   LOAD_ONE message51, lb[s9], $s6, $s4, -251, 251
   LOAD_ONE message52, lb[s9], $t2, $s3, -188, 192
   LOAD_ONE message53, lb[s9], $t2, $s6, 254, -247
   LOAD_ONE message54, lb[s9], $a1, $s5, 233, -229
   LOAD_ONE message55, lb[s9], $a0, $a7, -148, 150
   LOAD_ONE message56, lb[s9], $s5, $a2, -171, 173
   LOAD_ONE message57, lb[s9], $a6, $s7, 141, -141
   LOAD_ONE message58, lb[s9], $s0, $s5, 161, -155
   LOAD_ONE message59, lb[s9], $a5, $t3, -98, 102
   LOAD_ONE message60, lb[s9], $s2, $s2, -24, 24

# lbu[u12]
   LOAD_ONE message61, lbu[u12], $a4, $t2, 1487, -1485
   LOAD_ONE message62, lbu[u12], $s3, $s1, 1862, -1858
   LOAD_ONE message63, lbu[u12], $s0, $a2, 6, 0
   LOAD_ONE message64, lbu[u12], $a4, $a3, 1560, -1553
   LOAD_ONE message65, lbu[u12], $s2, $s6, 279, -273
   LOAD_ONE message66, lbu[u12], $a7, $s1, 1868, -1864
   LOAD_ONE message67, lbu[u12], $s0, $a2, 1216, -1214
   LOAD_ONE message68, lbu[u12], $s4, $a4, 322, -321
   LOAD_ONE message69, lbu[u12], $a3, $t2, 3587, -3585
   LOAD_ONE message70, lbu[u12], $t3, $s5, 2382, -2375
   LOAD_ONE message71, lbu[u12], $t0, $s1, 3453, -3449
   LOAD_ONE message72, lbu[u12], $a4, $t2, 1387, -1380
   LOAD_ONE message73, lbu[u12], $s4, $a7, 165, -162
   LOAD_ONE message74, lbu[u12], $t0, $a6, 2207, -2203
   LOAD_ONE message75, lbu[u12], $s0, $s0, 2390, -2389

# lbu[16]
   LOAD_ONE message76, lbu[16], $a0, $a0, 2, 5
   LOAD_ONE message77, lbu[16], $a2, $s1, 2, 3
   LOAD_ONE message78, lbu[16], $s2, $a0, 1, 1
   LOAD_ONE message79, lbu[16], $s1, $a2, 3, 3
   LOAD_ONE message80, lbu[16], $a0, $a2, 1, 3
   LOAD_ONE message81, lbu[16], $s1, $a2, 3, -2
   LOAD_ONE message82, lbu[16], $s0, $s3, 1, 0
   LOAD_ONE message83, lbu[16], $s3, $a3, 2, 5
   LOAD_ONE message84, lbu[16], $s0, $s0, 3, 3
   LOAD_ONE message85, lbu[16], $a1, $a1, 0, 7
   LOAD_ONE message86, lbu[16], $a1, $a1, 2, 5
   LOAD_ONE message87, lbu[16], $a3, $a1, 1, 3
   LOAD_ONE message88, lbu[16], $a3, $a2, 0, 1
   LOAD_ONE message89, lbu[16], $s0, $a0, 2, -1
   LOAD_ONE message90, lbu[16], $s3, $a0, 2, 4

# lbu[gp]
   LOAD_ONE_SPECIAL message91, lbu[gp], $gp, $s7, 252116, -252109
   LOAD_ONE_SPECIAL message92, lbu[gp], $gp, $a4, 116648, -116647
   LOAD_ONE_SPECIAL message93, lbu[gp], $gp, $a1, 102080, -102073
   LOAD_ONE_SPECIAL message94, lbu[gp], $gp, $s7, 192460, -192454
   LOAD_ONE_SPECIAL message95, lbu[gp], $gp, $a7, 258302, -258295
   LOAD_ONE_SPECIAL message96, lbu[gp], $gp, $a5, 238568, -238567
   LOAD_ONE_SPECIAL message97, lbu[gp], $gp, $t3, 167780, -167778
   LOAD_ONE_SPECIAL message98, lbu[gp], $gp, $a6, 96928, -96922
   LOAD_ONE_SPECIAL message99, lbu[gp], $gp, $s0, 40041, -40041
   LOAD_ONE_SPECIAL message100, lbu[gp], $gp, $t2, 9963, -9956
   LOAD_ONE_SPECIAL message101, lbu[gp], $gp, $a1, 175804, -175804
   LOAD_ONE_SPECIAL message102, lbu[gp], $gp, $a0, 76212, -76212
   LOAD_ONE_SPECIAL message103, lbu[gp], $gp, $t3, 44015, -44011
   LOAD_ONE_SPECIAL message104, lbu[gp], $gp, $a4, 73858, -73851
   LOAD_ONE_SPECIAL message105, lbu[gp], $gp, $s1, 34784, -34780

# lbu[s9]
   LOAD_ONE message106, lbu[s9], $a5, $s1, 226, -221
   LOAD_ONE message107, lbu[s9], $t3, $a3, -106, 107
   LOAD_ONE message108, lbu[s9], $t3, $t3, -136, 136
   LOAD_ONE message109, lbu[s9], $a5, $a7, -191, 197
   LOAD_ONE message110, lbu[s9], $a7, $s4, -114, 117
   LOAD_ONE message111, lbu[s9], $s5, $s1, -9, 15
   LOAD_ONE message112, lbu[s9], $s0, $a1, 21, -16
   LOAD_ONE message113, lbu[s9], $a2, $t3, 113, -110
   LOAD_ONE message114, lbu[s9], $s0, $a7, -192, 199
   LOAD_ONE message115, lbu[s9], $s3, $a2, -232, 238
   LOAD_ONE message116, lbu[s9], $s6, $t1, -34, 41
   LOAD_ONE message117, lbu[s9], $t0, $a3, -99, 102
   LOAD_ONE message118, lbu[s9], $a7, $a7, -178, 179
   LOAD_ONE message119, lbu[s9], $t0, $s2, 87, -83
   LOAD_ONE message120, lbu[s9], $s3, $a0, -70, 71

# lbux
   LOAD_ONE_INDEXED message121, lbux, $s3, $a3, $a0, -889890988, 889890991
   LOAD_ONE_INDEXED message122, lbux, $s6, $a7, $t0, 448736016, -448736010
   LOAD_ONE_INDEXED message123, lbux, $s5, $a1, $a6, -603183184, 603183186
   LOAD_ONE_INDEXED message124, lbux, $s3, $a6, $s2, 2047843929, -2047843929
   LOAD_ONE_INDEXED message125, lbux, $s6, $t1, $a6, -1809456980, 1809456981
   LOAD_ONE_INDEXED message126, lbux, $s7, $s3, $s1, -1834948869, 1834948874
   LOAD_ONE_INDEXED message127, lbux, $a4, $a2, $s4, -1701947355, 1701947355
   LOAD_ONE_INDEXED message128, lbux, $s3, $a3, $a6, -702101932, 702101939
   LOAD_ONE_INDEXED message129, lbux, $s7, $s7, $t0, -174660532, 174660535
   LOAD_ONE_INDEXED message130, lbux, $t1, $s4, $t0, -1788914064, 1788914066
   LOAD_ONE_INDEXED message131, lbux, $a7, $a2, $s7, 1397035608, -1397035603
   LOAD_ONE_INDEXED message132, lbux, $s3, $a5, $a3, 1667537883, -1667537882
   LOAD_ONE_INDEXED message133, lbux, $t3, $s5, $t2, -473198375, 473198378
   LOAD_ONE_INDEXED message134, lbux, $a7, $a7, $s0, -1334036079, 1334036086
   LOAD_ONE_INDEXED message135, lbux, $s5, $t3, $a2, -1070998134, 1070998134
   LOAD_ONE_INDEXED message136, lbux, $a2, $s7, $a7, -973969496, 973969496
   LOAD_ONE_INDEXED message137, lbux, $s7, $s4, $a3, 5756886, -5756885
   LOAD_ONE_INDEXED message138, lbux, $s2, $s6, $a5, -326176395, 326176399
   LOAD_ONE_INDEXED message139, lbux, $s0, $t2, $s4, 117404247, -117404242
   LOAD_ONE_INDEXED message140, lbux, $s4, $a0, $s5, 412386958, -412386957
   LOAD_ONE_INDEXED message141, lbux, $t3, $s7, $a6, -740462742, 740462743
   LOAD_ONE_INDEXED message142, lbux, $s0, $a3, $a2, -1115603238, 1115603241
   LOAD_ONE_INDEXED message143, lbux, $s2, $a5, $s3, 499929505, -499929505
   LOAD_ONE_INDEXED message144, lbux, $s5, $s2, $a1, 986827130, -986827123
   LOAD_ONE_INDEXED message145, lbux, $s2, $t3, $t0, -456599814, 456599818

# lbx
   LOAD_ONE_INDEXED message146, lbx, $t1, $a4, $t1, 1212928687, -1212928684
   LOAD_ONE_INDEXED message147, lbx, $s1, $s3, $s2, 1106623400, -1106623398
   LOAD_ONE_INDEXED message148, lbx, $s3, $s1, $s7, 805607772, -805607767
   LOAD_ONE_INDEXED message149, lbx, $a1, $s7, $t0, -247617974, 247617979
   LOAD_ONE_INDEXED message150, lbx, $a4, $t3, $s1, 1998663233, -1998663231
   LOAD_ONE_INDEXED message151, lbx, $s7, $s1, $s5, 947603248, -947603245
   LOAD_ONE_INDEXED message152, lbx, $s0, $s0, $s2, 7671704, -7671700
   LOAD_ONE_INDEXED message153, lbx, $s0, $s5, $s6, -1074890744, 1074890749
   LOAD_ONE_INDEXED message154, lbx, $a6, $s7, $s2, -1482043606, 1482043613
   LOAD_ONE_INDEXED message155, lbx, $a2, $t1, $a1, 342844121, -342844116
   LOAD_ONE_INDEXED message156, lbx, $t3, $s1, $s5, 821849392, -821849392
   LOAD_ONE_INDEXED message157, lbx, $s7, $s0, $s4, 910590088, -910590082
   LOAD_ONE_INDEXED message158, lbx, $a2, $t1, $a3, 935855104, -935855097
   LOAD_ONE_INDEXED message159, lbx, $s3, $a5, $s6, 101043846, -101043839
   LOAD_ONE_INDEXED message160, lbx, $s6, $s2, $a1, -377157448, 377157453
   LOAD_ONE_INDEXED message161, lbx, $s2, $s1, $s2, -1324616057, 1324616062
   LOAD_ONE_INDEXED message162, lbx, $a0, $s3, $a4, -1949314325, 1949314331
   LOAD_ONE_INDEXED message163, lbx, $s5, $t0, $a5, 637279836, -637279835
   LOAD_ONE_INDEXED message164, lbx, $a6, $t3, $a3, 2015198588, -2015198587
   LOAD_ONE_INDEXED message165, lbx, $a6, $t2, $a3, -861280020, 861280024
   LOAD_ONE_INDEXED message166, lbx, $a2, $s6, $a6, 2112295222, -2112295218
   LOAD_ONE_INDEXED message167, lbx, $t0, $t0, $a6, 1101483488, -1101483484
   LOAD_ONE_INDEXED message168, lbx, $a6, $a7, $s5, -483558942, 483558948
   LOAD_ONE_INDEXED message169, lbx, $t2, $s2, $a7, -347290944, 347290951
   LOAD_ONE_INDEXED message170, lbx, $a0, $s2, $s7, 1562598800, -1562598799

# lh[u12]
   LOAD_ONE message171, lh[u12], $s6, $a4, 557, -554
   LOAD_ONE message172, lh[u12], $s4, $t3, 3960, -3956
   LOAD_ONE message173, lh[u12], $a2, $a0, 2931, -2927
   LOAD_ONE message174, lh[u12], $s5, $s0, 743, -737
   LOAD_ONE message175, lh[u12], $s6, $a5, 3692, -3690
   LOAD_ONE message176, lh[u12], $a1, $a5, 3887, -3886
   LOAD_ONE message177, lh[u12], $a5, $s5, 2247, -2241
   LOAD_ONE message178, lh[u12], $s2, $a1, 1077, -1072
   LOAD_ONE message179, lh[u12], $t1, $s2, 3030, -3024
   LOAD_ONE message180, lh[u12], $a1, $s6, 1626, -1622
   LOAD_ONE message181, lh[u12], $s6, $s5, 2633, -2628
   LOAD_ONE message182, lh[u12], $s5, $t0, 3366, -3365
   LOAD_ONE message183, lh[u12], $a2, $a7, 272, -266
   LOAD_ONE message184, lh[u12], $s4, $s3, 1632, -1632
   LOAD_ONE message185, lh[u12], $a5, $a7, 45, -41

# lh[16]
   LOAD_ONE message186, lh[16], $s0, $a2, 6, -3
   LOAD_ONE message187, lh[16], $s0, $s1, 0, 1
   LOAD_ONE message188, lh[16], $s0, $a2, 0, 1
   LOAD_ONE message189, lh[16], $a0, $a2, 2, 2
   LOAD_ONE message190, lh[16], $a2, $a1, 0, 5
   LOAD_ONE message191, lh[16], $s0, $a1, 4, 0
   LOAD_ONE message192, lh[16], $a1, $s0, 4, 1
   LOAD_ONE message193, lh[16], $a3, $a3, 4, 2
   LOAD_ONE message194, lh[16], $s1, $s0, 6, -1
   LOAD_ONE message195, lh[16], $a1, $s2, 6, -3
   LOAD_ONE message196, lh[16], $a0, $a0, 4, -2
   LOAD_ONE message197, lh[16], $a2, $s2, 6, -5
   LOAD_ONE message198, lh[16], $s3, $s3, 4, 2
   LOAD_ONE message199, lh[16], $a0, $a1, 6, -3
   LOAD_ONE message200, lh[16], $s1, $s1, 2, 1

# lh[gp]
   LOAD_ONE_SPECIAL message201, lh[gp], $gp, $a4, 252712, -252712
   LOAD_ONE_SPECIAL message202, lh[gp], $gp, $s4, 260424, -260422
   LOAD_ONE_SPECIAL message203, lh[gp], $gp, $a1, 148444, -148444
   LOAD_ONE_SPECIAL message204, lh[gp], $gp, $a7, 31802, -31801
   LOAD_ONE_SPECIAL message205, lh[gp], $gp, $t2, 225268, -225266
   LOAD_ONE_SPECIAL message206, lh[gp], $gp, $s3, 128460, -128460
   LOAD_ONE_SPECIAL message207, lh[gp], $gp, $s2, 16464, -16462
   LOAD_ONE_SPECIAL message208, lh[gp], $gp, $s5, 11552, -11549
   LOAD_ONE_SPECIAL message209, lh[gp], $gp, $a5, 151326, -151325
   LOAD_ONE_SPECIAL message210, lh[gp], $gp, $s0, 75270, -75268
   LOAD_ONE_SPECIAL message211, lh[gp], $gp, $a1, 183456, -183452
   LOAD_ONE_SPECIAL message212, lh[gp], $gp, $s6, 52958, -52957
   LOAD_ONE_SPECIAL message213, lh[gp], $gp, $a1, 92644, -92640
   LOAD_ONE_SPECIAL message214, lh[gp], $gp, $a6, 2756, -2756
   LOAD_ONE_SPECIAL message215, lh[gp], $gp, $a3, 140512, -140511

# lh[s9]
   LOAD_ONE message216, lh[s9], $a1, $s4, 206, -202
   LOAD_ONE message217, lh[s9], $t2, $a5, -125, 131
   LOAD_ONE message218, lh[s9], $s0, $t3, -32, 33
   LOAD_ONE message219, lh[s9], $t2, $t3, -218, 222
   LOAD_ONE message220, lh[s9], $s0, $s5, 73, -71
   LOAD_ONE message221, lh[s9], $t3, $a7, 233, -230
   LOAD_ONE message222, lh[s9], $t2, $a4, 142, -139
   LOAD_ONE message223, lh[s9], $a1, $t2, -150, 150
   LOAD_ONE message224, lh[s9], $a3, $a1, -248, 249
   LOAD_ONE message225, lh[s9], $t2, $t3, 195, -189
   LOAD_ONE message226, lh[s9], $t2, $s3, 196, -193
   LOAD_ONE message227, lh[s9], $s2, $a6, 101, -101
   LOAD_ONE message228, lh[s9], $s4, $s2, -21, 23
   LOAD_ONE message229, lh[s9], $s1, $a4, 89, -85
   LOAD_ONE message230, lh[s9], $a1, $t0, 96, -91

# lhu[u12]
   LOAD_ONE message231, lhu[u12], $a6, $s3, 1732, -1727
   LOAD_ONE message232, lhu[u12], $s4, $t0, 3446, -3444
   LOAD_ONE message233, lhu[u12], $s7, $s6, 4077, -4071
   LOAD_ONE message234, lhu[u12], $t1, $t1, 3802, -3797
   LOAD_ONE message235, lhu[u12], $s7, $a7, 2086, -2083
   LOAD_ONE message236, lhu[u12], $t1, $s2, 2367, -2363
   LOAD_ONE message237, lhu[u12], $a6, $s4, 1861, -1857
   LOAD_ONE message238, lhu[u12], $s0, $s6, 29, -23
   LOAD_ONE message239, lhu[u12], $t1, $a5, 3984, -3982
   LOAD_ONE message240, lhu[u12], $s5, $s3, 1955, -1951
   LOAD_ONE message241, lhu[u12], $t1, $s4, 767, -764
   LOAD_ONE message242, lhu[u12], $s1, $a1, 1910, -1907
   LOAD_ONE message243, lhu[u12], $a1, $a5, 532, -527
   LOAD_ONE message244, lhu[u12], $a7, $s7, 389, -387
   LOAD_ONE message245, lhu[u12], $s0, $s6, 705, -701

# lhu[16]
   LOAD_ONE message246, lhu[16], $a0, $s2, 2, 4
   LOAD_ONE message247, lhu[16], $a0, $s0, 6, -1
   LOAD_ONE message248, lhu[16], $a3, $a2, 2, -2
   LOAD_ONE message249, lhu[16], $a2, $a1, 2, 4
   LOAD_ONE message250, lhu[16], $a3, $a1, 0, 6
   LOAD_ONE message251, lhu[16], $s2, $s1, 2, -2
   LOAD_ONE message252, lhu[16], $a1, $s3, 2, 3
   LOAD_ONE message253, lhu[16], $a2, $a3, 0, 2
   LOAD_ONE message254, lhu[16], $a2, $s1, 4, -3
   LOAD_ONE message255, lhu[16], $a1, $a2, 6, -2
   LOAD_ONE message256, lhu[16], $s0, $a2, 0, 6
   LOAD_ONE message257, lhu[16], $a3, $a2, 2, 0
   LOAD_ONE message258, lhu[16], $a3, $s0, 2, 0
   LOAD_ONE message259, lhu[16], $a1, $s1, 6, -5
   LOAD_ONE message260, lhu[16], $s0, $a1, 6, -2

# lhu[gp]
   LOAD_ONE_SPECIAL message261, lhu[gp], $gp, $s4, 102312, -102310
   LOAD_ONE_SPECIAL message262, lhu[gp], $gp, $t2, 142072, -142072
   LOAD_ONE_SPECIAL message263, lhu[gp], $gp, $a3, 288, -286
   LOAD_ONE_SPECIAL message264, lhu[gp], $gp, $t2, 64960, -64955
   LOAD_ONE_SPECIAL message265, lhu[gp], $gp, $s1, 66440, -66438
   LOAD_ONE_SPECIAL message266, lhu[gp], $gp, $s2, 123644, -123641
   LOAD_ONE_SPECIAL message267, lhu[gp], $gp, $a7, 238158, -238155
   LOAD_ONE_SPECIAL message268, lhu[gp], $gp, $t2, 60406, -60404
   LOAD_ONE_SPECIAL message269, lhu[gp], $gp, $t2, 244354, -244353
   LOAD_ONE_SPECIAL message270, lhu[gp], $gp, $s4, 100438, -100433
   LOAD_ONE_SPECIAL message271, lhu[gp], $gp, $t1, 230832, -230828
   LOAD_ONE_SPECIAL message272, lhu[gp], $gp, $s5, 212118, -212115
   LOAD_ONE_SPECIAL message273, lhu[gp], $gp, $s5, 85040, -85040
   LOAD_ONE_SPECIAL message274, lhu[gp], $gp, $a3, 185772, -185771
   LOAD_ONE_SPECIAL message275, lhu[gp], $gp, $s3, 187508, -187506

# lhu[s9]
   LOAD_ONE message276, lhu[s9], $a6, $s1, -36, 37
   LOAD_ONE message277, lhu[s9], $a5, $s1, 245, -241
   LOAD_ONE message278, lhu[s9], $s6, $a4, 161, -157
   LOAD_ONE message279, lhu[s9], $t3, $s5, -45, 49
   LOAD_ONE message280, lhu[s9], $t0, $t0, -249, 255
   LOAD_ONE message281, lhu[s9], $t1, $s7, -136, 138
   LOAD_ONE message282, lhu[s9], $a6, $s3, 68, -63
   LOAD_ONE message283, lhu[s9], $a7, $s3, -37, 40
   LOAD_ONE message284, lhu[s9], $s4, $s3, 46, -45
   LOAD_ONE message285, lhu[s9], $a0, $a3, 215, -214
   LOAD_ONE message286, lhu[s9], $a7, $s0, 50, -49
   LOAD_ONE message287, lhu[s9], $s6, $t2, -5, 5
   LOAD_ONE message288, lhu[s9], $a6, $s2, 44, -44
   LOAD_ONE message289, lhu[s9], $t2, $s4, -103, 109
   LOAD_ONE message290, lhu[s9], $s7, $s5, -239, 245

# lhux
   LOAD_ONE_INDEXED message291, lhux, $a4, $s4, $a0, 654528500, -654528500
   LOAD_ONE_INDEXED message292, lhux, $a7, $a5, $t1, -686463982, 686463987
   LOAD_ONE_INDEXED message293, lhux, $s7, $s3, $s2, -321234940, 321234946
   LOAD_ONE_INDEXED message294, lhux, $a2, $s5, $s3, 706252812, -706252807
   LOAD_ONE_INDEXED message295, lhux, $a7, $a0, $s1, -2058105489, 2058105493
   LOAD_ONE_INDEXED message296, lhux, $a0, $s4, $s1, -2061432420, 2061432426
   LOAD_ONE_INDEXED message297, lhux, $a6, $t0, $a6, 174228966, -174228961
   LOAD_ONE_INDEXED message298, lhux, $s1, $a6, $s5, 1891267847, -1891267841
   LOAD_ONE_INDEXED message299, lhux, $a5, $s3, $t0, -444379808, 444379812
   LOAD_ONE_INDEXED message300, lhux, $s1, $a6, $a0, -1477313998, 1477314003
   LOAD_ONE_INDEXED message301, lhux, $a6, $a3, $a6, -1022790268, 1022790271
   LOAD_ONE_INDEXED message302, lhux, $t3, $s5, $s3, 1371180774, -1371180769
   LOAD_ONE_INDEXED message303, lhux, $a3, $s1, $a5, -1308732034, 1308732039
   LOAD_ONE_INDEXED message304, lhux, $a2, $a4, $s3, -1977290648, 1977290650
   LOAD_ONE_INDEXED message305, lhux, $t2, $s6, $a3, 2123376358, -2123376352
   LOAD_ONE_INDEXED message306, lhux, $a0, $a2, $a1, 556796589, -556796586
   LOAD_ONE_INDEXED message307, lhux, $s5, $s4, $s0, -452824132, 452824138
   LOAD_ONE_INDEXED message308, lhux, $s2, $a2, $t1, -734918056, 734918056
   LOAD_ONE_INDEXED message309, lhux, $s0, $s6, $s7, 472633216, -472633215
   LOAD_ONE_INDEXED message310, lhux, $t2, $s4, $s7, 118985060, -118985054
   LOAD_ONE_INDEXED message311, lhux, $a1, $a6, $a1, -1677466044, 1677466049
   LOAD_ONE_INDEXED message312, lhux, $a3, $a6, $s2, -818907804, 818907810
   LOAD_ONE_INDEXED message313, lhux, $a6, $a0, $a1, -1202797778, 1202797780
   LOAD_ONE_INDEXED message314, lhux, $s7, $t3, $a2, -18117101, 18117104
   LOAD_ONE_INDEXED message315, lhux, $a5, $t2, $s1, 1467127064, -1467127060

# lhuxs
   LOAD_ONE_INDEXED message316, lhuxs, $s6, $a4, $s0, -1072335466, 2144670934
   LOAD_ONE_INDEXED message317, lhuxs, $s3, $a1, $t3, 44736666, -89473330
   LOAD_ONE_INDEXED message318, lhuxs, $s0, $s2, $s3, 133674173, -267348343
   LOAD_ONE_INDEXED message319, lhuxs, $a1, $a2, $a1, 656690193, -1313380381
   LOAD_ONE_INDEXED message320, lhuxs, $t1, $a5, $s2, 788342872, -1576685738
   LOAD_ONE_INDEXED message321, lhuxs, $s5, $a0, $a4, 985818210, -1971636418
   LOAD_ONE_INDEXED message322, lhuxs, $s7, $a4, $a7, -656671444, 1313342891
   LOAD_ONE_INDEXED message323, lhuxs, $a4, $a3, $s0, 684598409, -1369196816
   LOAD_ONE_INDEXED message324, lhuxs, $t3, $a4, $a3, 105658004, -211316008
   LOAD_ONE_INDEXED message325, lhuxs, $t0, $a2, $t0, -111340188, 222680378
   LOAD_ONE_INDEXED message326, lhuxs, $a1, $s7, $s1, 824156087, -1648312169
   LOAD_ONE_INDEXED message327, lhuxs, $a5, $s5, $s0, 341678488, -683356974
   LOAD_ONE_INDEXED message328, lhuxs, $a3, $a5, $a6, 339945582, -679891161
   LOAD_ONE_INDEXED message329, lhuxs, $t0, $s7, $t0, 995742750, -1991485496
   LOAD_ONE_INDEXED message330, lhuxs, $a7, $a5, $a7, -323634896, 647269793
   LOAD_ONE_INDEXED message331, lhuxs, $s0, $a6, $s3, 951034707, -1902069411
   LOAD_ONE_INDEXED message332, lhuxs, $a4, $s1, $a1, 630364265, -1260728525
   LOAD_ONE_INDEXED message333, lhuxs, $s7, $a2, $s3, -484640785, 969281574
   LOAD_ONE_INDEXED message334, lhuxs, $a6, $s3, $a1, -551719541, 1103439083
   LOAD_ONE_INDEXED message335, lhuxs, $s2, $s0, $s4, -723716982, 1447433967
   LOAD_ONE_INDEXED message336, lhuxs, $t1, $a6, $a2, -295377805, 590755610
   LOAD_ONE_INDEXED message337, lhuxs, $s1, $s7, $s4, -822001180, 1644002363
   LOAD_ONE_INDEXED message338, lhuxs, $t2, $a2, $t0, -956681624, 1913363249
   LOAD_ONE_INDEXED message339, lhuxs, $s2, $a1, $a6, -759445000, 1518890000
   LOAD_ONE_INDEXED message340, lhuxs, $t0, $s5, $s2, 1033401736, -2066803467

# lhx
   LOAD_ONE_INDEXED message341, lhx, $s7, $s3, $a1, 1645284674, -1645284671
   LOAD_ONE_INDEXED message342, lhx, $a7, $s0, $s6, -2025043786, 2025043792
   LOAD_ONE_INDEXED message343, lhx, $a4, $s4, $a6, 1589467140, -1589467138
   LOAD_ONE_INDEXED message344, lhx, $t0, $s3, $s5, 841647848, -841647847
   LOAD_ONE_INDEXED message345, lhx, $t1, $t3, $a2, 113753248, -113753248
   LOAD_ONE_INDEXED message346, lhx, $s1, $s1, $t3, 1975594398, -1975594392
   LOAD_ONE_INDEXED message347, lhx, $a0, $s1, $s4, -507222120, 507222123
   LOAD_ONE_INDEXED message348, lhx, $a7, $s3, $a6, 1948779837, -1948779835
   LOAD_ONE_INDEXED message349, lhx, $s4, $t3, $a2, -1962403782, 1962403784
   LOAD_ONE_INDEXED message350, lhx, $s5, $a1, $s3, -879319794, 879319795
   LOAD_ONE_INDEXED message351, lhx, $s4, $t0, $a2, -704505057, 704505059
   LOAD_ONE_INDEXED message352, lhx, $t3, $s5, $s0, -590509000, 590509005
   LOAD_ONE_INDEXED message353, lhx, $s3, $t2, $s6, -2131090755, 2131090761
   LOAD_ONE_INDEXED message354, lhx, $t3, $s5, $s6, -2089182202, 2089182205
   LOAD_ONE_INDEXED message355, lhx, $s1, $t3, $a4, 851281816, -851281814
   LOAD_ONE_INDEXED message356, lhx, $s5, $a1, $a0, -2114812424, 2114812429
   LOAD_ONE_INDEXED message357, lhx, $s2, $a7, $a5, -1057393146, 1057393146
   LOAD_ONE_INDEXED message358, lhx, $a6, $s3, $t2, 1691153635, -1691153629
   LOAD_ONE_INDEXED message359, lhx, $a3, $a1, $s2, -1226810384, 1226810386
   LOAD_ONE_INDEXED message360, lhx, $t0, $a1, $s4, 535625244, -535625239
   LOAD_ONE_INDEXED message361, lhx, $s0, $a5, $t1, -131196224, 131196226
   LOAD_ONE_INDEXED message362, lhx, $s5, $a5, $s6, -1821462231, 1821462231
   LOAD_ONE_INDEXED message363, lhx, $s6, $t0, $s3, -1399877727, 1399877733
   LOAD_ONE_INDEXED message364, lhx, $s1, $s0, $t0, -1715178930, 1715178936
   LOAD_ONE_INDEXED message365, lhx, $a7, $s5, $s6, 162878610, -162878604

# lhxs
   LOAD_ONE_INDEXED message366, lhxs, $a6, $a6, $s6, 910280620, -1820561240
   LOAD_ONE_INDEXED message367, lhxs, $s5, $a4, $s2, 24866152, -49732299
   LOAD_ONE_INDEXED message368, lhxs, $a1, $a7, $t2, 1037916655, -2075833304
   LOAD_ONE_INDEXED message369, lhxs, $a0, $a7, $a0, -267134206, 534268412
   LOAD_ONE_INDEXED message370, lhxs, $a2, $s3, $a3, -93144608, 186289219
   LOAD_ONE_INDEXED message371, lhxs, $a5, $s1, $a1, -308089315, 616178634
   LOAD_ONE_INDEXED message372, lhxs, $t1, $t1, $t2, -1071346460, 2142692925
   LOAD_ONE_INDEXED message373, lhxs, $s3, $s7, $s5, -34389579, 68779164
   LOAD_ONE_INDEXED message374, lhxs, $a6, $s5, $t0, -33386912, 66773830
   LOAD_ONE_INDEXED message375, lhxs, $t1, $s1, $t3, -596018741, 1192037485
   LOAD_ONE_INDEXED message376, lhxs, $t0, $s7, $a3, -735400890, 1470801786
   LOAD_ONE_INDEXED message377, lhxs, $t0, $s1, $a5, 307384596, -614769188
   LOAD_ONE_INDEXED message378, lhxs, $s2, $a4, $s4, 100138468, -200276934
   LOAD_ONE_INDEXED message379, lhxs, $a5, $s0, $t3, 68352297, -136704589
   LOAD_ONE_INDEXED message380, lhxs, $s3, $s3, $s7, -592008519, 1184017041
   LOAD_ONE_INDEXED message381, lhxs, $a7, $a5, $s0, -57986879, 115973761
   LOAD_ONE_INDEXED message382, lhxs, $a0, $s2, $a0, 821877743, -1643755485
   LOAD_ONE_INDEXED message383, lhxs, $s3, $t1, $a4, 703274845, -1406549684
   LOAD_ONE_INDEXED message384, lhxs, $t1, $s1, $s3, -944357408, 1888714822
   LOAD_ONE_INDEXED message385, lhxs, $t3, $a0, $s3, -424881185, 849762376
   LOAD_ONE_INDEXED message386, lhxs, $a7, $s2, $t1, 276356114, -552712223
   LOAD_ONE_INDEXED message387, lhxs, $a7, $t1, $s7, 823106538, -1646213072
   LOAD_ONE_INDEXED message388, lhxs, $a4, $s2, $s5, -99813810, 199627626
   LOAD_ONE_INDEXED message389, lhxs, $a3, $s7, $s5, -688493963, 1376987926
   LOAD_ONE_INDEXED message390, lhxs, $s4, $t3, $s4, -967088517, 1934177040

# lw[u12]
   LOAD_ONE message391, lw[u12], $a6, $s2, 1991, -1991
   LOAD_ONE message392, lw[u12], $s0, $a2, 2471, -2467
   LOAD_ONE message393, lw[u12], $s5, $s0, 3226, -3223
   LOAD_ONE message394, lw[u12], $s5, $s3, 154, -150
   LOAD_ONE message395, lw[u12], $t1, $s2, 3717, -3715
   LOAD_ONE message396, lw[u12], $a4, $t0, 2522, -2520
   LOAD_ONE message397, lw[u12], $a1, $s1, 2154, -2152
   LOAD_ONE message398, lw[u12], $a2, $a3, 3411, -3411
   LOAD_ONE message399, lw[u12], $s6, $s3, 2493, -2489
   LOAD_ONE message400, lw[u12], $s5, $a0, 2100, -2098
   LOAD_ONE message401, lw[u12], $a0, $t3, 2318, -2315
   LOAD_ONE message402, lw[u12], $a2, $s1, 2679, -2675
   LOAD_ONE message403, lw[u12], $s3, $s4, 41, -41
   LOAD_ONE message404, lw[u12], $s0, $s3, 2061, -2057
   LOAD_ONE message405, lw[u12], $s4, $a7, 2422, -2420

# lw[16]
   LOAD_ONE message406, lw[16], $s2, $a1, 4, -3
   LOAD_ONE message407, lw[16], $s0, $s1, 32, -30
   LOAD_ONE message408, lw[16], $a1, $s3, 52, -51
   LOAD_ONE message409, lw[16], $s2, $a3, 56, -52
   LOAD_ONE message410, lw[16], $s0, $a2, 40, -40
   LOAD_ONE message411, lw[16], $s2, $a1, 52, -48
   LOAD_ONE message412, lw[16], $s1, $s0, 0, 0
   LOAD_ONE message413, lw[16], $a3, $s2, 40, -36
   LOAD_ONE message414, lw[16], $a0, $a0, 20, -20
   LOAD_ONE message415, lw[16], $s1, $a0, 56, -54
   LOAD_ONE message416, lw[16], $s3, $s3, 48, -47
   LOAD_ONE message417, lw[16], $a3, $s0, 44, -41
   LOAD_ONE message418, lw[16], $s2, $s2, 28, -26
   LOAD_ONE message419, lw[16], $s3, $s2, 36, -32
   LOAD_ONE message420, lw[16], $a2, $s0, 0, 3

# lw[4x4]
   LOAD_ONE message421, lw[4x4], $s3, $s2, 12, -9
   LOAD_ONE message422, lw[4x4], $s2, $a4, 12, -9
   LOAD_ONE message423, lw[4x4], $s5, $s2, 12, -10
   LOAD_ONE message424, lw[4x4], $s1, $s6, 0, 3
   LOAD_ONE message425, lw[4x4], $s2, $a0, 12, -11
   LOAD_ONE message426, lw[4x4], $a6, $s3, 4, 0
   LOAD_ONE message427, lw[4x4], $a1, $s2, 8, -5
   LOAD_ONE message428, lw[4x4], $s6, $s4, 0, 4
   LOAD_ONE message429, lw[4x4], $a6, $s6, 4, -2
   LOAD_ONE message430, lw[4x4], $a3, $a4, 0, 1
   LOAD_ONE message431, lw[4x4], $s6, $s6, 12, -11
   LOAD_ONE message432, lw[4x4], $s5, $s6, 4, -3
   LOAD_ONE message433, lw[4x4], $s6, $a0, 0, 3
   LOAD_ONE message434, lw[4x4], $s3, $a6, 0, 0
   LOAD_ONE message435, lw[4x4], $a0, $s4, 4, -1

# lw[gp16]
   LOAD_ONE_SPECIAL message436, lw[gp16], $gp, $a0, 356, -354
   LOAD_ONE_SPECIAL message437, lw[gp16], $gp, $s2, 324, -320
   LOAD_ONE_SPECIAL message438, lw[gp16], $gp, $s3, 312, -311
   LOAD_ONE_SPECIAL message439, lw[gp16], $gp, $a0, 504, -504
   LOAD_ONE_SPECIAL message440, lw[gp16], $gp, $s2, 340, -339
   LOAD_ONE_SPECIAL message441, lw[gp16], $gp, $a3, 256, -253
   LOAD_ONE_SPECIAL message442, lw[gp16], $gp, $a3, 48, -46
   LOAD_ONE_SPECIAL message443, lw[gp16], $gp, $a3, 372, -369
   LOAD_ONE_SPECIAL message444, lw[gp16], $gp, $a2, 248, -248
   LOAD_ONE_SPECIAL message445, lw[gp16], $gp, $a2, 116, -116
   LOAD_ONE_SPECIAL message446, lw[gp16], $gp, $a1, 428, -428
   LOAD_ONE_SPECIAL message447, lw[gp16], $gp, $a2, 456, -455
   LOAD_ONE_SPECIAL message448, lw[gp16], $gp, $s3, 104, -101
   LOAD_ONE_SPECIAL message449, lw[gp16], $gp, $a0, 88, -84
   LOAD_ONE_SPECIAL message450, lw[gp16], $gp, $s3, 456, -456

# lw[gp]
   LOAD_ONE_SPECIAL message451, lw[gp], $gp, $t1, 354712, -354711
   LOAD_ONE_SPECIAL message452, lw[gp], $gp, $a3, 1822592, -1822588
   LOAD_ONE_SPECIAL message453, lw[gp], $gp, $t3, 521856, -521856
   LOAD_ONE_SPECIAL message454, lw[gp], $gp, $a1, 1223004, -1223002
   LOAD_ONE_SPECIAL message455, lw[gp], $gp, $a1, 152016, -152016
   LOAD_ONE_SPECIAL message456, lw[gp], $gp, $a7, 158700, -158696
   LOAD_ONE_SPECIAL message457, lw[gp], $gp, $t0, 1595072, -1595072
   LOAD_ONE_SPECIAL message458, lw[gp], $gp, $s1, 4276, -4273
   LOAD_ONE_SPECIAL message459, lw[gp], $gp, $t2, 1688504, -1688503
   LOAD_ONE_SPECIAL message460, lw[gp], $gp, $a0, 1868688, -1868687
   LOAD_ONE_SPECIAL message461, lw[gp], $gp, $s5, 934824, -934820
   LOAD_ONE_SPECIAL message462, lw[gp], $gp, $a5, 1945912, -1945911
   LOAD_ONE_SPECIAL message463, lw[gp], $gp, $s7, 1407980, -1407976
   LOAD_ONE_SPECIAL message464, lw[gp], $gp, $a0, 684520, -684517
   LOAD_ONE_SPECIAL message465, lw[gp], $gp, $a1, 384136, -384134

# lw[s9]
   LOAD_ONE message466, lw[s9], $a6, $s7, -172, 176
   LOAD_ONE message467, lw[s9], $s7, $a7, 198, -196
   LOAD_ONE message468, lw[s9], $a4, $s2, -78, 80
   LOAD_ONE message469, lw[s9], $t0, $a0, 159, -158
   LOAD_ONE message470, lw[s9], $a7, $a4, -241, 245
   LOAD_ONE message471, lw[s9], $s6, $a0, -28, 29
   LOAD_ONE message472, lw[s9], $s7, $s1, -73, 76
   LOAD_ONE message473, lw[s9], $s2, $t1, -162, 165
   LOAD_ONE message474, lw[s9], $a5, $a2, -199, 201
   LOAD_ONE message475, lw[s9], $s1, $a3, -101, 104
   LOAD_ONE message476, lw[s9], $a5, $a1, -122, 123
   LOAD_ONE message477, lw[s9], $a1, $t2, 178, -178
   LOAD_ONE message478, lw[s9], $s6, $a1, -18, 21
   LOAD_ONE message479, lw[s9], $s5, $a3, 234, -234
   LOAD_ONE message480, lw[s9], $a4, $s5, 124, -120

# lw[sp]
   LOAD_ONE_SPECIAL message481, lw[sp], $sp, $s3, 88, -84
   LOAD_ONE_SPECIAL message482, lw[sp], $sp, $t0, 12, -8
   LOAD_ONE_SPECIAL message483, lw[sp], $sp, $s0, 32, -30
   LOAD_ONE_SPECIAL message484, lw[sp], $sp, $t3, 84, -81
   LOAD_ONE_SPECIAL message485, lw[sp], $sp, $a0, 88, -87
   LOAD_ONE_SPECIAL message486, lw[sp], $sp, $s5, 0, 1
   LOAD_ONE_SPECIAL message487, lw[sp], $sp, $s6, 120, -118
   LOAD_ONE_SPECIAL message488, lw[sp], $sp, $a0, 36, -35
   LOAD_ONE_SPECIAL message489, lw[sp], $sp, $s7, 16, -15
   LOAD_ONE_SPECIAL message490, lw[sp], $sp, $t3, 48, -46
   LOAD_ONE_SPECIAL message491, lw[sp], $sp, $a5, 36, -32
   LOAD_ONE_SPECIAL message492, lw[sp], $sp, $a0, 48, -44
   LOAD_ONE_SPECIAL message493, lw[sp], $sp, $t3, 88, -84
   LOAD_ONE_SPECIAL message494, lw[sp], $sp, $s6, 28, -24
   LOAD_ONE_SPECIAL message495, lw[sp], $sp, $s6, 44, -41

# lwx
   LOAD_ONE_INDEXED message496, lwx, $s7, $s2, $a1, -1262298492, 1262298496
   LOAD_ONE_INDEXED message497, lwx, $t0, $s6, $s4, 1001646248, -1001646245
   LOAD_ONE_INDEXED message498, lwx, $s5, $a7, $s1, 1066803926, -1066803923
   LOAD_ONE_INDEXED message499, lwx, $s5, $s0, $s6, -2001926058, 2001926058
   LOAD_ONE_INDEXED message500, lwx, $s0, $a7, $s6, 1831537640, -1831537638
   LOAD_ONE_INDEXED message501, lwx, $a5, $s6, $t0, -62963148, 62963148
   LOAD_ONE_INDEXED message502, lwx, $a7, $t0, $s1, 729384569, -729384568
   LOAD_ONE_INDEXED message503, lwx, $t1, $t3, $a4, -695716325, 695716329
   LOAD_ONE_INDEXED message504, lwx, $s6, $s1, $a0, -1051753760, 1051753763
   LOAD_ONE_INDEXED message505, lwx, $a5, $a5, $a6, 1766774550, -1766774547
   LOAD_ONE_INDEXED message506, lwx, $s4, $s7, $a1, -1081126293, 1081126295
   LOAD_ONE_INDEXED message507, lwx, $a6, $s4, $s0, -56058416, 56058420
   LOAD_ONE_INDEXED message508, lwx, $a2, $a3, $a5, -1326612064, 1326612065
   LOAD_ONE_INDEXED message509, lwx, $a0, $a3, $s1, -32329192, 32329194
   LOAD_ONE_INDEXED message510, lwx, $a0, $a2, $s3, -150163436, 150163437
   LOAD_ONE_INDEXED message511, lwx, $a0, $a6, $t0, 1908722036, -1908722032
   LOAD_ONE_INDEXED message512, lwx, $s1, $s1, $a5, 1760694703, -1760694703
   LOAD_ONE_INDEXED message513, lwx, $s1, $s0, $s3, -734290784, 734290786
   LOAD_ONE_INDEXED message514, lwx, $s4, $a0, $a7, -934062392, 934062396
   LOAD_ONE_INDEXED message515, lwx, $s3, $s6, $s0, 1969292716, -1969292715
   LOAD_ONE_INDEXED message516, lwx, $t2, $a0, $s0, 348468254, -348468254
   LOAD_ONE_INDEXED message517, lwx, $s7, $t0, $a2, -724511640, 724511641
   LOAD_ONE_INDEXED message518, lwx, $s1, $a6, $s6, -1736815986, 1736815990
   LOAD_ONE_INDEXED message519, lwx, $a0, $s3, $s2, -378999095, 378999098
   LOAD_ONE_INDEXED message520, lwx, $t1, $s6, $s3, 653884433, -653884429

# lwxs[32]
   LOAD_ONE_INDEXED message521, lwxs[32], $a5, $a5, $t0, -109707623, 438830493
   LOAD_ONE_INDEXED message522, lwxs[32], $a6, $s4, $a3, -295291500, 1181166004
   LOAD_ONE_INDEXED message523, lwxs[32], $s0, $s2, $a3, -68061735, 272246940
   LOAD_ONE_INDEXED message524, lwxs[32], $t2, $a4, $a5, -365321637, 1461286550
   LOAD_ONE_INDEXED message525, lwxs[32], $s5, $t0, $s7, 274046107, -1096184428
   LOAD_ONE_INDEXED message526, lwxs[32], $a3, $a7, $s5, 393361672, -1573446685
   LOAD_ONE_INDEXED message527, lwxs[32], $t3, $s2, $t3, 33218196, -132872780
   LOAD_ONE_INDEXED message528, lwxs[32], $t2, $t3, $s4, 450020406, -1800081621
   LOAD_ONE_INDEXED message529, lwxs[32], $a1, $a1, $a6, 286804440, -1147217757
   LOAD_ONE_INDEXED message530, lwxs[32], $s5, $a7, $s2, 536026718, -2144106872
   LOAD_ONE_INDEXED message531, lwxs[32], $s0, $t1, $a3, -388558945, 1554235783
   LOAD_ONE_INDEXED message532, lwxs[32], $a4, $t2, $s6, 363944210, -1455776836
   LOAD_ONE_INDEXED message533, lwxs[32], $a1, $t1, $a2, 255521964, -1022087856
   LOAD_ONE_INDEXED message534, lwxs[32], $t3, $a5, $a0, 491360369, -1965441476
   LOAD_ONE_INDEXED message535, lwxs[32], $s3, $t3, $a7, -492807020, 1971228084
   LOAD_ONE_INDEXED message536, lwxs[32], $a7, $t1, $s0, 134615802, -538463206
   LOAD_ONE_INDEXED message537, lwxs[32], $a4, $a1, $t2, -87352785, 349411144
   LOAD_ONE_INDEXED message538, lwxs[32], $a1, $a1, $s3, 407819750, -1631278998
   LOAD_ONE_INDEXED message539, lwxs[32], $a5, $t3, $a3, 260213280, -1040853118
   LOAD_ONE_INDEXED message540, lwxs[32], $s1, $s6, $s3, 491300869, -1965203476
   LOAD_ONE_INDEXED message541, lwxs[32], $s1, $s3, $s1, 126438351, -505753402
   LOAD_ONE_INDEXED message542, lwxs[32], $a2, $a3, $a5, 267712272, -1070849086
   LOAD_ONE_INDEXED message543, lwxs[32], $t2, $s7, $s5, -478737599, 1914950400
   LOAD_ONE_INDEXED message544, lwxs[32], $s5, $s7, $a3, 11519183, -46076732
   LOAD_ONE_INDEXED message545, lwxs[32], $a4, $a0, $t1, 134524946, -538099784

# lwxs[16]
   LOAD_ONE_INDEXED message546, lwxs[16], $s2, $a1, $s0, -265499100, 1061996402
   LOAD_ONE_INDEXED message547, lwxs[16], $s0, $a3, $a2, -483193101, 1932772406
   LOAD_ONE_INDEXED message548, lwxs[16], $s2, $s0, $a2, 329968697, -1319874784
   LOAD_ONE_INDEXED message549, lwxs[16], $a1, $a0, $a2, 73279366, -293117464
   LOAD_ONE_INDEXED message550, lwxs[16], $s0, $s2, $a3, -117222797, 468891188
   LOAD_ONE_INDEXED message551, lwxs[16], $a3, $a0, $a3, -433441104, 1733764417
   LOAD_ONE_INDEXED message552, lwxs[16], $a3, $s0, $s1, 429960834, -1719843335
   LOAD_ONE_INDEXED message553, lwxs[16], $s0, $s1, $s2, 497781088, -1991124350
   LOAD_ONE_INDEXED message554, lwxs[16], $a1, $s1, $s3, 235725329, -942901316
   LOAD_ONE_INDEXED message555, lwxs[16], $a3, $a1, $s3, -29798356, 119193424
   LOAD_ONE_INDEXED message556, lwxs[16], $s1, $s3, $a1, 82899072, -331596284
   LOAD_ONE_INDEXED message557, lwxs[16], $s0, $s2, $a2, -216977686, 867910746
   LOAD_ONE_INDEXED message558, lwxs[16], $a0, $a1, $s1, 43254277, -173017104
   LOAD_ONE_INDEXED message559, lwxs[16], $a2, $s2, $a0, -230074003, 920296015
   LOAD_ONE_INDEXED message560, lwxs[16], $a1, $a2, $s2, -288984833, 1155939334
   LOAD_ONE_INDEXED message561, lwxs[16], $a2, $a0, $a2, 465122182, -1860488726
   LOAD_ONE_INDEXED message562, lwxs[16], $a3, $a1, $s1, 435549426, -1742197704
   LOAD_ONE_INDEXED message563, lwxs[16], $a3, $s3, $s1, -400451678, 1601806714
   LOAD_ONE_INDEXED message564, lwxs[16], $s3, $s1, $a2, -237578338, 950313356
   LOAD_ONE_INDEXED message565, lwxs[16], $a3, $s3, $a2, -95959018, 383836076
   LOAD_ONE_INDEXED message566, lwxs[16], $a2, $s3, $a3, -566956, 2267826
   LOAD_ONE_INDEXED message567, lwxs[16], $s3, $s3, $s1, 283663313, -1134653251
   LOAD_ONE_INDEXED message568, lwxs[16], $s1, $s0, $a1, -233427676, 933710705
   LOAD_ONE_INDEXED message569, lwxs[16], $s3, $a3, $s0, 220959901, -883839600
   LOAD_ONE_INDEXED message570, lwxs[16], $a1, $a0, $a1, 517544959, -2070179835

# sb[u12]
   STORE_ONE message571, sb[u12], $a7, $a4, 1160, -1158, 0xba
   STORE_ONE message572, sb[u12], $t0, $s7, 2118, -2113, 0x16
   STORE_ONE message573, sb[u12], $t2, $s5, 2553, -2552, 0x80
   STORE_ONE message574, sb[u12], $t0, $a6, 1710, -1709, 0x9f
   STORE_ONE message575, sb[u12], $a3, $s3, 3310, -3306, 0x68
   STORE_ONE message576, sb[u12], $s5, $t1, 2045, -2039, 0x96
   STORE_ONE message577, sb[u12], $s4, $s7, 3986, -3980, 0xc5
   STORE_ONE message578, sb[u12], $t1, $a3, 2090, -2088, 0x40
   STORE_ONE message579, sb[u12], $a6, $a1, 1522, -1522, 0xf2
   STORE_ONE message580, sb[u12], $a5, $s2, 2377, -2374, 0x10
   STORE_ONE message581, sb[u12], $a0, $a7, 1737, -1732, 0x49
   STORE_ONE message582, sb[u12], $s7, $t2, 2240, -2239, 0x2c
   STORE_ONE message583, sb[u12], $s3, $a5, 1093, -1092, 0xee
   STORE_ONE message584, sb[u12], $t2, $t1, 915, -911, 0x9c
   STORE_ONE message585, sb[u12], $a2, $a7, 2052, -2048, 0x2

# sb[16]
   STORE_ONE message586, sb[16], $r0, $a3, 2, 3, 0x0
   STORE_ONE message587, sb[16], $a2, $s3, 1, 2, 0x16
   STORE_ONE message588, sb[16], $r0, $a0, 3, -2, 0x0
   STORE_ONE message589, sb[16], $a3, $s1, 0, 7, 0xa0
   STORE_ONE message590, sb[16], $a2, $s2, 2, 4, 0x90
   STORE_ONE message591, sb[16], $a2, $s0, 2, -2, 0x38
   STORE_ONE message592, sb[16], $a2, $a3, 3, 2, 0x24
   STORE_ONE message593, sb[16], $a2, $s0, 1, 2, 0x0
   STORE_ONE message594, sb[16], $s3, $a1, 0, 6, 0x93
   STORE_ONE message595, sb[16], $a3, $s0, 1, 4, 0x24
   STORE_ONE message596, sb[16], $s2, $s1, 2, 2, 0x52
   STORE_ONE message597, sb[16], $s2, $s3, 1, 1, 0xa
   STORE_ONE message598, sb[16], $a3, $s3, 2, -2, 0xe8
   STORE_ONE message599, sb[16], $a0, $a1, 2, 3, 0x61
   STORE_ONE message600, sb[16], $a0, $a3, 3, -2, 0xb0

# sb[gp]
   STORE_ONE_SPECIAL message601, sb[gp], $gp, $a3, 121058, -121057, 0x68
   STORE_ONE_SPECIAL message602, sb[gp], $gp, $a3, 119583, -119583, 0x94
   STORE_ONE_SPECIAL message603, sb[gp], $gp, $s2, 260053, -260052, 0x72
   STORE_ONE_SPECIAL message604, sb[gp], $gp, $a1, 8584, -8581, 0xc3
   STORE_ONE_SPECIAL message605, sb[gp], $gp, $s3, 162114, -162108, 0xdc
   STORE_ONE_SPECIAL message606, sb[gp], $gp, $t0, 227969, -227962, 0x88
   STORE_ONE_SPECIAL message607, sb[gp], $gp, $s3, 254057, -254050, 0xed
   STORE_ONE_SPECIAL message608, sb[gp], $gp, $s3, 142976, -142969, 0x5f
   STORE_ONE_SPECIAL message609, sb[gp], $gp, $s5, 85314, -85310, 0xc8
   STORE_ONE_SPECIAL message610, sb[gp], $gp, $s7, 95765, -95760, 0x35
   STORE_ONE_SPECIAL message611, sb[gp], $gp, $a0, 137216, -137215, 0xce
   STORE_ONE_SPECIAL message612, sb[gp], $gp, $t1, 99817, -99815, 0x26
   STORE_ONE_SPECIAL message613, sb[gp], $gp, $s7, 229997, -229994, 0x50
   STORE_ONE_SPECIAL message614, sb[gp], $gp, $a7, 12276, -12274, 0x9c
   STORE_ONE_SPECIAL message615, sb[gp], $gp, $t3, 69799, -69799, 0x3d

# sb[s9]
   STORE_ONE message616, sb[s9], $s4, $a7, -161, 161, 0x18
   STORE_ONE message617, sb[s9], $s7, $a5, -115, 117, 0x81
   STORE_ONE message618, sb[s9], $a3, $s3, 229, -225, 0xc2
   STORE_ONE message619, sb[s9], $t2, $a1, 67, -60, 0x38
   STORE_ONE message620, sb[s9], $s5, $a3, 58, -57, 0x5b
   STORE_ONE message621, sb[s9], $a4, $t3, -48, 55, 0x80
   STORE_ONE message622, sb[s9], $a5, $s3, -43, 46, 0x85
   STORE_ONE message623, sb[s9], $a2, $a1, 248, -245, 0x22
   STORE_ONE message624, sb[s9], $s1, $t0, 184, -180, 0x70
   STORE_ONE message625, sb[s9], $t1, $t0, -241, 244, 0xee
   STORE_ONE message626, sb[s9], $s5, $a1, -234, 235, 0x10
   STORE_ONE message627, sb[s9], $t2, $s2, -239, 242, 0x1e
   STORE_ONE message628, sb[s9], $s0, $a7, -165, 169, 0xc7
   STORE_ONE message629, sb[s9], $s3, $t0, -199, 206, 0xa0
   STORE_ONE message630, sb[s9], $s7, $t1, 180, -174, 0x98

# sbx
   STORE_ONE_INDEXED message631, sbx, $a4, $t1, $s3, -1574287822, 1574287822, 0xfe
   STORE_ONE_INDEXED message632, sbx, $t1, $s4, $s3, 1979699608, -1979699605, 0x99
   STORE_ONE_INDEXED message633, sbx, $t0, $a4, $s5, -1309968153, 1309968158, 0x54
   STORE_ONE_INDEXED message634, sbx, $s2, $a4, $t3, -958573360, 958573367, 0x75
   STORE_ONE_INDEXED message635, sbx, $a7, $s0, $s4, 1763689577, -1763689573, 0xaa
   STORE_ONE_INDEXED message636, sbx, $t3, $s5, $a3, 1030827308, -1030827306, 0xe1
   STORE_ONE_INDEXED message637, sbx, $t3, $s0, $s2, 1887318020, -1887318019, 0x9e
   STORE_ONE_INDEXED message638, sbx, $s5, $s6, $a7, 388548465, -388548460, 0x64
   STORE_ONE_INDEXED message639, sbx, $a2, $t3, $s6, -421121032, 421121032, 0x9c
   STORE_ONE_INDEXED message640, sbx, $s0, $a1, $a3, -171314300, 171314301, 0x54
   STORE_ONE_INDEXED message641, sbx, $a4, $a3, $a5, -1849084165, 1849084167, 0x70
   STORE_ONE_INDEXED message642, sbx, $t1, $a7, $s7, 1392983028, -1392983028, 0xba
   STORE_ONE_INDEXED message643, sbx, $a7, $a2, $a4, -925831078, 925831085, 0xa4
   STORE_ONE_INDEXED message644, sbx, $s7, $a7, $s0, 1537995505, -1537995499, 0x87
   STORE_ONE_INDEXED message645, sbx, $t1, $a5, $a3, -1558649890, 1558649895, 0x90
   STORE_ONE_INDEXED message646, sbx, $a4, $a5, $a7, 1815928, -1815922, 0x34
   STORE_ONE_INDEXED message647, sbx, $s4, $a2, $s2, 2010077582, -2010077576, 0xb
   STORE_ONE_INDEXED message648, sbx, $s6, $a5, $t0, -293779635, 293779637, 0x4
   STORE_ONE_INDEXED message649, sbx, $s5, $a6, $t1, 1394515769, -1394515769, 0xa
   STORE_ONE_INDEXED message650, sbx, $a2, $s3, $a6, -612738550, 612738554, 0xc2
   STORE_ONE_INDEXED message651, sbx, $s5, $s4, $s1, 2011385364, -2011385358, 0x8e
   STORE_ONE_INDEXED message652, sbx, $s0, $a0, $a6, 925587052, -925587050, 0xf0
   STORE_ONE_INDEXED message653, sbx, $s0, $a3, $s5, -755215979, 755215980, 0x94
   STORE_ONE_INDEXED message654, sbx, $s5, $t3, $s1, 1798675143, -1798675143, 0xc0
   STORE_ONE_INDEXED message655, sbx, $t3, $a2, $s6, -1059270696, 1059270703, 0xa8

# sh[u12]
   STORE_ONE message656, sh[u12], $a0, $t0, 2823, -2820, 0xfe82
   STORE_ONE message657, sh[u12], $s5, $s4, 2564, -2561, 0xcf63
   STORE_ONE message658, sh[u12], $s1, $t2, 1175, -1172, 0xc858
   STORE_ONE message659, sh[u12], $a4, $a6, 3559, -3559, 0x6761
   STORE_ONE message660, sh[u12], $s6, $s1, 883, -881, 0x9a82
   STORE_ONE message661, sh[u12], $a1, $a6, 96, -94, 0x7806
   STORE_ONE message662, sh[u12], $s6, $s7, 3366, -3363, 0x5938
   STORE_ONE message663, sh[u12], $a1, $t3, 3621, -3620, 0xc87d
   STORE_ONE message664, sh[u12], $s0, $t3, 3400, -3400, 0xe8c2
   STORE_ONE message665, sh[u12], $a3, $s3, 37, -31, 0xb464
   STORE_ONE message666, sh[u12], $a5, $s5, 3730, -3730, 0x3d82
   STORE_ONE message667, sh[u12], $t0, $a0, 965, -963, 0x182f
   STORE_ONE message668, sh[u12], $a4, $t0, 2012, -2007, 0x5444
   STORE_ONE message669, sh[u12], $a2, $s1, 1673, -1673, 0x31ac
   STORE_ONE message670, sh[u12], $s1, $s5, 601, -599, 0x985a

# sh[16]
   STORE_ONE message671, sh[16], $a2, $s3, 2, -2, 0x9fc
   STORE_ONE message672, sh[16], $a1, $s1, 4, -2, 0xd991
   STORE_ONE message673, sh[16], $a0, $s2, 0, 2, 0xe4d4
   STORE_ONE message674, sh[16], $r0, $a1, 6, -3, 0x0
   STORE_ONE message675, sh[16], $r0, $s1, 2, 3, 0x0
   STORE_ONE message676, sh[16], $a0, $s2, 0, 4, 0xe66f
   STORE_ONE message677, sh[16], $s2, $a0, 0, 2, 0xfa90
   STORE_ONE message678, sh[16], $r0, $s3, 4, 2, 0x0
   STORE_ONE message679, sh[16], $a1, $a2, 6, -6, 0x4626
   STORE_ONE message680, sh[16], $r0, $s3, 0, 2, 0x0
   STORE_ONE message681, sh[16], $a3, $s0, 0, 4, 0x5ce0
   STORE_ONE message682, sh[16], $s2, $a0, 6, -2, 0x587c
   STORE_ONE message683, sh[16], $s3, $a0, 0, 0, 0x1fbe
   STORE_ONE message684, sh[16], $s3, $s0, 2, 1, 0x58a0
   STORE_ONE message685, sh[16], $r0, $a3, 4, -4, 0x0

# sh[gp]
   STORE_ONE_SPECIAL message686, sh[gp], $gp, $a7, 97914, -97909, 0xfa94
   STORE_ONE_SPECIAL message687, sh[gp], $gp, $a3, 198792, -198786, 0x212c
   STORE_ONE_SPECIAL message688, sh[gp], $gp, $s4, 77340, -77336, 0x9df7
   STORE_ONE_SPECIAL message689, sh[gp], $gp, $t2, 26858, -26858, 0x1ea0
   STORE_ONE_SPECIAL message690, sh[gp], $gp, $a6, 234866, -234860, 0x2dec
   STORE_ONE_SPECIAL message691, sh[gp], $gp, $a5, 8908, -8907, 0xdfce
   STORE_ONE_SPECIAL message692, sh[gp], $gp, $a1, 257884, -257882, 0xf500
   STORE_ONE_SPECIAL message693, sh[gp], $gp, $a7, 225462, -225456, 0xd710
   STORE_ONE_SPECIAL message694, sh[gp], $gp, $a7, 202850, -202849, 0x782c
   STORE_ONE_SPECIAL message695, sh[gp], $gp, $t0, 153756, -153751, 0x1b00
   STORE_ONE_SPECIAL message696, sh[gp], $gp, $s4, 1324, -1319, 0x2842
   STORE_ONE_SPECIAL message697, sh[gp], $gp, $a5, 248518, -248514, 0x1928
   STORE_ONE_SPECIAL message698, sh[gp], $gp, $t2, 209742, -209742, 0xff67
   STORE_ONE_SPECIAL message699, sh[gp], $gp, $a7, 82278, -82273, 0x350
   STORE_ONE_SPECIAL message700, sh[gp], $gp, $s5, 121808, -121806, 0xadfa

# sh[s9]
   STORE_ONE message701, sh[s9], $t2, $s7, 176, -175, 0xa382
   STORE_ONE message702, sh[s9], $s4, $s3, 48, -44, 0xa2c7
   STORE_ONE message703, sh[s9], $a3, $s4, -239, 243, 0xcd14
   STORE_ONE message704, sh[s9], $s2, $a0, 175, -170, 0x8be0
   STORE_ONE message705, sh[s9], $a1, $a5, 23, -21, 0x5466
   STORE_ONE message706, sh[s9], $a2, $s7, 2, 1, 0xef38
   STORE_ONE message707, sh[s9], $t3, $a6, 91, -90, 0x9900
   STORE_ONE message708, sh[s9], $a2, $t1, -116, 121, 0x5d6
   STORE_ONE message709, sh[s9], $s4, $s5, 177, -175, 0x6493
   STORE_ONE message710, sh[s9], $s2, $a4, 23, -17, 0x98a0
   STORE_ONE message711, sh[s9], $s7, $a2, 252, -246, 0x55b0
   STORE_ONE message712, sh[s9], $s5, $s7, 136, -134, 0x8d9e
   STORE_ONE message713, sh[s9], $t1, $a1, -204, 204, 0x955a
   STORE_ONE message714, sh[s9], $s3, $a5, 203, -199, 0xd3c8
   STORE_ONE message715, sh[s9], $s6, $t2, 23, -18, 0xabf5

# shx
   STORE_ONE_INDEXED message716, shx, $s5, $a2, $s1, -1185208376, 1185208380, 0xb528
   STORE_ONE_INDEXED message717, shx, $t0, $t2, $t3, -1881705750, 1881705751, 0x43c8
   STORE_ONE_INDEXED message718, shx, $t1, $a7, $s3, 1770890560, -1770890556, 0x7836
   STORE_ONE_INDEXED message719, shx, $t2, $s3, $t0, 111357488, -111357486, 0x184c
   STORE_ONE_INDEXED message720, shx, $a3, $a5, $s4, 1190462769, -1190462767, 0x2174
   STORE_ONE_INDEXED message721, shx, $a0, $t2, $s5, 467865414, -467865408, 0xb27c
   STORE_ONE_INDEXED message722, shx, $t0, $a3, $t3, 285042131, -285042126, 0x2d1c
   STORE_ONE_INDEXED message723, shx, $s3, $t0, $s4, 195994450, -195994444, 0x1ac6
   STORE_ONE_INDEXED message724, shx, $a0, $a5, $a7, 1352360907, -1352360905, 0x65bb
   STORE_ONE_INDEXED message725, shx, $s7, $t0, $t1, 2099009744, -2099009739, 0x18c1
   STORE_ONE_INDEXED message726, shx, $s5, $s2, $a3, -242174144, 242174150, 0x5cb0
   STORE_ONE_INDEXED message727, shx, $a2, $t2, $s5, -2122888420, 2122888421, 0x3cb0
   STORE_ONE_INDEXED message728, shx, $t3, $s1, $a7, 650814703, -650814698, 0x3958
   STORE_ONE_INDEXED message729, shx, $s7, $s5, $s2, -340017640, 340017640, 0xd074
   STORE_ONE_INDEXED message730, shx, $a7, $t3, $s6, -1038862342, 1038862347, 0x14e
   STORE_ONE_INDEXED message731, shx, $s1, $a1, $a5, -621030588, 621030590, 0x5f14
   STORE_ONE_INDEXED message732, shx, $t0, $s5, $a5, -1826111098, 1826111099, 0x6340
   STORE_ONE_INDEXED message733, shx, $t3, $a3, $a6, -359724274, 359724279, 0x6000
   STORE_ONE_INDEXED message734, shx, $a4, $a3, $a0, 1161270612, -1161270609, 0x5e20
   STORE_ONE_INDEXED message735, shx, $a0, $a2, $t0, 642405130, -642405127, 0xf21c
   STORE_ONE_INDEXED message736, shx, $s2, $t1, $a1, -1415439768, 1415439774, 0x3769
   STORE_ONE_INDEXED message737, shx, $s4, $s3, $t3, -1058126984, 1058126984, 0x5700
   STORE_ONE_INDEXED message738, shx, $a5, $a0, $s0, -1060949857, 1060949859, 0xaeef
   STORE_ONE_INDEXED message739, shx, $t3, $t0, $s2, 69652912, -69652912, 0x763e
   STORE_ONE_INDEXED message740, shx, $a4, $a2, $a7, 529202028, -529202028, 0x2580

# shxs
   STORE_ONE_INDEXED message741, shxs, $s0, $a1, $a4, 102300096, -204600191, 0x3c1
   STORE_ONE_INDEXED message742, shxs, $a1, $a7, $a4, 558852670, -1117705335, 0x7768
   STORE_ONE_INDEXED message743, shxs, $a7, $a3, $s0, -985714611, 1971429222, 0x8622
   STORE_ONE_INDEXED message744, shxs, $s3, $s4, $s1, -842016971, 1684033944, 0x1092
   STORE_ONE_INDEXED message745, shxs, $s7, $s6, $a5, -1026915704, 2053831411, 0x4ba0
   STORE_ONE_INDEXED message746, shxs, $t0, $a1, $t1, 936597760, -1873195519, 0x7ada
   STORE_ONE_INDEXED message747, shxs, $s2, $a7, $a6, -576284484, 1152568973, 0x5843
   STORE_ONE_INDEXED message748, shxs, $t2, $s1, $a6, -1010487808, 2020975620, 0xaff0
   STORE_ONE_INDEXED message749, shxs, $t1, $s4, $a6, -629208168, 1258416340, 0x2b44
   STORE_ONE_INDEXED message750, shxs, $a5, $a4, $a0, 189459358, -378918714, 0x5cb2
   STORE_ONE_INDEXED message751, shxs, $a0, $t3, $a1, 168543633, -337087266, 0xdb45
   STORE_ONE_INDEXED message752, shxs, $s7, $s1, $s5, -383950663, 767901328, 0xaad2
   STORE_ONE_INDEXED message753, shxs, $s5, $s3, $s0, -74461768, 148923541, 0xfb3a
   STORE_ONE_INDEXED message754, shxs, $a1, $t2, $a0, -178877856, 357755715, 0x924a
   STORE_ONE_INDEXED message755, shxs, $a7, $s7, $t3, -134211012, 268422030, 0x5a61
   STORE_ONE_INDEXED message756, shxs, $a2, $s7, $s6, 436248887, -872497768, 0xe800
   STORE_ONE_INDEXED message757, shxs, $a6, $a7, $s6, -544104827, 1088209656, 0x33f1
   STORE_ONE_INDEXED message758, shxs, $a7, $s3, $t1, -760662117, 1521324239, 0x8520
   STORE_ONE_INDEXED message759, shxs, $s4, $a5, $t1, -175918568, 351837137, 0x776c
   STORE_ONE_INDEXED message760, shxs, $a5, $s6, $s4, -583200884, 1166401770, 0x764e
   STORE_ONE_INDEXED message761, shxs, $t3, $t0, $s6, 761754835, -1523509668, 0xd40c
   STORE_ONE_INDEXED message762, shxs, $a2, $s4, $t0, 660254600, -1320509194, 0xdf1c
   STORE_ONE_INDEXED message763, shxs, $s7, $t1, $a6, -566325555, 1132651115, 0x3523
   STORE_ONE_INDEXED message764, shxs, $a2, $t3, $a7, -749413228, 1498826458, 0x8436
   STORE_ONE_INDEXED message765, shxs, $a4, $t0, $t2, -646900624, 1293801254, 0x8b0f

# sw[u12]
   STORE_ONE message766, sw[u12], $a3, $a6, 4067, -4067, 0x605408df
   STORE_ONE message767, sw[u12], $a0, $a5, 2410, -2408, 0xebc3ea8
   STORE_ONE message768, sw[u12], $t0, $a6, 1282, -1279, 0xcb0876d8
   STORE_ONE message769, sw[u12], $a4, $s6, 1802, -1802, 0xba4f0dbe
   STORE_ONE message770, sw[u12], $s6, $s1, 3258, -3258, 0x9070292
   STORE_ONE message771, sw[u12], $a0, $t1, 3290, -3286, 0x3de86ea4
   STORE_ONE message772, sw[u12], $s1, $a0, 3831, -3827, 0x57f06426
   STORE_ONE message773, sw[u12], $t2, $s2, 426, -422, 0x999cd10e
   STORE_ONE message774, sw[u12], $a7, $s1, 436, -433, 0xcd1ed957
   STORE_ONE message775, sw[u12], $a3, $t3, 1945, -1945, 0xf7573715
   STORE_ONE message776, sw[u12], $a5, $s3, 2312, -2311, 0xde306724
   STORE_ONE message777, sw[u12], $a0, $s4, 404, -400, 0xfe6b21fa
   STORE_ONE message778, sw[u12], $s3, $a1, 3408, -3404, 0xc2493841
   STORE_ONE message779, sw[u12], $t3, $t0, 1619, -1616, 0x32885c64
   STORE_ONE message780, sw[u12], $t3, $a4, 83, -80, 0x29c730bc

# sw[16]
   STORE_ONE message781, sw[16], $a3, $a0, 28, -26, 0xf1973456
   STORE_ONE message782, sw[16], $s2, $a1, 4, -4, 0xcc16087c
   STORE_ONE message783, sw[16], $s1, $s3, 32, -28, 0x7724098e
   STORE_ONE message784, sw[16], $a0, $s3, 0, 3, 0x7f99d23f
   STORE_ONE message785, sw[16], $s3, $s2, 52, -51, 0x1cb6bffe
   STORE_ONE message786, sw[16], $a0, $s0, 60, -59, 0xeccfdcc2
   STORE_ONE message787, sw[16], $s1, $a1, 40, -40, 0xe1cafebf
   STORE_ONE message788, sw[16], $s2, $s0, 40, -40, 0x6bdda588
   STORE_ONE message789, sw[16], $a2, $a0, 28, -25, 0x325cbb80
   STORE_ONE message790, sw[16], $s2, $a2, 28, -28, 0x8c879230
   STORE_ONE message791, sw[16], $a2, $s2, 20, -17, 0xc4b014d1
   STORE_ONE message792, sw[16], $a1, $s2, 48, -47, 0x45f22586
   STORE_ONE message793, sw[16], $s1, $s0, 4, -3, 0x301cb53c
   STORE_ONE message794, sw[16], $a3, $s2, 36, -35, 0x29066fb
   STORE_ONE message795, sw[16], $s2, $s3, 36, -33, 0xde0287ee

# sw[4x4]
   STORE_ONE message796, sw[4x4], $a1, $a5, 4, -4, 0xd002e16b
   STORE_ONE message797, sw[4x4], $s6, $s3, 12, -8, 0x137ce040
   STORE_ONE message798, sw[4x4], $s7, $s5, 12, -8, 0x9b916c76
   STORE_ONE message799, sw[4x4], $s0, $a5, 8, -4, 0x10dae673
   STORE_ONE message800, sw[4x4], $s3, $a3, 12, -8, 0x3104afd1
   STORE_ONE message801, sw[4x4], $a1, $s5, 4, 0, 0xb1f22230
   STORE_ONE message802, sw[4x4], $s2, $s7, 8, -8, 0x8f7539ab
   STORE_ONE message803, sw[4x4], $a2, $s4, 0, 2, 0xcf5eb1c4
   STORE_ONE message804, sw[4x4], $s5, $a4, 8, -7, 0x23f21ad7
   STORE_ONE message805, sw[4x4], $a5, $a4, 4, -1, 0x24699d1f
   STORE_ONE message806, sw[4x4], $s1, $a4, 8, -4, 0x783fff40
   STORE_ONE message807, sw[4x4], $s7, $s2, 12, -10, 0x8a5493cc
   STORE_ONE message808, sw[4x4], $s6, $s7, 8, -5, 0x5831758
   STORE_ONE message809, sw[4x4], $a0, $a2, 8, -8, 0x7ec0c4b4
   STORE_ONE message810, sw[4x4], $a0, $a1, 12, -10, 0x53dc6b1e

# sw[gp16]
   STORE_ONE_SPECIAL message811, sw[gp16], $gp, $a2, 64, -61, 0xf0b9b4c4
   STORE_ONE_SPECIAL message812, sw[gp16], $gp, $a0, 80, -79, 0xb6b6d0fe
   STORE_ONE_SPECIAL message813, sw[gp16], $gp, $s1, 204, -204, 0xcb48a2e8
   STORE_ONE_SPECIAL message814, sw[gp16], $gp, $a3, 108, -106, 0x7d3324b2
   STORE_ONE_SPECIAL message815, sw[gp16], $gp, $s1, 500, -499, 0xedc40250
   STORE_ONE_SPECIAL message816, sw[gp16], $gp, $s2, 336, -336, 0x8f50e794
   STORE_ONE_SPECIAL message817, sw[gp16], $gp, $a2, 244, -244, 0xa3a59a64
   STORE_ONE_SPECIAL message818, sw[gp16], $gp, $s1, 448, -445, 0x81e828c4
   STORE_ONE_SPECIAL message819, sw[gp16], $gp, $r0, 436, -435, 0x0
   STORE_ONE_SPECIAL message820, sw[gp16], $gp, $a2, 472, -468, 0x239b7c59
   STORE_ONE_SPECIAL message821, sw[gp16], $gp, $s1, 296, -293, 0xb6441221
   STORE_ONE_SPECIAL message822, sw[gp16], $gp, $a2, 104, -101, 0x1fa0412d
   STORE_ONE_SPECIAL message823, sw[gp16], $gp, $a2, 268, -267, 0xe28ad89c
   STORE_ONE_SPECIAL message824, sw[gp16], $gp, $s1, 12, -9, 0x4c177db0
   STORE_ONE_SPECIAL message825, sw[gp16], $gp, $r0, 508, -507, 0x0

# sw[gp]
   STORE_ONE_SPECIAL message826, sw[gp], $gp, $t1, 1256212, -1256209, 0x9bf7a4c0
   STORE_ONE_SPECIAL message827, sw[gp], $gp, $s7, 335156, -335152, 0x1e684ba8
   STORE_ONE_SPECIAL message828, sw[gp], $gp, $a0, 1691004, -1691001, 0x7f18ee47
   STORE_ONE_SPECIAL message829, sw[gp], $gp, $a4, 1227428, -1227424, 0x3185808f
   STORE_ONE_SPECIAL message830, sw[gp], $gp, $s0, 1940092, -1940089, 0xf63e7320
   STORE_ONE_SPECIAL message831, sw[gp], $gp, $s6, 669684, -669683, 0xc8aefe78
   STORE_ONE_SPECIAL message832, sw[gp], $gp, $s4, 907596, -907595, 0xc52a5945
   STORE_ONE_SPECIAL message833, sw[gp], $gp, $a6, 1096132, -1096129, 0xc1cedd62
   STORE_ONE_SPECIAL message834, sw[gp], $gp, $s1, 461204, -461201, 0xb6aa0369
   STORE_ONE_SPECIAL message835, sw[gp], $gp, $a4, 1508912, -1508910, 0x6214e23e
   STORE_ONE_SPECIAL message836, sw[gp], $gp, $s5, 416328, -416327, 0x183fad53
   STORE_ONE_SPECIAL message837, sw[gp], $gp, $s0, 1214072, -1214072, 0x6af683c0
   STORE_ONE_SPECIAL message838, sw[gp], $gp, $a1, 1678396, -1678395, 0xc4ccadca
   STORE_ONE_SPECIAL message839, sw[gp], $gp, $s0, 1081964, -1081960, 0x299fb1bb
   STORE_ONE_SPECIAL message840, sw[gp], $gp, $a6, 2010592, -2010590, 0xb324b492

# sw[s9]
   STORE_ONE message841, sw[s9], $s4, $s7, -143, 143, 0x90f732c2
   STORE_ONE message842, sw[s9], $s3, $s4, 144, -143, 0x892b4450
   STORE_ONE message843, sw[s9], $a7, $a0, 226, -223, 0x5ce31003
   STORE_ONE message844, sw[s9], $s4, $s3, -95, 98, 0x4ba6a914
   STORE_ONE message845, sw[s9], $a2, $a4, 27, -25, 0x581c02b7
   STORE_ONE message846, sw[s9], $s6, $a7, 216, -214, 0x1d581a04
   STORE_ONE message847, sw[s9], $a6, $s2, -240, 240, 0x6d7183c9
   STORE_ONE message848, sw[s9], $a2, $s6, -12, 16, 0xed003c5c
   STORE_ONE message849, sw[s9], $t1, $t0, 91, -91, 0x423d6c34
   STORE_ONE message850, sw[s9], $a0, $a2, -243, 246, 0x739ef8f0
   STORE_ONE message851, sw[s9], $s1, $a5, -237, 238, 0xdc34d0b3
   STORE_ONE message852, sw[s9], $t2, $s3, -75, 77, 0xcda3f67c
   STORE_ONE message853, sw[s9], $a5, $s7, -218, 219, 0x847cceb9
   STORE_ONE message854, sw[s9], $s5, $s1, 235, -234, 0x3e1a88c9
   STORE_ONE message855, sw[s9], $s1, $s4, 118, -115, 0x19e9a9dc

# sw[sp]
   STORE_ONE_SPECIAL message856, sw[sp], $sp, $s2, 104, -103, 0x350e7b3
   STORE_ONE_SPECIAL message857, sw[sp], $sp, $t0, 0, 2, 0xe3d79142
   STORE_ONE_SPECIAL message858, sw[sp], $sp, $t3, 16, -16, 0xa8350be7
   STORE_ONE_SPECIAL message859, sw[sp], $sp, $s5, 88, -88, 0xe78e466c
   STORE_ONE_SPECIAL message860, sw[sp], $sp, $s3, 12, -9, 0x91982b89
   STORE_ONE_SPECIAL message861, sw[sp], $sp, $s4, 112, -110, 0xd0629772
   STORE_ONE_SPECIAL message862, sw[sp], $sp, $a2, 88, -88, 0x1bdced56
   STORE_ONE_SPECIAL message863, sw[sp], $sp, $s4, 100, -100, 0xb7273b18
   STORE_ONE_SPECIAL message864, sw[sp], $sp, $s4, 124, -123, 0x1694f4ce
   STORE_ONE_SPECIAL message865, sw[sp], $sp, $a7, 56, -56, 0xd2156bd8
   STORE_ONE_SPECIAL message866, sw[sp], $sp, $t0, 104, -100, 0xfe07630
   STORE_ONE_SPECIAL message867, sw[sp], $sp, $s6, 12, -9, 0xa2934e46
   STORE_ONE_SPECIAL message868, sw[sp], $sp, $a2, 44, -41, 0xc02e2525
   STORE_ONE_SPECIAL message869, sw[sp], $sp, $a1, 16, -13, 0x5abfbd34
   STORE_ONE_SPECIAL message870, sw[sp], $sp, $a5, 12, -11, 0xe2408472

# swx
   STORE_ONE_INDEXED message871, swx, $s3, $s1, $t3, -267022376, 267022378, 0xae5606f2
   STORE_ONE_INDEXED message872, swx, $a3, $s1, $t1, -2050865115, 2050865116, 0xf8e8eec6
   STORE_ONE_INDEXED message873, swx, $s1, $a2, $s2, 470286098, -470286095, 0x85fd1998
   STORE_ONE_INDEXED message874, swx, $s1, $s3, $s6, 558580824, -558580820, 0x31e721bd
   STORE_ONE_INDEXED message875, swx, $s2, $a3, $a0, -1315160890, 1315160893, 0xdbd46b6a
   STORE_ONE_INDEXED message876, swx, $s2, $s4, $a6, -46089267, 46089268, 0xf3f0eda
   STORE_ONE_INDEXED message877, swx, $s0, $t2, $a0, 1565893736, -1565893733, 0x9c50c2c3
   STORE_ONE_INDEXED message878, swx, $s5, $a1, $t1, -1318616795, 1318616795, 0x476174c
   STORE_ONE_INDEXED message879, swx, $s0, $a3, $a2, -1253016562, 1253016564, 0xcc1fe50
   STORE_ONE_INDEXED message880, swx, $s4, $a2, $t0, -428619024, 428619027, 0xee3f7f30
   STORE_ONE_INDEXED message881, swx, $s5, $a1, $t1, -1648889980, 1648889981, 0xc1585396
   STORE_ONE_INDEXED message882, swx, $a3, $s5, $a4, -1446250253, 1446250253, 0x6d87dc1
   STORE_ONE_INDEXED message883, swx, $a1, $t1, $s3, 924269755, -924269755, 0xb83828f3
   STORE_ONE_INDEXED message884, swx, $a5, $s3, $s6, -1962913772, 1962913774, 0xe4cd2a7e
   STORE_ONE_INDEXED message885, swx, $s7, $t3, $a3, 1225037090, -1225037086, 0xe03c5d4
   STORE_ONE_INDEXED message886, swx, $a3, $t3, $a4, -1104778944, 1104778945, 0xf9d86f10
   STORE_ONE_INDEXED message887, swx, $a3, $t3, $a4, -1004753154, 1004753156, 0x943a140b
   STORE_ONE_INDEXED message888, swx, $a1, $t1, $a2, 1613638279, -1613638279, 0x4a82d9c7
   STORE_ONE_INDEXED message889, swx, $a4, $a1, $a0, 1102633316, -1102633313, 0x91de8140
   STORE_ONE_INDEXED message890, swx, $s7, $s6, $t0, -1427384786, 1427384789, 0x502d0d3e
   STORE_ONE_INDEXED message891, swx, $s0, $s6, $a0, -1543934228, 1543934231, 0xbfb2b74b
   STORE_ONE_INDEXED message892, swx, $a1, $t1, $a2, 1817001073, -1817001071, 0x1d1209e0
   STORE_ONE_INDEXED message893, swx, $t2, $t0, $s5, 169141656, -169141655, 0x33802f08
   STORE_ONE_INDEXED message894, swx, $s5, $t0, $t3, -957047160, 957047162, 0x4baef5e1
   STORE_ONE_INDEXED message895, swx, $t1, $s4, $s2, 1201439944, -1201439940, 0xcdb10990

# swxs
   STORE_ONE_INDEXED message896, swxs, $a3, $s4, $t1, -506690338, 2026761355, 0x7dfbfc6d
   STORE_ONE_INDEXED message897, swxs, $a7, $s4, $s7, -366619988, 1466479954, 0x2ad94f18
   STORE_ONE_INDEXED message898, swxs, $a7, $a0, $a6, -299490627, 1197962511, 0xe6f5623c
   STORE_ONE_INDEXED message899, swxs, $s4, $s3, $t0, -192777074, 771108299, 0xea1ee6b2
   STORE_ONE_INDEXED message900, swxs, $a7, $a0, $t3, -481416775, 1925667103, 0x71fb5772
   STORE_ONE_INDEXED message901, swxs, $s0, $s7, $a6, 498553996, -1994215980, 0x9f885748
   STORE_ONE_INDEXED message902, swxs, $s2, $t3, $t1, 371121559, -1484486234, 0x6763425
   STORE_ONE_INDEXED message903, swxs, $t1, $s6, $s2, 310771462, -1243085844, 0x54339880
   STORE_ONE_INDEXED message904, swxs, $a6, $t0, $a2, -319208424, 1276833700, 0x27503c0
   STORE_ONE_INDEXED message905, swxs, $a3, $a0, $a5, -87227538, 348910153, 0xdf9addfc
   STORE_ONE_INDEXED message906, swxs, $a7, $t3, $a3, 180676538, -722706149, 0xbbaa505c
   STORE_ONE_INDEXED message907, swxs, $s2, $t0, $a7, 327465602, -1309862405, 0x9784cc4c
   STORE_ONE_INDEXED message908, swxs, $s4, $t2, $s0, 461538417, -1846153667, 0x14cf412e
   STORE_ONE_INDEXED message909, swxs, $a6, $t3, $a0, -528041401, 2112165606, 0xba0eadd4
   STORE_ONE_INDEXED message910, swxs, $a7, $a4, $s5, -78452748, 313810992, 0x25428b10
   STORE_ONE_INDEXED message911, swxs, $t3, $t1, $a6, -275566081, 1102264328, 0xb7df5e77
   STORE_ONE_INDEXED message912, swxs, $a1, $a7, $a3, 327508595, -1310034379, 0xe1a4bbb
   STORE_ONE_INDEXED message913, swxs, $s2, $a1, $a6, -121923671, 487694684, 0x7e033432
   STORE_ONE_INDEXED message914, swxs, $s5, $a1, $a5, 39179448, -156717788, 0x4f401f06
   STORE_ONE_INDEXED message915, swxs, $a3, $a2, $a7, -404613987, 1618455950, 0x21d86d90
   STORE_ONE_INDEXED message916, swxs, $a5, $s7, $a1, -504190100, 2016760400, 0xe71f88eb
   STORE_ONE_INDEXED message917, swxs, $s5, $a3, $a6, -241242052, 964968212, 0x13e2648e
   STORE_ONE_INDEXED message918, swxs, $a7, $s3, $s6, 391055714, -1564222852, 0xc4187810
   STORE_ONE_INDEXED message919, swxs, $t2, $a6, $t3, 220755400, -883021596, 0xbe02a7d0
   STORE_ONE_INDEXED message920, swxs, $a5, $a7, $a4, 164070419, -656281676, 0x29240bda

# li[16]
   LOAD_IMMIDIATE message921, li[16], $s2, 0x45
   LOAD_IMMIDIATE message922, li[16], $s0, 0x61
   LOAD_IMMIDIATE message923, li[16], $a0, 0x76
   LOAD_IMMIDIATE message924, li[16], $s2, 0x56
   LOAD_IMMIDIATE message925, li[16], $a2, 0x4a
   LOAD_IMMIDIATE message926, li[16], $a2, 0x56
   LOAD_IMMIDIATE message927, li[16], $s0, 0x24
   LOAD_IMMIDIATE message928, li[16], $s1, 0x47
   LOAD_IMMIDIATE message929, li[16], $a3, 0x73
   LOAD_IMMIDIATE message930, li[16], $a3, 0x7a
   LOAD_IMMIDIATE message931, li[16], $a3, 0x36
   LOAD_IMMIDIATE message932, li[16], $s0, 0x7e
   LOAD_IMMIDIATE message933, li[16], $a2, 0x2e
   LOAD_IMMIDIATE message934, li[16], $a1, 0x38
   LOAD_IMMIDIATE message935, li[16], $s0, 0x63

# li[48]
   LOAD_IMMIDIATE message936, li[48], $s1, 0xe88802f8
   LOAD_IMMIDIATE message937, li[48], $t0, 0xca7c9510
   LOAD_IMMIDIATE message938, li[48], $s2, 0x6fa34b69
   LOAD_IMMIDIATE message939, li[48], $a4, 0xaad5e96a
   LOAD_IMMIDIATE message940, li[48], $s1, 0x6af9999c
   LOAD_IMMIDIATE message941, li[48], $s3, 0xf1668e16
   LOAD_IMMIDIATE message942, li[48], $a5, 0x66d91818
   LOAD_IMMIDIATE message943, li[48], $a0, 0xdda3aba4
   LOAD_IMMIDIATE message944, li[48], $a5, 0xdd393f4c
   LOAD_IMMIDIATE message945, li[48], $s3, 0x747848a9
   LOAD_IMMIDIATE message946, li[48], $s6, 0xb2a4a54c
   LOAD_IMMIDIATE message947, li[48], $a0, 0x5e4cdfc8
   LOAD_IMMIDIATE message948, li[48], $t1, 0xf68fb365
   LOAD_IMMIDIATE message949, li[48], $a2, 0x9f08e274
   LOAD_IMMIDIATE message950, li[48], $t3, 0x651f88dc

# lui
   LOAD_IMMIDIATE_HI message951, lui, $a4, 0x94c22000
   LOAD_IMMIDIATE_HI message952, lui, $s2, 0x29ea8000
   LOAD_IMMIDIATE_HI message953, lui, $a4, 0xbe5c9000
   LOAD_IMMIDIATE_HI message954, lui, $s5, 0xe52cb000
   LOAD_IMMIDIATE_HI message955, lui, $s3, 0xf1618000
   LOAD_IMMIDIATE_HI message956, lui, $a4, 0x74a54000
   LOAD_IMMIDIATE_HI message957, lui, $a5, 0xe4517000
   LOAD_IMMIDIATE_HI message958, lui, $a7, 0x3c117000
   LOAD_IMMIDIATE_HI message959, lui, $a6, 0x56987000
   LOAD_IMMIDIATE_HI message960, lui, $t2, 0x93839000
   LOAD_IMMIDIATE_HI message961, lui, $s1, 0xc4372000
   LOAD_IMMIDIATE_HI message962, lui, $s6, 0xd7aa3000
   LOAD_IMMIDIATE_HI message963, lui, $s4, 0x884a000
   LOAD_IMMIDIATE_HI message964, lui, $a0, 0xa801c000
   LOAD_IMMIDIATE_HI message965, lui, $s6, 0x89c8f000

# lsa
   THREE_REG_1_IMM message966, lsa, $a6, $a0, $t1, 40, 24, test_ints, test_ints, 0
   THREE_REG_1_IMM message967, lsa, $t2, $a5, $a4, 0, 28, test_ints, test_ints, 3
   THREE_REG_1_IMM message968, lsa, $a6, $s1, $s1, 24, 28, test_ints, test_ints, 3
   THREE_REG_1_IMM message969, lsa, $a7, $t1, $s3, 28, 20, test_ints, test_ints, 3
   THREE_REG_1_IMM message970, lsa, $a1, $s1, $t0, 12, 44, test_ints, test_ints, 0
   THREE_REG_1_IMM message971, lsa, $s6, $a2, $t1, 32, 0, test_ints, test_ints, 3
   THREE_REG_1_IMM message972, lsa, $a4, $a2, $a0, 16, 24, test_ints, test_ints, 3
   THREE_REG_1_IMM message973, lsa, $s4, $s0, $a5, 20, 44, test_ints, test_ints, 0
   THREE_REG_1_IMM message974, lsa, $s0, $s6, $s5, 12, 24, test_ints, test_ints, 2
   THREE_REG_1_IMM message975, lsa, $a2, $a3, $a4, 40, 40, test_ints, test_ints, 3
   THREE_REG_1_IMM message976, lsa, $s6, $a1, $a1, 44, 4, test_ints, test_ints, 2
   THREE_REG_1_IMM message977, lsa, $a6, $a1, $a0, 24, 40, test_ints, test_ints, 3
   THREE_REG_1_IMM message978, lsa, $a2, $a6, $a3, 44, 44, test_ints, test_ints, 2
   THREE_REG_1_IMM message979, lsa, $s7, $a7, $s5, 32, 24, test_ints, test_ints, 3
   THREE_REG_1_IMM message980, lsa, $s6, $a0, $t2, 40, 40, test_ints, test_ints, 1
   THREE_REG_1_IMM message981, lsa, $s2, $s0, $a6, 28, 32, test_ints, test_ints, 3
   THREE_REG_1_IMM message982, lsa, $a1, $s2, $a4, 8, 4, test_ints, test_ints, 2
   THREE_REG_1_IMM message983, lsa, $t1, $s3, $a4, 32, 8, test_ints, test_ints, 3
   THREE_REG_1_IMM message984, lsa, $s3, $a5, $s2, 32, 36, test_ints, test_ints, 0
   THREE_REG_1_IMM message985, lsa, $s3, $t3, $a0, 20, 40, test_ints, test_ints, 2
   THREE_REG_1_IMM message986, lsa, $a6, $s4, $a3, 16, 12, test_ints, test_ints, 3
   THREE_REG_1_IMM message987, lsa, $s7, $t1, $a5, 0, 28, test_ints, test_ints, 2
   THREE_REG_1_IMM message988, lsa, $t2, $s0, $t1, 44, 0, test_ints, test_ints, 3
   THREE_REG_1_IMM message989, lsa, $a6, $a3, $s4, 4, 12, test_ints, test_ints, 1
   THREE_REG_1_IMM message990, lsa, $t1, $a2, $s1, 40, 32, test_ints, test_ints, 3
   THREE_REG_1_IMM message991, lsa, $s4, $s2, $s3, 28, 28, test_ints, test_ints, 2
   THREE_REG_1_IMM message992, lsa, $s6, $s2, $s7, 0, 24, test_ints, test_ints, 2
   THREE_REG_1_IMM message993, lsa, $t2, $a1, $a6, 16, 0, test_ints, test_ints, 3
   THREE_REG_1_IMM message994, lsa, $s7, $a2, $s2, 32, 12, test_ints, test_ints, 2
   THREE_REG_1_IMM message995, lsa, $t1, $a1, $a4, 24, 12, test_ints, test_ints, 0
   THREE_REG_1_IMM message996, lsa, $t1, $s7, $s3, 0, 28, test_ints, test_ints, 2
   THREE_REG_1_IMM message997, lsa, $t3, $t1, $s0, 8, 4, test_ints, test_ints, 3
   THREE_REG_1_IMM message998, lsa, $s5, $a0, $s4, 28, 32, test_ints, test_ints, 0
   THREE_REG_1_IMM message999, lsa, $s2, $a3, $a2, 4, 12, test_ints, test_ints, 2
   THREE_REG_1_IMM message1000, lsa, $s7, $s5, $a7, 44, 44, test_ints, test_ints, 2
   THREE_REG_1_IMM message1001, lsa, $a0, $a1, $s5, 12, 4, test_ints, test_ints, 0
   THREE_REG_1_IMM message1002, lsa, $a1, $a0, $a6, 20, 12, test_ints, test_ints, 3
   THREE_REG_1_IMM message1003, lsa, $s4, $s4, $t3, 16, 44, test_ints, test_ints, 3
   THREE_REG_1_IMM message1004, lsa, $a4, $s6, $s3, 28, 28, test_ints, test_ints, 2
   THREE_REG_1_IMM message1005, lsa, $s5, $s6, $t3, 32, 36, test_ints, test_ints, 3

# lwm $fp, -204($a5), 2
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)

   LOAD_STORE_MULTIPLE lwm, message1006, $fp, $a5, -204, 260, 2
   move $s1, $ra
   move $s0, $fp
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   li $a0, message1006
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_30
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_31
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9

# lwm $t2, -251($s3), 1

   LOAD_STORE_MULTIPLE lwm, message1007, $t2, $s3, -251, 287, 1
   move $s0, $t2
   li $a0, message1007
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_14
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9

# lwm $a3, -52($s0), 2

   LOAD_STORE_MULTIPLE lwm, message1008, $a3, $s0, -52, 104, 2
   move $s0, $a3
   move $s1, $a4
   li $a0, message1008
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_7
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_8
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9

# lwm $t0, 3($s3), 3

   LOAD_STORE_MULTIPLE lwm, message1009, $t0, $s3, 3, 17, 3
   move $s0, $t0
   move $s1, $t1
   move $s2, $t2
   li $a0, message1009
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_12
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_13
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_14
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9

# lwm $a2, -55($s3), 1

   LOAD_STORE_MULTIPLE lwm, message1010, $a2, $s3, -55, 87, 1
   move $s0, $a2
   li $a0, message1010
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_6
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9

# swm $sp, -37($a6), 1
   .data
   src_message1011:
   .ascii "$sp, 2f10b798\n"
   .byte 0
   .text
   li $a0, src_message1011
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li $sp, 0x2f10b798
   LOAD_STORE_MULTIPLE swm, message1011, $sp, $a6, -37, 93, 1
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   li $a0, message1011
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# swm $s4, -211($a1), 4
   .data
   src_message1012:
   .ascii "$s4, ab4634cc\n$s5, fde9da3a\n$s6, b0b084f2\n$s7, 992324c0\n"
   .byte 0
   .text
   li $a0, src_message1012
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li $s4, 0xab4634cc
   li $s5, 0xfde9da3a
   li $s6, 0xb0b084f2
   li $s7, 0x992324c0
   LOAD_STORE_MULTIPLE swm, message1012, $s4, $a1, -211, 247, 4
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   li $a0, message1012
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# swm $s6, -77($a4), 3
   .data
   src_message1013:
   .ascii "$s6, e0c2a22e\n$s7, fa07c370\n$t8, bab6978e\n"
   .byte 0
   .text
   li $a0, src_message1013
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li $s6, 0xe0c2a22e
   li $s7, 0xfa07c370
   li $t8, 0xbab6978e
   LOAD_STORE_MULTIPLE swm, message1013, $s6, $a4, -77, 85, 3
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   li $a0, message1013
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# swm $t4, -240($s2), 1
   .data
   src_message1014:
   .ascii "$t4, d1109670\n"
   .byte 0
   .text
   li $a0, src_message1014
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $t4, 0xd1109670
   LOAD_STORE_MULTIPLE swm, message1014, $t4, $s2, -240, 268, 1
   li $a0, message1014
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# swm $t2, 22($s1), 2
   .data
   src_message1015:
   .ascii "$t2, 8658e0c0\n$t3, de5a7cc8\n"
   .byte 0
   .text
   li $a0, src_message1015
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $t2, 0x8658e0c0
   li $t3, 0xde5a7cc8
   LOAD_STORE_MULTIPLE swm, message1015, $t2, $s1, 22, -2, 2
   li $a0, message1015
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# restore.jrc[16]  96, $ra
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -32
   li $ra, label1016
   sw $ra, 92($sp)
   restore.jrc[16]  96, $ra
   label1016:
   move $s0, $ra
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1016:
   .ascii "restore.jrc[16]  96, $ra   %d\n"
   .byte 0
   .text
   li $a0, message1016
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[16]  112, $ra, $s0, $s1
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -48
   li $ra, label1017
   sw $ra, 108($sp)
   restore.jrc[16]  112, $ra, $s0, $s1
   label1017:
   move $s2, $s1
   move $s1, $s0
   move $s0, $ra
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1017:
   .ascii "restore.jrc[16]  112, $ra, $s0, $s1   %d\n"
   .byte 0
   .text
   li $a0, message1017
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_16
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_17
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[16]  128, $fp, $ra, $s0
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -64
   li $ra, label1018
   sw $ra, 120($sp)
   restore.jrc[16]  128, $fp, $ra, $s0
   label1018:
   move $s2, $s0
   move $s1, $ra
   move $s0, $fp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1018:
   .ascii "restore.jrc[16]  128, $fp, $ra, $s0   %d\n"
   .byte 0
   .text
   li $a0, message1018
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_30
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_16
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[16]  176, $fp, $ra, $s0, $s1, $s2, $s3, $s4
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -112
   li $ra, label1019
   sw $ra, 168($sp)
   restore.jrc[16]  176, $fp, $ra, $s0, $s1, $s2, $s3, $s4
   label1019:
   move $s6, $s4
   move $s5, $s3
   move $s4, $s2
   move $s3, $s1
   move $s2, $s0
   move $s1, $ra
   move $s0, $fp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1019:
   .ascii "restore.jrc[16]  176, $fp, $ra, $s0, $s1, $s2, $s3, $s4   %d\n"
   .byte 0
   .text
   li $a0, message1019
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_30
   move $a1, $fp
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_16
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_17
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_18
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_19
   move $a1, $s3
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_20
   move $a1, $s4
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[16]  240, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li $sp, test_data2 + -176
   li $ra, label1020
   sw $ra, 232($sp)
   restore.jrc  240, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8
   label1020:
   sw $s6, 0($a0)
   sw $s7, -4($a0)
   sw $t8, -8($a0)
   move $s7, $s5
   move $s6, $s4
   move $s5, $s3
   move $s4, $s2
   move $s3, $s1
   move $s2, $s0
   move $s1, $ra
   move $s0, $fp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1020:
   .ascii "restore.jrc[16]  240, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8   %d\n"
   .byte 0
   .text
   li $a0, message1020
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_30
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_16
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_17
   move $a1, $s3
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_18
   move $a1, $s4
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_19
   move $a1, $s5
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_20
   move $a1, $s6
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_21
   move $a1, $s7
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   addiu $sp, 12
   lw $s6, 0($sp)
   lw $s7, -4($sp)
   lw $t8, -8($sp)
   li $a0, reg_name_22
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_23
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_24
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[32]  2496, $a7, $t0
   move $t8, $sp
   li  $sp, test_data2 + -2432
   li $ra, label1021
   restore.jrc[32]  2496, $a7, $t0
   label1021:
   move $s0, $a7
   move $s1, $t0
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1021:
   .ascii "restore.jrc[32]  2496, $a7, $t0   %d\n"
   .byte 0
   .text
   li $a0, message1021
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_11
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_12
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[32]  3480, $t2
   move $t8, $sp
   li  $sp, test_data2 + -3416
   li $ra, label1022
   restore.jrc[32]  3480, $t2
   label1022:
   move $s0, $t2
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1022:
   .ascii "restore.jrc[32]  3480, $t2   %d\n"
   .byte 0
   .text
   li $a0, message1022
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_14
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[32]  1648, $a0, $a1
   move $t8, $sp
   li  $sp, test_data2 + -1584
   li $ra, label1023
   restore.jrc[32]  1648, $a0, $a1
   label1023:
   move $s0, $a0
   move $s1, $a1
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1023:
   .ascii "restore.jrc[32]  1648, $a0, $a1   %d\n"
   .byte 0
   .text
   li $a0, message1023
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_4
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_5
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[32]  2856, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3
   move $t8, $sp
   li  $sp, test_data2 + -2792
   li $ra, label1024
   restore.jrc[32]  2856, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3
   label1024:
   move $s0, $a4
   move $s1, $a5
   move $s2, $a6
   move $s3, $a7
   move $s4, $t0
   move $s5, $t1
   move $s6, $t2
   move $s7, $t3
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1024:
   .ascii "restore.jrc[32]  2856, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3   %d\n"
   .byte 0
   .text
   li $a0, message1024
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_8
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_9
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_10
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_11
   move $a1, $s3
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_12
   move $a1, $s4
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_13
   move $a1, $s5
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_14
   move $a1, $s6
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_15
   move $a1, $s7
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore.jrc[32]  2216, $gp
   move $t8, $sp
   li  $sp, test_data2 + -2152
   li $ra, label1025
   move $t9, $gp
   restore.jrc[32]  2216, $gp
   label1025:
   move $gp, $t9
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1025:
   .ascii "restore.jrc[32]  2216, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1025
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_28
   move $a1, $gp
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore  1696, $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -1632
   restore  1696, $t9
   label1026:
   move $s0, $t9
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1026:
   .ascii "restore  1696, $t9   %d\n"
   .byte 0
   .text
   li $a0, message1026
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_25
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore  1496, $s1, $gp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -1432
   restore  1496, $s1, $gp
   label1027:
   move $s0, $s1
   move $s1, $gp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)

   .data
   message1027:
   .ascii "restore  1496, $s1, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1027
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_17
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_28
   move $a1, $gp
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore  104, $gp
   move $t8, $sp
   li  $sp, test_data2 + -40
   sw $gp, 4($t8)
   restore  104, $gp
   label1028:
   lw $gp, 4($t8)
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1028:
   .ascii "restore  104, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1028
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_28
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore  2616, $a5, $a6, $a7, $t0, $t1, $t2
   move $t8, $sp
   li  $sp, test_data2 + -2552
   restore  2616, $a5, $a6, $a7, $t0, $t1, $t2
   label1029:
   move $s0, $a5
   move $s1, $a6
   move $s2, $a7
   move $s3, $t0
   move $s4, $t1
   move $s5, $t2
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1029:
   .ascii "restore  2616, $a5, $a6, $a7, $t0, $t1, $t2   %d\n"
   .byte 0
   .text
   li $a0, message1029
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_9
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_10
   move $a1, $s1
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_11
   move $a1, $s2
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_12
   move $a1, $s3
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_13
   move $a1, $s4
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_14
   move $a1, $s5
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# restore  1800, $gp
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + -1736
   restore  1800, $gp
   label1030:
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1030:
   .ascii "restore  1800, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1030
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   li $a0, reg_name_28
   move $a1, $s0
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc cleanup_test_data2

# save[16]  80, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8
   .data
   src_message1031:
   .ascii "$fp, fa689f75\n$ra, 1c688bea\n$s0, 5c808b67\n$s1, c2d26968\n$s2, 1e152238\n$s3, fca9a1a5\n$s4, 76661001\n$s5, 233ba56b\n$s6, a89de2d8\n$s7, 234a400a\n$t8, 40ff6fa0\n"
   .byte 0
   .text
   li $a0, src_message1031
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $fp, 0xfa689f75
   li $ra, 0x1c688bea
   li $s0, 0x5c808b67
   li $s1, 0xc2d26968
   li $s2, 0x1e152238
   li $s3, 0xfca9a1a5
   li $s4, 0x76661001
   li $s5, 0x233ba56b
   li $s6, 0xa89de2d8
   li $s7, 0x234a400a
   li $t8, 0x40ff6fa0
   save[16]  80, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1031:
   .ascii "save[16]  80, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8   %d\n"
   .byte 0
   .text
   li $a0, message1031
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[16]  32, $ra, $s0, $s1, $s2, $s3, $s4, $s5
   .data
   src_message1032:
   .ascii "$ra, 3c251bcc\n$s0, e3f53e91\n$s1, adb3d5c4\n$s2, 7b1bea67\n$s3, 966427bc\n$s4, 9569e26f\n$s5, 26a88cb8\n"
   .byte 0
   .text
   li $a0, src_message1032
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $ra, 0x3c251bcc
   li $s0, 0xe3f53e91
   li $s1, 0xadb3d5c4
   li $s2, 0x7b1bea67
   li $s3, 0x966427bc
   li $s4, 0x9569e26f
   li $s5, 0x26a88cb8
   save[16]  32, $ra, $s0, $s1, $s2, $s3, $s4, $s5
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1032:
   .ascii "save[16]  32, $ra, $s0, $s1, $s2, $s3, $s4, $s5   %d\n"
   .byte 0
   .text
   li $a0, message1032
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[16]  32, $ra, $s0
   .data
   src_message1033:
   .ascii "$ra, 0565def4\n$s0, f2609528\n"
   .byte 0
   .text
   li $a0, src_message1033
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $ra, 0x565def4
   li $s0, 0xf2609528
   save[16]  32, $ra, $s0
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1033:
   .ascii "save[16]  32, $ra, $s0   %d\n"
   .byte 0
   .text
   li $a0, message1033
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[16]  0, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6
   .data
   src_message1034:
   .ascii "$ra, 923806e4\n$s0, 8d3e51c9\n$s1, a2ebb62a\n$s2, c88fd690\n$s3, 8063ba58\n$s4, b95e13c8\n$s5, 4e3de233\n$s6, dbf7e475\n"
   .byte 0
   .text
   li $a0, src_message1034
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $ra, 0x923806e4
   li $s0, 0x8d3e51c9
   li $s1, 0xa2ebb62a
   li $s2, 0xc88fd690
   li $s3, 0x8063ba58
   li $s4, 0xb95e13c8
   li $s5, 0x4e3de233
   li $s6, 0xdbf7e475
   save[16]  32, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1034:
   .ascii "save[16]  32, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6   %d\n"
   .byte 0
   .text
   li $a0, message1034
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[16]  208, $fp, $ra, $s0, $s1, $s2, $s3
   .data
   src_message1035:
   .ascii "$fp, e1962a24\n$ra, 58889c30\n$s0, fed1758e\n$s1, f6d7fc53\n$s2, 7d4ec880\n$s3, 515b21a0\n"
   .byte 0
   .text
   li $a0, src_message1035
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $fp, 0xe1962a24
   li $ra, 0x58889c30
   li $s0, 0xfed1758e
   li $s1, 0xf6d7fc53
   li $s2, 0x7d4ec880
   li $s3, 0x515b21a0
   save[16]  208, $fp, $ra, $s0, $s1, $s2, $s3
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1035:
   .ascii "save[16]  208, $fp, $ra, $s0, $s1, $s2, $s3   %d\n"
   .byte 0
   .text
   li $a0, message1035
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[32]  616, $s3, $gp
   .data
   src_message1036:
   .ascii "$s3, 13a56f08\n$gp, e3a42263\n"
   .byte 0
   .text
   li $a0, src_message1036
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li $sp, test_data2 + 64
   li $s3, 0x13a56f08
   li $gp, 0xe3a42263
   save[32]  616, $s3, $gp
   li $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1036:
   .ascii "save[32]  616, $s3, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1036
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[32]  3504, $t5
   .data
   src_message1037:
   .ascii "$t5, 37f0ee90\n"
   .byte 0
   .text
   li $a0, src_message1037
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $t8, $sp
   li  $sp, test_data2 + 64
   li $t5, 0x37f0ee90
   save[32]  3504, $t5
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1037:
   .ascii "save[32]  3504, $t5   %d\n"
   .byte 0
   .text
   li $a0, message1037
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[32]  3288, $t5, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1
   .data
   src_message1038:
   .ascii "$t5, 5c7b08e4\n$a0, f6fa6f06\n$a1, 072cc436\n$a2, e08d250f\n$a3, 7cf9c2b0\n$a4, b767f2a2\n$a5, 93401500\n$a6, 8c7ad451\n$a7, 589b34ba\n$t0, f71368b1\n$t1, 0bfd8b6e\n"
   .byte 0
   .text
   li $a0, src_message1038
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $t8, $sp
   li  $sp, test_data2 + 64
   li $t5, 0x5c7b08e4
   li $a0, 0xf6fa6f06
   li $a1, 0x72cc436
   li $a2, 0xe08d250f
   li $a3, 0x7cf9c2b0
   li $a4, 0xb767f2a2
   li $a5, 0x93401500
   li $a6, 0x8c7ad451
   li $a7, 0x589b34ba
   li $t0, 0xf71368b1
   li $t1, 0xbfd8b6e
   save[32]  3288, $t5, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   .data
   message1038:
   .ascii "save[32]  3288, $t5, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1   %d\n"
   .byte 0
   .text
   li $a0, message1038
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[32]  560, $s7, $t8, $gp
   .data
   src_message1039:
   .ascii "$s7, 00ad63b4\n$t8, 8dc2b7c5\n$gp, 9f979d78\n"
   .byte 0
   .text
   li $a0, src_message1039
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $a0, $sp
   move $a0, $sp
   sw $gp, 4($a0)
   sw $fp, 8($a0)
   li  $sp, test_data2 + 64
   li $s7, 0xad63b4
   li $t8, 0x8dc2b7c5
   li $gp, 0x9f979d78
   save[32]  560, $s7, $t8, $gp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $a0
   move $sp, $a0
   lw $gp, 4($a0)
   lw $fp, 8($a0)
   .data
   message1039:
   .ascii "save[32]  560, $s7, $t8, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1039
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2

# save[32]  1960, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $gp
   .data
   src_message1040:
   .ascii "$a0, e3bc5b52\n$a1, e7a91b40\n$a2, 5b05bf18\n$a3, 459e6a80\n$a4, 68218fea\n$a5, 8be79d00\n$a6, f58513d4\n$a7, 111ff068\n$t0, 9738fc4d\n$t1, 7ed7aa28\n$t2, 854c725a\n$gp, 80d4db98\n"
   .byte 0
   .text
   li $a0, src_message1040
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   move $t8, $sp
   sw $gp, 4($t8)
   li  $sp, test_data2 + 64
   li $a0, 0xe3bc5b52
   li $a1, 0xe7a91b40
   li $a2, 0x5b05bf18
   li $a3, 0x459e6a80
   li $a4, 0x68218fea
   li $a5, 0x8be79d00
   li $a6, 0xf58513d4
   li $a7, 0x111ff068
   li $t0, 0x9738fc4d
   li $t1, 0x7ed7aa28
   li $t2, 0x854c725a
   li $gp, 0x80d4db98
   save[32]  1960, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $gp
   li  $a1, test_data2
   subu $a1, $a1, $sp
   move $sp, $t8
   lw $gp, 4($t8)
   .data
   message1040:
   .ascii "save[32]  1960, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $gp   %d\n"
   .byte 0
   .text
   li $a0, message1040
   lapc[48] $t9, mwrap_printf
   jalrc $t9
   balc print_test_data2
   balc cleanup_test_data2
   move $a0, $zero
   lw $ra, 0($sp)
   restore 16, $zero
   jrc   $ra
   .set  macro
   .set  reorder
   .end  main
   .size main, .-main
