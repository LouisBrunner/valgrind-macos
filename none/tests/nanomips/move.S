   .data
   test_ints:
       .word 0xFFFFFF65
       .word 0xFFFFFFFA
       .word 0x00000000
       .word 0x00000045
       .word 0x00000176
       .word 0x7FFFFFFF
       .word 0x80000000
       .word 0xFFFFFFFF

   .text
       .align   1

   .macro THREE_REG label, instruction, reg1, reg2, reg3, offset1, offset2, mem1, mem2
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x &reg3 %08x\n"
         .byte 0
       .text
         li $ra, &mem1
         lw &reg2, &offset1($ra)
         li $ra, &mem2
         lw &reg3, &offset2($ra)
         &instruction &reg1, &reg2, &reg3
         move $a1, &reg1
         li $a0, &label
         lw $a3, &offset2($ra)
         li $ra, &mem1
         lw $a2, &offset1($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro TWO_REG_1_IMM label, instruction, reg1, reg2, offset, mem, imm
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x, &imm\n"
         .byte 0
       .text
         li $ra, &mem
         lw &reg2, &offset($ra)
         &instruction &reg1, &reg2, &imm
         move $a1, &reg1
         li $a0, &label
         lw $a2, &offset($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro THREE_REG_A label, instruction, reg1, reg2, reg3, offset1, offset2, offset3, mem1, mem2, mem3
       .data
       &label:
         .ascii "&instruction &reg1 %08x old %08x &reg2 %08x &reg3 %08x\n"
         .byte 0
       .text
         li $ra, &mem1
         lw &reg1, &offset1($ra)
         li $ra, &mem2
         lw &reg2, &offset2($ra)
         li $ra, &mem3
         lw &reg3, &offset3($ra)
         &instruction &reg1, &reg2, &reg3
         move $a1, &reg1
         li $a0, &label
         lw $a4, &offset3($ra)
         li $ra, &mem1
         lw $a2, &offset1($ra)
         li $ra, &mem2
         lw $a3, &offset2($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro FOUR_REG label, instruction, reg1, reg2, reg3, reg4, offset1, offset2, mem1, mem2
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x &reg3 %08x &reg4 %08x\n"
         .byte 0
       .text
         li $ra, &mem1
         lw &reg3, &offset1($ra)
         li $ra, &mem2
         lw &reg4, &offset2($ra)
         &instruction &reg1, &reg2, &reg3, &reg4
         sw &reg2, 4($sp)
         move $a1, &reg1
         li $a0, &label
         lw $a2, 4($sp)
         lw $a4, &offset2($ra)
         li $ra, &mem1
         lw $a3, &offset1($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm


   .macro MOVE_BALC label, label2, reg1, reg2, offset, mem
       .data
       &label:
         .ascii "move.balc &reg1 %08x &reg2 %08x, next_instruction\n"
         .byte 0
       .text
         li $ra, &mem
         lw &reg2, &offset($ra)
         move.balc &reg1, &reg2, &label2
       &label2:
         move $a1, &reg1
         li $a0, &label
         li $ra, &mem
         lw $a2, &offset($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .globl main
   .ent   main
   .type  main, @function
main:
   .set  noreorder
   .set  nomacro
   save  16, $ra

# slt
   THREE_REG message1, slt, $a3, $s7, $s5, 20, 16, test_ints, test_ints
   THREE_REG message2, slt, $a6, $t2, $t3, 4, 8, test_ints, test_ints
   THREE_REG message3, slt, $a6, $a5, $t1, 28, 12, test_ints, test_ints
   THREE_REG message4, slt, $a3, $s1, $a3, 4, 8, test_ints, test_ints
   THREE_REG message5, slt, $s0, $s3, $a2, 16, 28, test_ints, test_ints
   THREE_REG message6, slt, $a0, $s5, $s0, 8, 8, test_ints, test_ints
   THREE_REG message7, slt, $s7, $a2, $s6, 0, 28, test_ints, test_ints
   THREE_REG message8, slt, $a6, $a7, $a5, 4, 0, test_ints, test_ints
   THREE_REG message9, slt, $s7, $a3, $s1, 0, 24, test_ints, test_ints
   THREE_REG message10, slt, $t0, $s0, $a4, 0, 4, test_ints, test_ints
   THREE_REG message11, slt, $s6, $a0, $a4, 16, 4, test_ints, test_ints
   THREE_REG message12, slt, $s0, $s0, $s2, 0, 28, test_ints, test_ints
   THREE_REG message13, slt, $s1, $a3, $a1, 12, 28, test_ints, test_ints
   THREE_REG message14, slt, $s0, $a1, $a2, 24, 8, test_ints, test_ints
   THREE_REG message15, slt, $a7, $a1, $a1, 0, 24, test_ints, test_ints
   THREE_REG message16, slt, $s7, $a5, $s6, 28, 20, test_ints, test_ints
   THREE_REG message17, slt, $a7, $a6, $t1, 28, 24, test_ints, test_ints
   THREE_REG message18, slt, $s6, $s3, $s7, 4, 28, test_ints, test_ints
   THREE_REG message19, slt, $s6, $a6, $a6, 12, 20, test_ints, test_ints
   THREE_REG message20, slt, $s2, $s7, $s6, 16, 4, test_ints, test_ints
   THREE_REG message21, slt, $a4, $s3, $s7, 4, 16, test_ints, test_ints
   THREE_REG message22, slt, $a1, $s4, $a5, 12, 0, test_ints, test_ints
   THREE_REG message23, slt, $s6, $s6, $a2, 28, 4, test_ints, test_ints
   THREE_REG message24, slt, $s0, $a6, $a1, 12, 12, test_ints, test_ints
   THREE_REG message25, slt, $s4, $s1, $a1, 28, 0, test_ints, test_ints
   THREE_REG message26, slt, $a2, $a1, $a4, 0, 4, test_ints, test_ints
   THREE_REG message27, slt, $t1, $s4, $s1, 16, 4, test_ints, test_ints
   THREE_REG message28, slt, $s1, $s5, $a1, 8, 16, test_ints, test_ints
   THREE_REG message29, slt, $a5, $a0, $a6, 12, 28, test_ints, test_ints
   THREE_REG message30, slt, $t0, $s0, $a1, 4, 28, test_ints, test_ints
   THREE_REG message31, slt, $a4, $s5, $s5, 4, 0, test_ints, test_ints
   THREE_REG message32, slt, $a1, $t3, $t1, 4, 12, test_ints, test_ints
   THREE_REG message33, slt, $s7, $a7, $s3, 16, 12, test_ints, test_ints
   THREE_REG message34, slt, $a0, $a5, $t1, 24, 28, test_ints, test_ints
   THREE_REG message35, slt, $s1, $s3, $a3, 0, 12, test_ints, test_ints
   THREE_REG message36, slt, $a6, $t0, $t3, 0, 4, test_ints, test_ints
   THREE_REG message37, slt, $a2, $a4, $s2, 12, 24, test_ints, test_ints
   THREE_REG message38, slt, $a7, $a1, $a1, 0, 8, test_ints, test_ints
   THREE_REG message39, slt, $a1, $t3, $s1, 0, 12, test_ints, test_ints
   THREE_REG message40, slt, $a1, $t1, $a0, 8, 12, test_ints, test_ints

# sltu
   THREE_REG message41, sltu, $a3, $t3, $a2, 24, 12, test_ints, test_ints
   THREE_REG message42, sltu, $a5, $a5, $s7, 16, 20, test_ints, test_ints
   THREE_REG message43, sltu, $s4, $t3, $s1, 28, 24, test_ints, test_ints
   THREE_REG message44, sltu, $s3, $a2, $s3, 4, 8, test_ints, test_ints
   THREE_REG message45, sltu, $a2, $t2, $t2, 12, 24, test_ints, test_ints
   THREE_REG message46, sltu, $a5, $a4, $s3, 24, 8, test_ints, test_ints
   THREE_REG message47, sltu, $a2, $a1, $a6, 4, 16, test_ints, test_ints
   THREE_REG message48, sltu, $a5, $a2, $a5, 20, 12, test_ints, test_ints
   THREE_REG message49, sltu, $a2, $a5, $t2, 28, 0, test_ints, test_ints
   THREE_REG message50, sltu, $t0, $s7, $a2, 16, 0, test_ints, test_ints
   THREE_REG message51, sltu, $a1, $s6, $s2, 28, 4, test_ints, test_ints
   THREE_REG message52, sltu, $a4, $t0, $s6, 0, 8, test_ints, test_ints
   THREE_REG message53, sltu, $a0, $t2, $s0, 8, 28, test_ints, test_ints
   THREE_REG message54, sltu, $s0, $t0, $s2, 20, 4, test_ints, test_ints
   THREE_REG message55, sltu, $s1, $a7, $s6, 28, 8, test_ints, test_ints
   THREE_REG message56, sltu, $s7, $s7, $a5, 20, 28, test_ints, test_ints
   THREE_REG message57, sltu, $t1, $s2, $a1, 28, 4, test_ints, test_ints
   THREE_REG message58, sltu, $t3, $a4, $s2, 4, 16, test_ints, test_ints
   THREE_REG message59, sltu, $a4, $a5, $s2, 0, 0, test_ints, test_ints
   THREE_REG message60, sltu, $t2, $s4, $a0, 0, 4, test_ints, test_ints
   THREE_REG message61, sltu, $a1, $s1, $a4, 28, 0, test_ints, test_ints
   THREE_REG message62, sltu, $s7, $a2, $s7, 0, 0, test_ints, test_ints
   THREE_REG message63, sltu, $a3, $s2, $s6, 0, 4, test_ints, test_ints
   THREE_REG message64, sltu, $t0, $a3, $a1, 8, 0, test_ints, test_ints
   THREE_REG message65, sltu, $s5, $a2, $s6, 16, 12, test_ints, test_ints
   THREE_REG message66, sltu, $t2, $s6, $t3, 8, 24, test_ints, test_ints
   THREE_REG message67, sltu, $t1, $s3, $s7, 20, 24, test_ints, test_ints
   THREE_REG message68, sltu, $a3, $s0, $a1, 8, 20, test_ints, test_ints
   THREE_REG message69, sltu, $a5, $s5, $s3, 12, 24, test_ints, test_ints
   THREE_REG message70, sltu, $a0, $s7, $s5, 24, 4, test_ints, test_ints
   THREE_REG message71, sltu, $a2, $a7, $s0, 16, 12, test_ints, test_ints
   THREE_REG message72, sltu, $s7, $a6, $s1, 24, 16, test_ints, test_ints
   THREE_REG message73, sltu, $s7, $a3, $t3, 24, 20, test_ints, test_ints
   THREE_REG message74, sltu, $t1, $a5, $t1, 8, 0, test_ints, test_ints
   THREE_REG message75, sltu, $t2, $s7, $a1, 20, 28, test_ints, test_ints
   THREE_REG message76, sltu, $s3, $s2, $a2, 4, 16, test_ints, test_ints
   THREE_REG message77, sltu, $t0, $t3, $a3, 16, 12, test_ints, test_ints
   THREE_REG message78, sltu, $a7, $s3, $s5, 0, 4, test_ints, test_ints
   THREE_REG message79, sltu, $a1, $a4, $a1, 16, 24, test_ints, test_ints
   THREE_REG message80, sltu, $s2, $s1, $a4, 28, 0, test_ints, test_ints

# slti
   TWO_REG_1_IMM message81, slti, $a0, $a6, 12, test_ints, 1793
   TWO_REG_1_IMM message82, slti, $a7, $a2, 20, test_ints, 2473
   TWO_REG_1_IMM message83, slti, $a5, $s2, 20, test_ints, 777
   TWO_REG_1_IMM message84, slti, $s1, $s1, 20, test_ints, 856
   TWO_REG_1_IMM message85, slti, $s0, $s4, 20, test_ints, 2648
   TWO_REG_1_IMM message86, slti, $s1, $a2, 0, test_ints, 2034
   TWO_REG_1_IMM message87, slti, $s6, $t3, 0, test_ints, 3335
   TWO_REG_1_IMM message88, slti, $a3, $a0, 28, test_ints, 1551
   TWO_REG_1_IMM message89, slti, $a6, $s7, 0, test_ints, 1153
   TWO_REG_1_IMM message90, slti, $t1, $a5, 12, test_ints, 646
   TWO_REG_1_IMM message91, slti, $s7, $a0, 28, test_ints, 2208
   TWO_REG_1_IMM message92, slti, $a1, $s0, 0, test_ints, 2561
   TWO_REG_1_IMM message93, slti, $a0, $a5, 8, test_ints, 1910
   TWO_REG_1_IMM message94, slti, $t3, $a2, 0, test_ints, 4037
   TWO_REG_1_IMM message95, slti, $a1, $t1, 20, test_ints, 268
   TWO_REG_1_IMM message96, slti, $t1, $s0, 12, test_ints, 2919
   TWO_REG_1_IMM message97, slti, $a3, $s4, 0, test_ints, 3837
   TWO_REG_1_IMM message98, slti, $s1, $a7, 12, test_ints, 949
   TWO_REG_1_IMM message99, slti, $a0, $t3, 20, test_ints, 3357
   TWO_REG_1_IMM message100, slti, $a7, $s2, 28, test_ints, 4028
   TWO_REG_1_IMM message101, slti, $a7, $s5, 8, test_ints, 3991
   TWO_REG_1_IMM message102, slti, $s7, $a6, 16, test_ints, 1349
   TWO_REG_1_IMM message103, slti, $a3, $a5, 4, test_ints, 420
   TWO_REG_1_IMM message104, slti, $s6, $a1, 12, test_ints, 2693
   TWO_REG_1_IMM message105, slti, $s5, $a0, 8, test_ints, 1634
   TWO_REG_1_IMM message106, slti, $t3, $s6, 28, test_ints, 1995
   TWO_REG_1_IMM message107, slti, $s5, $a5, 4, test_ints, 936
   TWO_REG_1_IMM message108, slti, $a7, $t0, 16, test_ints, 686
   TWO_REG_1_IMM message109, slti, $s1, $a6, 20, test_ints, 1484
   TWO_REG_1_IMM message110, slti, $s5, $a6, 4, test_ints, 2412

# sltiu
   TWO_REG_1_IMM message111, sltiu, $s3, $s7, 4, test_ints, 2369
   TWO_REG_1_IMM message112, sltiu, $t0, $s4, 28, test_ints, 105
   TWO_REG_1_IMM message113, sltiu, $a4, $s5, 12, test_ints, 24
   TWO_REG_1_IMM message114, sltiu, $t3, $a7, 12, test_ints, 2664
   TWO_REG_1_IMM message115, sltiu, $a4, $t0, 4, test_ints, 1547
   TWO_REG_1_IMM message116, sltiu, $s4, $a1, 4, test_ints, 85
   TWO_REG_1_IMM message117, sltiu, $s0, $s3, 8, test_ints, 3389
   TWO_REG_1_IMM message118, sltiu, $s1, $a7, 4, test_ints, 2668
   TWO_REG_1_IMM message119, sltiu, $a2, $a6, 24, test_ints, 358
   TWO_REG_1_IMM message120, sltiu, $t3, $s1, 28, test_ints, 3815
   TWO_REG_1_IMM message121, sltiu, $s6, $s7, 28, test_ints, 3342
   TWO_REG_1_IMM message122, sltiu, $s6, $s7, 24, test_ints, 206
   TWO_REG_1_IMM message123, sltiu, $s3, $a7, 4, test_ints, 3204
   TWO_REG_1_IMM message124, sltiu, $s1, $a6, 4, test_ints, 777
   TWO_REG_1_IMM message125, sltiu, $s1, $s3, 24, test_ints, 1842
   TWO_REG_1_IMM message126, sltiu, $s3, $a3, 28, test_ints, 1733
   TWO_REG_1_IMM message127, sltiu, $s2, $a1, 16, test_ints, 641
   TWO_REG_1_IMM message128, sltiu, $a2, $a7, 0, test_ints, 384
   TWO_REG_1_IMM message129, sltiu, $s6, $s0, 24, test_ints, 3092
   TWO_REG_1_IMM message130, sltiu, $a3, $a1, 8, test_ints, 2810
   TWO_REG_1_IMM message131, sltiu, $s0, $a7, 24, test_ints, 649
   TWO_REG_1_IMM message132, sltiu, $a6, $s4, 8, test_ints, 3551
   TWO_REG_1_IMM message133, sltiu, $s7, $s0, 8, test_ints, 2946
   TWO_REG_1_IMM message134, sltiu, $a4, $a5, 0, test_ints, 1394
   TWO_REG_1_IMM message135, sltiu, $t2, $a0, 12, test_ints, 2240
   TWO_REG_1_IMM message136, sltiu, $a7, $s7, 0, test_ints, 1798
   TWO_REG_1_IMM message137, sltiu, $a3, $t3, 8, test_ints, 270
   TWO_REG_1_IMM message138, sltiu, $a0, $s1, 4, test_ints, 2656
   TWO_REG_1_IMM message139, sltiu, $t0, $s7, 8, test_ints, 1798
   TWO_REG_1_IMM message140, sltiu, $t3, $t0, 24, test_ints, 3891

# sov
   THREE_REG message141, sov, $a5, $t0, $s5, 20, 4, test_ints, test_ints
   THREE_REG message142, sov, $t1, $a7, $s3, 4, 8, test_ints, test_ints
   THREE_REG message143, sov, $s3, $t1, $a1, 28, 28, test_ints, test_ints
   THREE_REG message144, sov, $t1, $t2, $a1, 28, 8, test_ints, test_ints
   THREE_REG message145, sov, $s2, $s0, $t3, 28, 0, test_ints, test_ints
   THREE_REG message146, sov, $t1, $s5, $s7, 4, 12, test_ints, test_ints
   THREE_REG message147, sov, $a2, $s2, $t3, 0, 12, test_ints, test_ints
   THREE_REG message148, sov, $s4, $t1, $s2, 12, 28, test_ints, test_ints
   THREE_REG message149, sov, $s0, $t2, $a4, 8, 8, test_ints, test_ints
   THREE_REG message150, sov, $s7, $t3, $s0, 0, 8, test_ints, test_ints
   THREE_REG message151, sov, $a7, $t3, $a3, 24, 24, test_ints, test_ints
   THREE_REG message152, sov, $s3, $s7, $s3, 8, 16, test_ints, test_ints
   THREE_REG message153, sov, $a7, $a1, $a3, 8, 20, test_ints, test_ints
   THREE_REG message154, sov, $a6, $a3, $a6, 4, 8, test_ints, test_ints
   THREE_REG message155, sov, $s5, $t1, $t1, 4, 28, test_ints, test_ints
   THREE_REG message156, sov, $a7, $a4, $t2, 28, 4, test_ints, test_ints
   THREE_REG message157, sov, $a5, $s6, $a0, 0, 0, test_ints, test_ints
   THREE_REG message158, sov, $s2, $s3, $t3, 4, 20, test_ints, test_ints
   THREE_REG message159, sov, $a0, $a0, $s2, 12, 12, test_ints, test_ints
   THREE_REG message160, sov, $s3, $a1, $a2, 8, 8, test_ints, test_ints
   THREE_REG message161, sov, $s4, $s7, $a0, 4, 20, test_ints, test_ints
   THREE_REG message162, sov, $t3, $s0, $s1, 8, 0, test_ints, test_ints
   THREE_REG message163, sov, $a2, $a7, $s2, 24, 28, test_ints, test_ints
   THREE_REG message164, sov, $a3, $a4, $t2, 24, 24, test_ints, test_ints
   THREE_REG message165, sov, $a7, $s6, $s2, 8, 4, test_ints, test_ints
   THREE_REG message166, sov, $a1, $a1, $a3, 12, 12, test_ints, test_ints
   THREE_REG message167, sov, $a5, $s0, $t3, 20, 4, test_ints, test_ints
   THREE_REG message168, sov, $t0, $t1, $t2, 4, 28, test_ints, test_ints
   THREE_REG message169, sov, $s6, $a0, $a6, 16, 24, test_ints, test_ints
   THREE_REG message170, sov, $a5, $t3, $s3, 28, 24, test_ints, test_ints
   THREE_REG message171, sov, $s5, $a2, $s0, 28, 0, test_ints, test_ints
   THREE_REG message172, sov, $t2, $a5, $a6, 4, 16, test_ints, test_ints
   THREE_REG message173, sov, $s1, $t2, $s0, 16, 16, test_ints, test_ints
   THREE_REG message174, sov, $t2, $t0, $a5, 0, 8, test_ints, test_ints
   THREE_REG message175, sov, $t0, $s6, $s6, 24, 24, test_ints, test_ints
   THREE_REG message176, sov, $a0, $t3, $s2, 12, 8, test_ints, test_ints
   THREE_REG message177, sov, $s4, $t0, $t0, 0, 16, test_ints, test_ints
   THREE_REG message178, sov, $a1, $s2, $t1, 12, 28, test_ints, test_ints
   THREE_REG message179, sov, $s1, $s0, $s6, 24, 20, test_ints, test_ints
   THREE_REG message180, sov, $a2, $s0, $a5, 12, 16, test_ints, test_ints

# movn
   THREE_REG_A message181, movn, $s3, $a3, $a2, 4, 4, 16, test_ints, test_ints, test_ints
   THREE_REG_A message182, movn, $t2, $t0, $a2, 20, 24, 24, test_ints, test_ints, test_ints
   THREE_REG_A message183, movn, $s2, $t2, $t3, 24, 28, 20, test_ints, test_ints, test_ints
   THREE_REG_A message184, movn, $a3, $t2, $a5, 16, 12, 12, test_ints, test_ints, test_ints
   THREE_REG_A message185, movn, $a2, $s0, $s5, 24, 20, 0, test_ints, test_ints, test_ints
   THREE_REG_A message186, movn, $s2, $t1, $s7, 0, 24, 16, test_ints, test_ints, test_ints
   THREE_REG_A message187, movn, $t1, $s4, $a0, 28, 24, 24, test_ints, test_ints, test_ints
   THREE_REG_A message188, movn, $t2, $s0, $s4, 20, 24, 16, test_ints, test_ints, test_ints
   THREE_REG_A message189, movn, $s2, $a5, $s6, 28, 20, 4, test_ints, test_ints, test_ints
   THREE_REG_A message190, movn, $t2, $s4, $a1, 28, 24, 28, test_ints, test_ints, test_ints
   THREE_REG_A message191, movn, $s7, $t1, $a4, 8, 4, 8, test_ints, test_ints, test_ints
   THREE_REG_A message192, movn, $s6, $s2, $s3, 24, 24, 4, test_ints, test_ints, test_ints
   THREE_REG_A message193, movn, $a0, $s4, $s1, 4, 4, 28, test_ints, test_ints, test_ints
   THREE_REG_A message194, movn, $a1, $a3, $s0, 12, 12, 20, test_ints, test_ints, test_ints
   THREE_REG_A message195, movn, $s1, $s1, $s5, 24, 20, 16, test_ints, test_ints, test_ints
   THREE_REG_A message196, movn, $s1, $a0, $s1, 4, 28, 24, test_ints, test_ints, test_ints
   THREE_REG_A message197, movn, $a4, $t1, $s5, 28, 16, 12, test_ints, test_ints, test_ints
   THREE_REG_A message198, movn, $t0, $t0, $s3, 4, 4, 0, test_ints, test_ints, test_ints
   THREE_REG_A message199, movn, $s0, $s6, $t3, 0, 8, 24, test_ints, test_ints, test_ints
   THREE_REG_A message200, movn, $t1, $s3, $s4, 12, 20, 4, test_ints, test_ints, test_ints
   THREE_REG_A message201, movn, $a3, $a3, $t1, 16, 16, 16, test_ints, test_ints, test_ints
   THREE_REG_A message202, movn, $a6, $a4, $s2, 28, 28, 24, test_ints, test_ints, test_ints
   THREE_REG_A message203, movn, $s2, $a7, $s6, 4, 0, 0, test_ints, test_ints, test_ints
   THREE_REG_A message204, movn, $t1, $a0, $s6, 20, 0, 0, test_ints, test_ints, test_ints
   THREE_REG_A message205, movn, $s3, $t2, $a7, 28, 4, 20, test_ints, test_ints, test_ints
   THREE_REG_A message206, movn, $s4, $a4, $s0, 24, 16, 16, test_ints, test_ints, test_ints
   THREE_REG_A message207, movn, $s6, $s6, $s5, 16, 24, 16, test_ints, test_ints, test_ints
   THREE_REG_A message208, movn, $a2, $t0, $s4, 4, 8, 16, test_ints, test_ints, test_ints
   THREE_REG_A message209, movn, $t1, $a7, $t1, 28, 0, 20, test_ints, test_ints, test_ints
   THREE_REG_A message210, movn, $s0, $a0, $s7, 28, 12, 16, test_ints, test_ints, test_ints
   THREE_REG_A message211, movn, $a0, $s0, $a0, 16, 8, 0, test_ints, test_ints, test_ints
   THREE_REG_A message212, movn, $t1, $t0, $a3, 24, 4, 20, test_ints, test_ints, test_ints
   THREE_REG_A message213, movn, $s6, $s7, $a5, 24, 16, 28, test_ints, test_ints, test_ints
   THREE_REG_A message214, movn, $s3, $a1, $s7, 0, 20, 12, test_ints, test_ints, test_ints
   THREE_REG_A message215, movn, $t1, $s5, $s7, 4, 16, 12, test_ints, test_ints, test_ints
   THREE_REG_A message216, movn, $s1, $a1, $s3, 8, 20, 4, test_ints, test_ints, test_ints
   THREE_REG_A message217, movn, $s2, $a6, $s5, 4, 16, 24, test_ints, test_ints, test_ints
   THREE_REG_A message218, movn, $a6, $a3, $s6, 16, 20, 24, test_ints, test_ints, test_ints
   THREE_REG_A message219, movn, $s3, $s4, $a0, 24, 0, 4, test_ints, test_ints, test_ints
   THREE_REG_A message220, movn, $s6, $a2, $t2, 4, 28, 8, test_ints, test_ints, test_ints

# movz
   THREE_REG_A message221, movz, $s4, $t0, $s3, 12, 24, 4, test_ints, test_ints, test_ints
   THREE_REG_A message222, movz, $s0, $a1, $t3, 8, 8, 16, test_ints, test_ints, test_ints
   THREE_REG_A message223, movz, $a0, $a5, $a3, 8, 20, 0, test_ints, test_ints, test_ints
   THREE_REG_A message224, movz, $a5, $s4, $s1, 20, 28, 4, test_ints, test_ints, test_ints
   THREE_REG_A message225, movz, $a2, $a5, $s7, 16, 8, 24, test_ints, test_ints, test_ints
   THREE_REG_A message226, movz, $a2, $a7, $t3, 8, 8, 4, test_ints, test_ints, test_ints
   THREE_REG_A message227, movz, $a7, $s3, $a2, 8, 4, 20, test_ints, test_ints, test_ints
   THREE_REG_A message228, movz, $t2, $a5, $t2, 4, 16, 12, test_ints, test_ints, test_ints
   THREE_REG_A message229, movz, $t2, $t1, $s3, 12, 8, 8, test_ints, test_ints, test_ints
   THREE_REG_A message230, movz, $a0, $s0, $a7, 16, 16, 24, test_ints, test_ints, test_ints
   THREE_REG_A message231, movz, $s2, $s6, $s5, 20, 16, 28, test_ints, test_ints, test_ints
   THREE_REG_A message232, movz, $s3, $a7, $t2, 4, 8, 28, test_ints, test_ints, test_ints
   THREE_REG_A message233, movz, $t2, $t0, $a1, 16, 8, 20, test_ints, test_ints, test_ints
   THREE_REG_A message234, movz, $a7, $a0, $a2, 12, 28, 16, test_ints, test_ints, test_ints
   THREE_REG_A message235, movz, $a5, $a7, $t0, 4, 28, 0, test_ints, test_ints, test_ints
   THREE_REG_A message236, movz, $s3, $a6, $s2, 16, 28, 28, test_ints, test_ints, test_ints
   THREE_REG_A message237, movz, $t3, $s2, $s2, 8, 0, 0, test_ints, test_ints, test_ints
   THREE_REG_A message238, movz, $s5, $a6, $t1, 8, 12, 12, test_ints, test_ints, test_ints
   THREE_REG_A message239, movz, $t3, $s6, $t3, 4, 20, 24, test_ints, test_ints, test_ints
   THREE_REG_A message240, movz, $s1, $a3, $a5, 4, 16, 4, test_ints, test_ints, test_ints
   THREE_REG_A message241, movz, $s5, $s3, $s3, 16, 28, 8, test_ints, test_ints, test_ints
   THREE_REG_A message242, movz, $t3, $a6, $a1, 4, 16, 4, test_ints, test_ints, test_ints
   THREE_REG_A message243, movz, $s2, $t2, $a7, 12, 0, 8, test_ints, test_ints, test_ints
   THREE_REG_A message244, movz, $t2, $a4, $s1, 20, 4, 8, test_ints, test_ints, test_ints
   THREE_REG_A message245, movz, $s7, $s7, $a1, 16, 0, 4, test_ints, test_ints, test_ints
   THREE_REG_A message246, movz, $a5, $a6, $a4, 0, 24, 12, test_ints, test_ints, test_ints
   THREE_REG_A message247, movz, $t3, $s5, $a6, 16, 8, 8, test_ints, test_ints, test_ints
   THREE_REG_A message248, movz, $a1, $a4, $s4, 16, 28, 4, test_ints, test_ints, test_ints
   THREE_REG_A message249, movz, $a3, $s5, $a1, 16, 8, 8, test_ints, test_ints, test_ints
   THREE_REG_A message250, movz, $t2, $s1, $a5, 16, 8, 8, test_ints, test_ints, test_ints
   THREE_REG_A message251, movz, $s5, $t3, $t0, 8, 0, 8, test_ints, test_ints, test_ints
   THREE_REG_A message252, movz, $s1, $a7, $a3, 12, 28, 20, test_ints, test_ints, test_ints
   THREE_REG_A message253, movz, $s2, $s4, $a6, 24, 0, 4, test_ints, test_ints, test_ints
   THREE_REG_A message254, movz, $s3, $a7, $a7, 0, 12, 4, test_ints, test_ints, test_ints
   THREE_REG_A message255, movz, $a3, $s2, $s3, 16, 24, 4, test_ints, test_ints, test_ints
   THREE_REG_A message256, movz, $t2, $a7, $a4, 8, 20, 0, test_ints, test_ints, test_ints
   THREE_REG_A message257, movz, $t0, $s7, $a3, 28, 24, 8, test_ints, test_ints, test_ints
   THREE_REG_A message258, movz, $a1, $t0, $a6, 28, 12, 28, test_ints, test_ints, test_ints
   THREE_REG_A message259, movz, $a0, $s6, $a6, 28, 12, 8, test_ints, test_ints, test_ints
   THREE_REG_A message260, movz, $s1, $s6, $a0, 0, 8, 8, test_ints, test_ints, test_ints

# movep
   FOUR_REG message261, movep, $a2, $a3, $a0, $s7, 28, 28, test_ints, test_ints
   FOUR_REG message262, movep, $a0, $a1, $a3, $s1, 0, 4, test_ints, test_ints
   FOUR_REG message263, movep, $a0, $a1, $s4, $s7, 20, 16, test_ints, test_ints
   FOUR_REG message264, movep, $a2, $a3, $s3, $s1, 24, 28, test_ints, test_ints
   FOUR_REG message265, movep, $a2, $a3, $a1, $s5, 0, 20, test_ints, test_ints
   FOUR_REG message266, movep, $a2, $a3, $s2, $s3, 12, 24, test_ints, test_ints
   FOUR_REG message267, movep, $a0, $a1, $s7, $a1, 24, 0, test_ints, test_ints
   FOUR_REG message268, movep, $a1, $a2, $s2, $s1, 20, 28, test_ints, test_ints
   FOUR_REG message269, movep, $a1, $a2, $s4, $a0, 16, 8, test_ints, test_ints
   FOUR_REG message270, movep, $a3, $a4, $s4, $s0, 16, 16, test_ints, test_ints
   FOUR_REG message271, movep, $a1, $a2, $s6, $s0, 8, 8, test_ints, test_ints
   FOUR_REG message272, movep, $a2, $a3, $s5, $a5, 28, 12, test_ints, test_ints
   FOUR_REG message273, movep, $a1, $a2, $s7, $s2, 12, 12, test_ints, test_ints
   FOUR_REG message274, movep, $a2, $a3, $a5, $a1, 24, 8, test_ints, test_ints
   FOUR_REG message275, movep, $a1, $a2, $s6, $r0, 8, 20, test_ints, test_ints
   FOUR_REG message276, movep, $a3, $a4, $s7, $a5, 8, 16, test_ints, test_ints
   FOUR_REG message277, movep, $a1, $a2, $s6, $s2, 4, 20, test_ints, test_ints
   FOUR_REG message278, movep, $a0, $a1, $s0, $s6, 16, 24, test_ints, test_ints
   FOUR_REG message279, movep, $a1, $a2, $s7, $r0, 12, 24, test_ints, test_ints
   FOUR_REG message280, movep, $a3, $a4, $a6, $s7, 16, 12, test_ints, test_ints
   FOUR_REG message281, movep, $a1, $a2, $a5, $s2, 0, 4, test_ints, test_ints
   FOUR_REG message282, movep, $a3, $a4, $r0, $s3, 28, 24, test_ints, test_ints
   FOUR_REG message283, movep, $a1, $a2, $s7, $a3, 20, 24, test_ints, test_ints
   FOUR_REG message284, movep, $a3, $a4, $s3, $s2, 24, 8, test_ints, test_ints
   FOUR_REG message285, movep, $a1, $a2, $s5, $s2, 20, 12, test_ints, test_ints
   FOUR_REG message286, movep, $a3, $a4, $r0, $s6, 8, 16, test_ints, test_ints
   FOUR_REG message287, movep, $a1, $a2, $r0, $s2, 0, 8, test_ints, test_ints
   FOUR_REG message288, movep, $a3, $a4, $a0, $r0, 20, 12, test_ints, test_ints
   FOUR_REG message289, movep, $a1, $a2, $s5, $s3, 24, 12, test_ints, test_ints
   FOUR_REG message290, movep, $a1, $a2, $s6, $s2, 4, 8, test_ints, test_ints
   FOUR_REG message291, movep, $a2, $a3, $s6, $s5, 24, 4, test_ints, test_ints
   FOUR_REG message292, movep, $a2, $a3, $s3, $s1, 28, 12, test_ints, test_ints
   FOUR_REG message293, movep, $a0, $a1, $s5, $a6, 28, 8, test_ints, test_ints
   FOUR_REG message294, movep, $a0, $a1, $s4, $a6, 4, 8, test_ints, test_ints
   FOUR_REG message295, movep, $a3, $a4, $a6, $a2, 16, 16, test_ints, test_ints
   FOUR_REG message296, movep, $a0, $a1, $a5, $s4, 12, 24, test_ints, test_ints
   FOUR_REG message297, movep, $a1, $a2, $s0, $s1, 28, 20, test_ints, test_ints
   FOUR_REG message298, movep, $a0, $a1, $r0, $s2, 16, 16, test_ints, test_ints
   FOUR_REG message299, movep, $a0, $a1, $s4, $s0, 24, 0, test_ints, test_ints
   FOUR_REG message300, movep, $a3, $a4, $a6, $s7, 16, 24, test_ints, test_ints

# movep[rev]
   FOUR_REG message301, movep[rev], $s1, $s0, $a2, $a3, 16, 8, test_ints, test_ints
   FOUR_REG message302, movep[rev], $a0, $a6, $a3, $a4, 0, 28, test_ints, test_ints
   FOUR_REG message303, movep[rev], $s6, $s4, $a0, $a1, 20, 0, test_ints, test_ints
   FOUR_REG message304, movep[rev], $s7, $s1, $a0, $a1, 28, 12, test_ints, test_ints
   FOUR_REG message305, movep[rev], $s2, $s6, $a0, $a1, 20, 16, test_ints, test_ints
   FOUR_REG message306, movep[rev], $s7, $a6, $a0, $a1, 16, 0, test_ints, test_ints
   FOUR_REG message307, movep[rev], $s7, $s2, $a2, $a3, 12, 4, test_ints, test_ints
   FOUR_REG message308, movep[rev], $a7, $s5, $a3, $a4, 24, 16, test_ints, test_ints
   FOUR_REG message309, movep[rev], $a4, $s5, $a2, $a3, 28, 28, test_ints, test_ints
   FOUR_REG message310, movep[rev], $s1, $s3, $a3, $a4, 8, 16, test_ints, test_ints
   FOUR_REG message311, movep[rev], $a7, $s1, $a2, $a3, 0, 20, test_ints, test_ints
   FOUR_REG message312, movep[rev], $s4, $s2, $a0, $a1, 28, 0, test_ints, test_ints
   FOUR_REG message313, movep[rev], $a2, $a6, $a0, $a1, 12, 28, test_ints, test_ints
   FOUR_REG message314, movep[rev], $a3, $a2, $a1, $a2, 8, 16, test_ints, test_ints
   FOUR_REG message315, movep[rev], $a6, $s6, $a2, $a3, 4, 20, test_ints, test_ints
   FOUR_REG message316, movep[rev], $s1, $a0, $a1, $a2, 8, 4, test_ints, test_ints
   FOUR_REG message317, movep[rev], $s7, $a7, $a0, $a1, 28, 12, test_ints, test_ints
   FOUR_REG message318, movep[rev], $a2, $a4, $a3, $a4, 0, 4, test_ints, test_ints
   FOUR_REG message319, movep[rev], $a2, $a6, $a0, $a1, 16, 28, test_ints, test_ints
   FOUR_REG message320, movep[rev], $s6, $s5, $a2, $a3, 24, 12, test_ints, test_ints
   FOUR_REG message321, movep[rev], $a0, $a1, $a3, $a4, 20, 16, test_ints, test_ints
   FOUR_REG message322, movep[rev], $s3, $a6, $a0, $a1, 24, 28, test_ints, test_ints
   FOUR_REG message323, movep[rev], $a3, $s4, $a1, $a2, 16, 12, test_ints, test_ints
   FOUR_REG message324, movep[rev], $s6, $s3, $a1, $a2, 8, 8, test_ints, test_ints
   FOUR_REG message325, movep[rev], $s4, $a6, $a1, $a2, 4, 24, test_ints, test_ints
   FOUR_REG message326, movep[rev], $a4, $s4, $a2, $a3, 28, 12, test_ints, test_ints
   FOUR_REG message327, movep[rev], $a6, $s0, $a0, $a1, 20, 0, test_ints, test_ints
   FOUR_REG message328, movep[rev], $s2, $a6, $a2, $a3, 0, 8, test_ints, test_ints
   FOUR_REG message329, movep[rev], $a6, $s6, $a2, $a3, 16, 20, test_ints, test_ints
   FOUR_REG message330, movep[rev], $a4, $s2, $a2, $a3, 4, 12, test_ints, test_ints
   FOUR_REG message331, movep[rev], $s1, $s6, $a2, $a3, 8, 16, test_ints, test_ints
   FOUR_REG message332, movep[rev], $a5, $a3, $a0, $a1, 16, 0, test_ints, test_ints
   FOUR_REG message333, movep[rev], $s1, $a0, $a2, $a3, 0, 12, test_ints, test_ints
   FOUR_REG message334, movep[rev], $a5, $a0, $a1, $a2, 12, 8, test_ints, test_ints
   FOUR_REG message335, movep[rev], $a7, $s1, $a2, $a3, 20, 8, test_ints, test_ints
   FOUR_REG message336, movep[rev], $s6, $s2, $a0, $a1, 16, 8, test_ints, test_ints
   FOUR_REG message337, movep[rev], $a6, $s7, $a2, $a3, 12, 8, test_ints, test_ints
   FOUR_REG message338, movep[rev], $a0, $a7, $a2, $a3, 4, 24, test_ints, test_ints
   FOUR_REG message339, movep[rev], $s5, $s7, $a1, $a2, 4, 20, test_ints, test_ints
   FOUR_REG message340, movep[rev], $a5, $s5, $a1, $a2, 16, 20, test_ints, test_ints

# move.balc
   MOVE_BALC message341, label341, $a1, $s2, 16, test_ints
   MOVE_BALC message342, label342, $a1, $s4, 8, test_ints
   MOVE_BALC message343, label343, $a1, $s0, 4, test_ints
   MOVE_BALC message344, label344, $a0, $s3, 20, test_ints
   MOVE_BALC message345, label345, $a1, $s6, 8, test_ints
   MOVE_BALC message346, label346, $a0, $a5, 12, test_ints
   MOVE_BALC message347, label347, $a1, $a5, 0, test_ints
   MOVE_BALC message348, label348, $a1, $a3, 4, test_ints
   MOVE_BALC message349, label349, $a0, $a0, 20, test_ints
   MOVE_BALC message350, label350, $a1, $a0, 24, test_ints
   MOVE_BALC message351, label351, $a1, $r0, 8, test_ints
   MOVE_BALC message352, label352, $a1, $a1, 20, test_ints
   MOVE_BALC message353, label353, $a0, $a5, 24, test_ints
   MOVE_BALC message354, label354, $a1, $r0, 8, test_ints
   MOVE_BALC message355, label355, $a0, $s7, 0, test_ints
   MOVE_BALC message356, label356, $a0, $s4, 20, test_ints
   MOVE_BALC message357, label357, $a1, $s3, 8, test_ints
   MOVE_BALC message358, label358, $a0, $s1, 20, test_ints
   MOVE_BALC message359, label359, $a1, $a4, 20, test_ints
   MOVE_BALC message360, label360, $a0, $a4, 24, test_ints

   move $a0, $zero
   restore 16, $ra
   jrc   $ra
   .set  macro
   .set  reorder
   .end  main
   .size main, .-main
