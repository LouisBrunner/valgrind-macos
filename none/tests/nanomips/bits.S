   .data
   test_words:
       .word 0x0
       .word 0xFFFFFFFF
       .word 0x55555555
       .word 0xAAAAAAAA
       .word 0x33333333
       .word 0xCCCCCCCC
       .word 0x12345678
       .word 0x9ABCDEF
   test_shifts:
       .word 0
       .word 1
       .word 7
       .word 8
       .word 16
       .word 17
       .word 31
       .word 32

   .text
       .align   1

   .macro TWO_REG label, instruction, reg1, reg2, offset, mem
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x\n"
         .byte 0
       .text
         li $ra, &mem
         lw &reg2, &offset($ra)
         &instruction &reg1, &reg2
         move $a1, &reg1
         li $a0, &label
         lw $a2, &offset($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

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

   # One of source reg is also destination
   .macro TWO_REG_16 label, instruction, reg1, reg2, offset1, offset2, mem1, mem2
       .data
       &label:
         .ascii "&instruction &reg1 %08x old &reg1 %08x &reg2 %08x\n"
         .byte 0
       .text
         li $ra, &mem1
         lw &reg1, &offset1($ra)
         li $ra, &mem2
         lw &reg2, &offset2($ra)
         &instruction &reg1, &reg2
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

   .macro TWO_REG_2_IMM label, instruction, reg1, reg2, offset, mem, imm1, imm2
       .data
       &label:
         .ascii "&instruction &reg1 %08x &reg2 %08x, &imm1, &imm2\n"
         .byte 0
       .text
         li $ra, &mem
         lw &reg2, &offset($ra)
         &instruction &reg1, &reg2, &imm1, &imm2
         move $a1, &reg1
         li $a0, &label
         lw $a2, &offset($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro TWO_REG_2_IMM_B label, instruction, reg1, reg2, offset1, offset2, mem1, mem2, imm1, imm2
       .data
       &label:
         .ascii "&instruction &reg1 %08x old &reg1 %08x &reg2 %08x, &imm1, &imm2\n"
         .byte 0
         .text
      li $ra, &mem1
      lw &reg1, &offset1($ra)
      li $ra, &mem2
      lw &reg2, &offset2($ra)
      &instruction &reg1, &reg2, &imm1, &imm2
      move $a1, &reg1
      li $a0, &label
      lw $a3, &offset2($ra)
      li $ra, &mem1
      lw $a2, &offset1($ra)
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

   .globl main
   .ent   main
   .type  main, @function
main:
   .set  noreorder
   .set  nomacro
   save  16, $ra

# clo
   TWO_REG message1, clo, $a2, $s1, 28, test_words
   TWO_REG message2, clo, $s0, $a6, 20, test_words
   TWO_REG message3, clo, $t0, $s4, 12, test_words
   TWO_REG message4, clo, $t0, $a7, 12, test_words
   TWO_REG message5, clo, $s0, $a2, 28, test_words
   TWO_REG message6, clo, $s0, $s0, 20, test_words
   TWO_REG message7, clo, $s2, $s0, 8, test_words
   TWO_REG message8, clo, $a7, $s4, 4, test_words
   TWO_REG message9, clo, $a7, $s4, 4, test_words
   TWO_REG message10, clo, $s2, $s1, 28, test_words
   TWO_REG message11, clo, $s0, $a3, 4, test_words
   TWO_REG message12, clo, $s7, $s3, 28, test_words
   TWO_REG message13, clo, $s4, $t3, 28, test_words
   TWO_REG message14, clo, $a3, $t0, 8, test_words
   TWO_REG message15, clo, $s3, $a4, 20, test_words
   TWO_REG message16, clo, $a2, $a4, 4, test_words
   TWO_REG message17, clo, $s7, $a6, 20, test_words
   TWO_REG message18, clo, $s2, $s5, 8, test_words
   TWO_REG message19, clo, $s7, $a0, 8, test_words
   TWO_REG message20, clo, $a5, $s2, 28, test_words

# clz
   TWO_REG message21, clz, $a4, $s2, 12, test_words
   TWO_REG message22, clz, $s5, $a6, 24, test_words
   TWO_REG message23, clz, $s0, $s2, 8, test_words
   TWO_REG message24, clz, $t0, $s6, 24, test_words
   TWO_REG message25, clz, $s6, $t1, 24, test_words
   TWO_REG message26, clz, $s7, $s3, 24, test_words
   TWO_REG message27, clz, $a5, $a6, 0, test_words
   TWO_REG message28, clz, $a6, $s0, 20, test_words
   TWO_REG message29, clz, $a0, $a4, 20, test_words
   TWO_REG message30, clz, $a6, $a1, 12, test_words
   TWO_REG message31, clz, $s6, $a5, 4, test_words
   TWO_REG message32, clz, $a5, $t3, 28, test_words
   TWO_REG message33, clz, $t3, $a7, 8, test_words
   TWO_REG message34, clz, $s5, $s7, 16, test_words
   TWO_REG message35, clz, $s7, $s2, 4, test_words
   TWO_REG message36, clz, $s5, $a5, 0, test_words
   TWO_REG message37, clz, $s7, $a2, 24, test_words
   TWO_REG message38, clz, $a3, $s1, 28, test_words
   TWO_REG message39, clz, $a4, $s1, 12, test_words
   TWO_REG message40, clz, $t1, $t0, 16, test_words

# not
   TWO_REG message41, not, $a0, $s2, 4, test_words
   TWO_REG message42, not, $a2, $s3, 16, test_words
   TWO_REG message43, not, $a1, $s2, 16, test_words
   TWO_REG message44, not, $a3, $a0, 28, test_words
   TWO_REG message45, not, $s3, $s3, 4, test_words
   TWO_REG message46, not, $a0, $s1, 28, test_words
   TWO_REG message47, not, $a0, $a0, 4, test_words
   TWO_REG message48, not, $s3, $s0, 24, test_words
   TWO_REG message49, not, $s2, $s0, 0, test_words
   TWO_REG message50, not, $a1, $a2, 0, test_words
   TWO_REG message51, not, $s1, $a2, 24, test_words
   TWO_REG message52, not, $s2, $s0, 20, test_words
   TWO_REG message53, not, $a3, $s2, 12, test_words
   TWO_REG message54, not, $s3, $a1, 12, test_words
   TWO_REG message55, not, $s2, $a1, 12, test_words
   TWO_REG message56, not, $a0, $a1, 0, test_words
   TWO_REG message57, not, $s3, $a2, 0, test_words
   TWO_REG message58, not, $a0, $a1, 16, test_words
   TWO_REG message59, not, $s3, $s3, 4, test_words
   TWO_REG message60, not, $s3, $a0, 12, test_words

# seb
   TWO_REG message61, seb, $t3, $s1, 20, test_words
   TWO_REG message62, seb, $a5, $a3, 8, test_words
   TWO_REG message63, seb, $a2, $t2, 0, test_words
   TWO_REG message64, seb, $t1, $s1, 4, test_words
   TWO_REG message65, seb, $s1, $t0, 8, test_words
   TWO_REG message66, seb, $s4, $s0, 16, test_words
   TWO_REG message67, seb, $a4, $s3, 24, test_words
   TWO_REG message68, seb, $t0, $t3, 28, test_words
   TWO_REG message69, seb, $a5, $s2, 24, test_words
   TWO_REG message70, seb, $a6, $s1, 24, test_words
   TWO_REG message71, seb, $t1, $a0, 12, test_words
   TWO_REG message72, seb, $t2, $a5, 24, test_words
   TWO_REG message73, seb, $s0, $t3, 20, test_words
   TWO_REG message74, seb, $s0, $t1, 8, test_words
   TWO_REG message75, seb, $a2, $a6, 24, test_words
   TWO_REG message76, seb, $t0, $s2, 8, test_words
   TWO_REG message77, seb, $s4, $s6, 20, test_words
   TWO_REG message78, seb, $s6, $a2, 4, test_words
   TWO_REG message79, seb, $a1, $t3, 12, test_words
   TWO_REG message80, seb, $a3, $a5, 20, test_words

# seh
   TWO_REG message81, seh, $a1, $a2, 20, test_words
   TWO_REG message82, seh, $a0, $s5, 12, test_words
   TWO_REG message83, seh, $a3, $a1, 8, test_words
   TWO_REG message84, seh, $a4, $s2, 28, test_words
   TWO_REG message85, seh, $a6, $s4, 20, test_words
   TWO_REG message86, seh, $a5, $a0, 28, test_words
   TWO_REG message87, seh, $a3, $a5, 20, test_words
   TWO_REG message88, seh, $a5, $t3, 0, test_words
   TWO_REG message89, seh, $s6, $s5, 12, test_words
   TWO_REG message90, seh, $a5, $s0, 4, test_words
   TWO_REG message91, seh, $t2, $s2, 12, test_words
   TWO_REG message92, seh, $a0, $a2, 0, test_words
   TWO_REG message93, seh, $s3, $a5, 8, test_words
   TWO_REG message94, seh, $s1, $s1, 0, test_words
   TWO_REG message95, seh, $t1, $s0, 16, test_words
   TWO_REG message96, seh, $s2, $a5, 0, test_words
   TWO_REG message97, seh, $s6, $s0, 4, test_words
   TWO_REG message98, seh, $a3, $a5, 20, test_words
   TWO_REG message99, seh, $s7, $t3, 8, test_words
   TWO_REG message100, seh, $a7, $t1, 24, test_words

# and[32]
   THREE_REG message101, and[32], $t0, $s7, $s4, 12, 28, test_words, test_words
   THREE_REG message102, and[32], $s7, $t0, $t2, 16, 24, test_words, test_words
   THREE_REG message103, and[32], $a0, $a2, $s6, 4, 24, test_words, test_words
   THREE_REG message104, and[32], $a6, $a7, $a7, 8, 20, test_words, test_words
   THREE_REG message105, and[32], $a7, $a0, $t1, 20, 4, test_words, test_words
   THREE_REG message106, and[32], $s4, $a4, $a3, 12, 20, test_words, test_words
   THREE_REG message107, and[32], $a5, $t3, $s1, 8, 28, test_words, test_words
   THREE_REG message108, and[32], $s0, $t1, $t3, 28, 4, test_words, test_words
   THREE_REG message109, and[32], $a5, $a7, $a3, 28, 0, test_words, test_words
   THREE_REG message110, and[32], $a5, $t1, $s3, 0, 12, test_words, test_words
   THREE_REG message111, and[32], $a5, $a0, $t3, 24, 20, test_words, test_words
   THREE_REG message112, and[32], $s0, $s6, $s1, 12, 24, test_words, test_words
   THREE_REG message113, and[32], $s3, $s1, $t1, 16, 28, test_words, test_words
   THREE_REG message114, and[32], $s0, $a0, $s0, 12, 12, test_words, test_words
   THREE_REG message115, and[32], $t1, $s4, $a6, 4, 12, test_words, test_words
   THREE_REG message116, and[32], $a2, $t2, $a0, 8, 12, test_words, test_words
   THREE_REG message117, and[32], $a0, $s3, $a3, 28, 20, test_words, test_words
   THREE_REG message118, and[32], $s0, $a0, $t3, 20, 12, test_words, test_words
   THREE_REG message119, and[32], $s1, $a0, $a4, 28, 16, test_words, test_words
   THREE_REG message120, and[32], $s7, $a7, $s5, 12, 12, test_words, test_words
   THREE_REG message121, and[32], $s4, $s5, $t3, 28, 8, test_words, test_words
   THREE_REG message122, and[32], $a3, $a1, $t0, 28, 12, test_words, test_words
   THREE_REG message123, and[32], $a7, $a7, $s2, 8, 28, test_words, test_words
   THREE_REG message124, and[32], $a3, $s2, $t3, 28, 0, test_words, test_words
   THREE_REG message125, and[32], $s6, $a4, $s0, 12, 12, test_words, test_words
   THREE_REG message126, and[32], $s5, $t2, $s3, 24, 24, test_words, test_words
   THREE_REG message127, and[32], $t2, $s2, $t3, 20, 4, test_words, test_words
   THREE_REG message128, and[32], $t1, $s4, $a3, 20, 0, test_words, test_words
   THREE_REG message129, and[32], $a2, $s5, $a7, 4, 28, test_words, test_words
   THREE_REG message130, and[32], $a6, $s0, $t2, 20, 28, test_words, test_words
   THREE_REG message131, and[32], $t2, $a4, $s4, 8, 12, test_words, test_words
   THREE_REG message132, and[32], $s7, $s3, $a5, 8, 20, test_words, test_words
   THREE_REG message133, and[32], $s3, $t0, $a0, 8, 24, test_words, test_words
   THREE_REG message134, and[32], $a1, $t3, $t2, 16, 4, test_words, test_words
   THREE_REG message135, and[32], $t2, $t3, $t2, 24, 16, test_words, test_words
   THREE_REG message136, and[32], $a5, $t0, $a4, 28, 8, test_words, test_words
   THREE_REG message137, and[32], $a0, $a1, $s2, 0, 16, test_words, test_words
   THREE_REG message138, and[32], $t1, $t3, $a3, 8, 8, test_words, test_words
   THREE_REG message139, and[32], $s5, $t2, $s6, 4, 0, test_words, test_words
   THREE_REG message140, and[32], $s0, $s6, $a0, 28, 28, test_words, test_words

# nor
   THREE_REG message141, nor, $a5, $t1, $a2, 12, 12, test_words, test_words
   THREE_REG message142, nor, $a6, $a0, $a4, 8, 0, test_words, test_words
   THREE_REG message143, nor, $s2, $a2, $t1, 16, 24, test_words, test_words
   THREE_REG message144, nor, $s1, $a1, $a6, 4, 12, test_words, test_words
   THREE_REG message145, nor, $a4, $t2, $s5, 8, 12, test_words, test_words
   THREE_REG message146, nor, $s6, $t3, $s1, 24, 8, test_words, test_words
   THREE_REG message147, nor, $a0, $a3, $t3, 24, 24, test_words, test_words
   THREE_REG message148, nor, $a7, $a0, $a6, 12, 28, test_words, test_words
   THREE_REG message149, nor, $a2, $s5, $t1, 0, 4, test_words, test_words
   THREE_REG message150, nor, $a4, $s1, $a6, 8, 24, test_words, test_words
   THREE_REG message151, nor, $s5, $a6, $a4, 28, 16, test_words, test_words
   THREE_REG message152, nor, $a7, $a5, $a7, 4, 28, test_words, test_words
   THREE_REG message153, nor, $t1, $s1, $s6, 4, 16, test_words, test_words
   THREE_REG message154, nor, $s4, $a4, $t0, 24, 28, test_words, test_words
   THREE_REG message155, nor, $s3, $s5, $a0, 20, 20, test_words, test_words
   THREE_REG message156, nor, $t1, $a5, $a6, 28, 12, test_words, test_words
   THREE_REG message157, nor, $s5, $s0, $s5, 4, 12, test_words, test_words
   THREE_REG message158, nor, $s1, $a5, $a4, 4, 24, test_words, test_words
   THREE_REG message159, nor, $a7, $s6, $a3, 20, 12, test_words, test_words
   THREE_REG message160, nor, $t3, $a5, $a7, 0, 16, test_words, test_words
   THREE_REG message161, nor, $s6, $t3, $s5, 8, 16, test_words, test_words
   THREE_REG message162, nor, $t2, $a3, $s5, 0, 24, test_words, test_words
   THREE_REG message163, nor, $a0, $s1, $t3, 20, 12, test_words, test_words
   THREE_REG message164, nor, $s2, $s7, $a4, 12, 0, test_words, test_words
   THREE_REG message165, nor, $t2, $a2, $t2, 4, 16, test_words, test_words
   THREE_REG message166, nor, $a2, $a1, $s5, 20, 4, test_words, test_words
   THREE_REG message167, nor, $a1, $t0, $s0, 24, 24, test_words, test_words
   THREE_REG message168, nor, $s1, $a4, $a6, 8, 20, test_words, test_words
   THREE_REG message169, nor, $t0, $s7, $a2, 28, 0, test_words, test_words
   THREE_REG message170, nor, $s1, $s2, $s7, 4, 4, test_words, test_words
   THREE_REG message171, nor, $t3, $a3, $t3, 8, 4, test_words, test_words
   THREE_REG message172, nor, $a7, $s4, $t2, 20, 20, test_words, test_words
   THREE_REG message173, nor, $t3, $a6, $s1, 12, 4, test_words, test_words
   THREE_REG message174, nor, $s4, $t0, $s1, 24, 12, test_words, test_words
   THREE_REG message175, nor, $a2, $t2, $t2, 4, 8, test_words, test_words
   THREE_REG message176, nor, $t2, $a6, $s0, 24, 16, test_words, test_words
   THREE_REG message177, nor, $a5, $t1, $s7, 0, 28, test_words, test_words
   THREE_REG message178, nor, $s4, $a4, $a3, 24, 4, test_words, test_words
   THREE_REG message179, nor, $t1, $a1, $s7, 24, 20, test_words, test_words
   THREE_REG message180, nor, $a0, $s2, $s1, 8, 16, test_words, test_words

# or[32]
   THREE_REG message181, or[32], $a0, $s4, $a3, 24, 20, test_words, test_words
   THREE_REG message182, or[32], $a1, $a1, $t0, 4, 12, test_words, test_words
   THREE_REG message183, or[32], $s0, $a6, $t0, 28, 8, test_words, test_words
   THREE_REG message184, or[32], $t0, $a0, $t2, 28, 8, test_words, test_words
   THREE_REG message185, or[32], $t3, $s0, $s0, 12, 12, test_words, test_words
   THREE_REG message186, or[32], $s1, $s7, $s1, 24, 4, test_words, test_words
   THREE_REG message187, or[32], $a2, $s3, $s2, 20, 20, test_words, test_words
   THREE_REG message188, or[32], $t3, $t2, $s6, 12, 28, test_words, test_words
   THREE_REG message189, or[32], $s5, $s7, $s1, 24, 28, test_words, test_words
   THREE_REG message190, or[32], $a3, $t2, $a3, 8, 20, test_words, test_words
   THREE_REG message191, or[32], $t1, $s5, $s2, 4, 16, test_words, test_words
   THREE_REG message192, or[32], $t1, $a6, $t0, 24, 4, test_words, test_words
   THREE_REG message193, or[32], $a5, $a0, $a0, 12, 4, test_words, test_words
   THREE_REG message194, or[32], $s5, $a7, $s7, 0, 8, test_words, test_words
   THREE_REG message195, or[32], $a6, $a1, $s2, 28, 12, test_words, test_words
   THREE_REG message196, or[32], $a5, $t3, $s5, 16, 20, test_words, test_words
   THREE_REG message197, or[32], $s7, $t1, $t2, 4, 12, test_words, test_words
   THREE_REG message198, or[32], $a7, $a2, $s5, 28, 16, test_words, test_words
   THREE_REG message199, or[32], $a2, $a0, $a1, 24, 0, test_words, test_words
   THREE_REG message200, or[32], $t2, $t0, $s7, 8, 16, test_words, test_words
   THREE_REG message201, or[32], $s1, $a4, $s1, 12, 0, test_words, test_words
   THREE_REG message202, or[32], $a5, $t0, $a3, 24, 0, test_words, test_words
   THREE_REG message203, or[32], $s0, $s5, $a6, 24, 24, test_words, test_words
   THREE_REG message204, or[32], $s5, $a5, $s4, 8, 0, test_words, test_words
   THREE_REG message205, or[32], $a5, $s1, $s5, 8, 28, test_words, test_words
   THREE_REG message206, or[32], $s5, $t0, $s3, 16, 24, test_words, test_words
   THREE_REG message207, or[32], $a7, $a1, $s3, 4, 4, test_words, test_words
   THREE_REG message208, or[32], $a3, $s6, $a1, 8, 16, test_words, test_words
   THREE_REG message209, or[32], $s2, $s6, $t2, 0, 0, test_words, test_words
   THREE_REG message210, or[32], $s0, $a1, $a6, 20, 28, test_words, test_words
   THREE_REG message211, or[32], $t2, $t2, $s0, 28, 0, test_words, test_words
   THREE_REG message212, or[32], $s4, $t0, $a4, 12, 0, test_words, test_words
   THREE_REG message213, or[32], $t3, $s7, $s2, 8, 16, test_words, test_words
   THREE_REG message214, or[32], $s7, $a5, $s2, 16, 12, test_words, test_words
   THREE_REG message215, or[32], $s6, $t2, $t1, 0, 8, test_words, test_words
   THREE_REG message216, or[32], $s1, $s5, $a7, 28, 24, test_words, test_words
   THREE_REG message217, or[32], $a7, $t2, $t0, 28, 4, test_words, test_words
   THREE_REG message218, or[32], $s4, $s3, $a2, 0, 12, test_words, test_words
   THREE_REG message219, or[32], $s2, $a7, $a2, 16, 20, test_words, test_words
   THREE_REG message220, or[32], $a6, $s7, $s6, 16, 0, test_words, test_words

# xor[32]
   THREE_REG message221, xor[32], $t1, $a2, $a2, 24, 12, test_words, test_words
   THREE_REG message222, xor[32], $s1, $t0, $a4, 16, 12, test_words, test_words
   THREE_REG message223, xor[32], $a2, $s7, $a5, 24, 12, test_words, test_words
   THREE_REG message224, xor[32], $t3, $a2, $s6, 20, 8, test_words, test_words
   THREE_REG message225, xor[32], $a1, $a3, $t2, 12, 0, test_words, test_words
   THREE_REG message226, xor[32], $a7, $a1, $s3, 8, 4, test_words, test_words
   THREE_REG message227, xor[32], $s7, $s3, $a4, 24, 8, test_words, test_words
   THREE_REG message228, xor[32], $a3, $t3, $a6, 12, 28, test_words, test_words
   THREE_REG message229, xor[32], $s5, $s1, $s7, 28, 12, test_words, test_words
   THREE_REG message230, xor[32], $t2, $t2, $a1, 0, 12, test_words, test_words
   THREE_REG message231, xor[32], $s3, $a6, $a6, 20, 4, test_words, test_words
   THREE_REG message232, xor[32], $s2, $s1, $a3, 8, 12, test_words, test_words
   THREE_REG message233, xor[32], $a4, $s1, $a2, 16, 12, test_words, test_words
   THREE_REG message234, xor[32], $s4, $a3, $s2, 24, 24, test_words, test_words
   THREE_REG message235, xor[32], $s2, $s4, $a3, 4, 28, test_words, test_words
   THREE_REG message236, xor[32], $a2, $a3, $s5, 12, 28, test_words, test_words
   THREE_REG message237, xor[32], $a4, $a7, $s5, 24, 0, test_words, test_words
   THREE_REG message238, xor[32], $s7, $s5, $t1, 24, 28, test_words, test_words
   THREE_REG message239, xor[32], $a0, $a6, $s0, 12, 12, test_words, test_words
   THREE_REG message240, xor[32], $a0, $a7, $a2, 24, 8, test_words, test_words
   THREE_REG message241, xor[32], $t1, $a0, $s2, 0, 20, test_words, test_words
   THREE_REG message242, xor[32], $s5, $a3, $a0, 24, 8, test_words, test_words
   THREE_REG message243, xor[32], $s0, $a2, $t1, 4, 16, test_words, test_words
   THREE_REG message244, xor[32], $t2, $a0, $s1, 28, 24, test_words, test_words
   THREE_REG message245, xor[32], $s0, $t0, $a1, 20, 28, test_words, test_words
   THREE_REG message246, xor[32], $s0, $t1, $s2, 24, 12, test_words, test_words
   THREE_REG message247, xor[32], $s0, $t3, $a0, 24, 16, test_words, test_words
   THREE_REG message248, xor[32], $t1, $s7, $s7, 8, 20, test_words, test_words
   THREE_REG message249, xor[32], $a5, $t2, $a7, 12, 28, test_words, test_words
   THREE_REG message250, xor[32], $s4, $a5, $t0, 4, 0, test_words, test_words
   THREE_REG message251, xor[32], $t2, $a2, $s0, 12, 28, test_words, test_words
   THREE_REG message252, xor[32], $s3, $t3, $s0, 24, 8, test_words, test_words
   THREE_REG message253, xor[32], $a7, $s6, $a5, 28, 4, test_words, test_words
   THREE_REG message254, xor[32], $a5, $s1, $s4, 16, 28, test_words, test_words
   THREE_REG message255, xor[32], $a6, $s2, $s1, 4, 20, test_words, test_words
   THREE_REG message256, xor[32], $a4, $s1, $a2, 16, 12, test_words, test_words
   THREE_REG message257, xor[32], $s6, $s3, $t1, 12, 24, test_words, test_words
   THREE_REG message258, xor[32], $s0, $a6, $s6, 0, 16, test_words, test_words
   THREE_REG message259, xor[32], $s0, $a7, $a7, 4, 28, test_words, test_words
   THREE_REG message260, xor[32], $a4, $s3, $s0, 0, 28, test_words, test_words

# and[16]
   TWO_REG_16 message261, and[16], $a3, $s2, 4, 0, test_words, test_words
   TWO_REG_16 message262, and[16], $a0, $s2, 16, 4, test_words, test_words
   TWO_REG_16 message263, and[16], $s0, $a1, 16, 12, test_words, test_words
   TWO_REG_16 message264, and[16], $s0, $a1, 24, 8, test_words, test_words
   TWO_REG_16 message265, and[16], $s1, $s0, 16, 20, test_words, test_words
   TWO_REG_16 message266, and[16], $a1, $a0, 20, 0, test_words, test_words
   TWO_REG_16 message267, and[16], $a2, $s0, 0, 20, test_words, test_words
   TWO_REG_16 message268, and[16], $a0, $a0, 16, 12, test_words, test_words
   TWO_REG_16 message269, and[16], $s3, $s2, 12, 28, test_words, test_words
   TWO_REG_16 message270, and[16], $s0, $s3, 0, 4, test_words, test_words
   TWO_REG_16 message271, and[16], $a0, $s1, 16, 16, test_words, test_words
   TWO_REG_16 message272, and[16], $s2, $a2, 28, 16, test_words, test_words
   TWO_REG_16 message273, and[16], $s2, $a3, 4, 28, test_words, test_words
   TWO_REG_16 message274, and[16], $s0, $s2, 28, 24, test_words, test_words
   TWO_REG_16 message275, and[16], $a2, $s3, 12, 8, test_words, test_words
   TWO_REG_16 message276, and[16], $a0, $s3, 20, 28, test_words, test_words
   TWO_REG_16 message277, and[16], $s1, $a0, 24, 8, test_words, test_words
   TWO_REG_16 message278, and[16], $a0, $s2, 12, 0, test_words, test_words
   TWO_REG_16 message279, and[16], $a3, $s3, 20, 8, test_words, test_words
   TWO_REG_16 message280, and[16], $a1, $s0, 24, 28, test_words, test_words

# or[16]
   TWO_REG_16 message281, or[16], $s3, $s3, 28, 12, test_words, test_words
   TWO_REG_16 message282, or[16], $s2, $s2, 4, 0, test_words, test_words
   TWO_REG_16 message283, or[16], $s2, $s0, 12, 24, test_words, test_words
   TWO_REG_16 message284, or[16], $s0, $a0, 20, 4, test_words, test_words
   TWO_REG_16 message285, or[16], $a1, $a3, 12, 4, test_words, test_words
   TWO_REG_16 message286, or[16], $a1, $s2, 4, 20, test_words, test_words
   TWO_REG_16 message287, or[16], $s1, $s2, 28, 24, test_words, test_words
   TWO_REG_16 message288, or[16], $a2, $s1, 24, 8, test_words, test_words
   TWO_REG_16 message289, or[16], $s0, $s1, 20, 8, test_words, test_words
   TWO_REG_16 message290, or[16], $a3, $s3, 12, 4, test_words, test_words
   TWO_REG_16 message291, or[16], $a3, $s2, 28, 28, test_words, test_words
   TWO_REG_16 message292, or[16], $s2, $s0, 4, 28, test_words, test_words
   TWO_REG_16 message293, or[16], $s3, $s0, 0, 4, test_words, test_words
   TWO_REG_16 message294, or[16], $a3, $a2, 24, 0, test_words, test_words
   TWO_REG_16 message295, or[16], $a0, $s1, 28, 12, test_words, test_words
   TWO_REG_16 message296, or[16], $a2, $s1, 20, 24, test_words, test_words
   TWO_REG_16 message297, or[16], $a2, $a2, 4, 20, test_words, test_words
   TWO_REG_16 message298, or[16], $a1, $s0, 28, 20, test_words, test_words
   TWO_REG_16 message299, or[16], $a2, $s2, 16, 0, test_words, test_words
   TWO_REG_16 message300, or[16], $a3, $s1, 0, 8, test_words, test_words

# xor[16]
   TWO_REG_16 message301, xor[16], $a2, $a0, 12, 20, test_words, test_words
   TWO_REG_16 message302, xor[16], $a2, $a1, 20, 12, test_words, test_words
   TWO_REG_16 message303, xor[16], $s2, $s0, 24, 0, test_words, test_words
   TWO_REG_16 message304, xor[16], $a1, $a3, 28, 12, test_words, test_words
   TWO_REG_16 message305, xor[16], $s1, $a0, 4, 28, test_words, test_words
   TWO_REG_16 message306, xor[16], $s0, $a0, 16, 24, test_words, test_words
   TWO_REG_16 message307, xor[16], $s2, $a0, 24, 4, test_words, test_words
   TWO_REG_16 message308, xor[16], $s2, $s2, 16, 0, test_words, test_words
   TWO_REG_16 message309, xor[16], $s3, $s3, 20, 4, test_words, test_words
   TWO_REG_16 message310, xor[16], $a1, $a2, 16, 28, test_words, test_words
   TWO_REG_16 message311, xor[16], $s3, $a2, 0, 0, test_words, test_words
   TWO_REG_16 message312, xor[16], $s1, $s3, 16, 12, test_words, test_words
   TWO_REG_16 message313, xor[16], $s3, $s1, 8, 12, test_words, test_words
   TWO_REG_16 message314, xor[16], $s1, $s2, 4, 12, test_words, test_words
   TWO_REG_16 message315, xor[16], $s2, $s3, 20, 16, test_words, test_words
   TWO_REG_16 message316, xor[16], $s2, $a1, 16, 20, test_words, test_words
   TWO_REG_16 message317, xor[16], $a0, $a1, 24, 4, test_words, test_words
   TWO_REG_16 message318, xor[16], $s0, $a3, 4, 12, test_words, test_words
   TWO_REG_16 message319, xor[16], $s1, $a1, 12, 12, test_words, test_words
   TWO_REG_16 message320, xor[16], $a0, $s3, 24, 28, test_words, test_words

# rotrv
   THREE_REG message321, rotrv, $a4, $s4, $a2, 4, 24, test_words, test_shifts
   THREE_REG message322, rotrv, $s3, $s1, $a4, 8, 8, test_words, test_shifts
   THREE_REG message323, rotrv, $t1, $s0, $s7, 20, 20, test_words, test_shifts
   THREE_REG message324, rotrv, $a7, $s7, $t0, 20, 12, test_words, test_shifts
   THREE_REG message325, rotrv, $s7, $a6, $a6, 16, 28, test_words, test_shifts
   THREE_REG message326, rotrv, $a5, $a3, $s7, 4, 20, test_words, test_shifts
   THREE_REG message327, rotrv, $s2, $s5, $s5, 0, 28, test_words, test_shifts
   THREE_REG message328, rotrv, $a7, $s7, $t0, 0, 20, test_words, test_shifts
   THREE_REG message329, rotrv, $t2, $a1, $a6, 4, 24, test_words, test_shifts
   THREE_REG message330, rotrv, $s3, $a0, $s5, 28, 4, test_words, test_shifts
   THREE_REG message331, rotrv, $a4, $a6, $s3, 24, 12, test_words, test_shifts
   THREE_REG message332, rotrv, $a2, $s0, $t2, 20, 4, test_words, test_shifts
   THREE_REG message333, rotrv, $s0, $t3, $t2, 20, 12, test_words, test_shifts
   THREE_REG message334, rotrv, $t1, $a1, $t2, 4, 4, test_words, test_shifts
   THREE_REG message335, rotrv, $t0, $s3, $t2, 8, 0, test_words, test_shifts
   THREE_REG message336, rotrv, $a4, $a5, $t1, 24, 20, test_words, test_shifts
   THREE_REG message337, rotrv, $s2, $s6, $a3, 24, 20, test_words, test_shifts
   THREE_REG message338, rotrv, $t2, $s0, $a5, 4, 8, test_words, test_shifts
   THREE_REG message339, rotrv, $a2, $t1, $t1, 16, 24, test_words, test_shifts
   THREE_REG message340, rotrv, $a1, $s2, $a3, 28, 12, test_words, test_shifts
   THREE_REG message341, rotrv, $a0, $a7, $t3, 8, 20, test_words, test_shifts
   THREE_REG message342, rotrv, $a3, $s7, $a7, 0, 4, test_words, test_shifts
   THREE_REG message343, rotrv, $s4, $s7, $t3, 12, 20, test_words, test_shifts
   THREE_REG message344, rotrv, $t0, $a6, $s1, 4, 12, test_words, test_shifts
   THREE_REG message345, rotrv, $a7, $s7, $a0, 4, 16, test_words, test_shifts
   THREE_REG message346, rotrv, $a2, $s2, $t2, 8, 4, test_words, test_shifts
   THREE_REG message347, rotrv, $a5, $a6, $a1, 16, 20, test_words, test_shifts
   THREE_REG message348, rotrv, $a2, $a0, $s4, 20, 0, test_words, test_shifts
   THREE_REG message349, rotrv, $t1, $s5, $a7, 0, 4, test_words, test_shifts
   THREE_REG message350, rotrv, $s0, $t1, $t3, 24, 24, test_words, test_shifts
   THREE_REG message351, rotrv, $s6, $t1, $a6, 8, 8, test_words, test_shifts
   THREE_REG message352, rotrv, $a6, $s0, $s0, 0, 24, test_words, test_shifts
   THREE_REG message353, rotrv, $a2, $a1, $a1, 12, 24, test_words, test_shifts
   THREE_REG message354, rotrv, $t2, $s1, $a6, 24, 12, test_words, test_shifts
   THREE_REG message355, rotrv, $s2, $t3, $a4, 24, 28, test_words, test_shifts
   THREE_REG message356, rotrv, $t1, $t2, $a4, 16, 0, test_words, test_shifts
   THREE_REG message357, rotrv, $s7, $a2, $t2, 4, 16, test_words, test_shifts
   THREE_REG message358, rotrv, $t0, $t3, $t1, 4, 12, test_words, test_shifts
   THREE_REG message359, rotrv, $a3, $t3, $s0, 0, 8, test_words, test_shifts
   THREE_REG message360, rotrv, $s6, $a2, $s7, 0, 16, test_words, test_shifts

# sllv
   THREE_REG message361, sllv, $a6, $a3, $a3, 12, 20, test_words, test_shifts
   THREE_REG message362, sllv, $a3, $s4, $a3, 28, 4, test_words, test_shifts
   THREE_REG message363, sllv, $a4, $s6, $s7, 8, 12, test_words, test_shifts
   THREE_REG message364, sllv, $a4, $a2, $t2, 4, 28, test_words, test_shifts
   THREE_REG message365, sllv, $a5, $t0, $t2, 24, 4, test_words, test_shifts
   THREE_REG message366, sllv, $t0, $a0, $s7, 0, 4, test_words, test_shifts
   THREE_REG message367, sllv, $s4, $t2, $s0, 28, 20, test_words, test_shifts
   THREE_REG message368, sllv, $s5, $t2, $a6, 16, 8, test_words, test_shifts
   THREE_REG message369, sllv, $t3, $a4, $s4, 24, 24, test_words, test_shifts
   THREE_REG message370, sllv, $a4, $t2, $s5, 24, 28, test_words, test_shifts
   THREE_REG message371, sllv, $s4, $a0, $s0, 12, 24, test_words, test_shifts
   THREE_REG message372, sllv, $a1, $t3, $s6, 0, 12, test_words, test_shifts
   THREE_REG message373, sllv, $t3, $a0, $a2, 28, 16, test_words, test_shifts
   THREE_REG message374, sllv, $s7, $a0, $a6, 20, 4, test_words, test_shifts
   THREE_REG message375, sllv, $t0, $s4, $a5, 20, 28, test_words, test_shifts
   THREE_REG message376, sllv, $a4, $a5, $a5, 4, 12, test_words, test_shifts
   THREE_REG message377, sllv, $s1, $a1, $t3, 20, 0, test_words, test_shifts
   THREE_REG message378, sllv, $a5, $a2, $a4, 0, 24, test_words, test_shifts
   THREE_REG message379, sllv, $a7, $s3, $s3, 20, 28, test_words, test_shifts
   THREE_REG message380, sllv, $s3, $t1, $s3, 20, 24, test_words, test_shifts
   THREE_REG message381, sllv, $s4, $a6, $s7, 8, 12, test_words, test_shifts
   THREE_REG message382, sllv, $a2, $s6, $a4, 12, 28, test_words, test_shifts
   THREE_REG message383, sllv, $a3, $s0, $a4, 28, 20, test_words, test_shifts
   THREE_REG message384, sllv, $a1, $a0, $s3, 4, 0, test_words, test_shifts
   THREE_REG message385, sllv, $s6, $a4, $a4, 20, 24, test_words, test_shifts
   THREE_REG message386, sllv, $s7, $s0, $s7, 8, 4, test_words, test_shifts
   THREE_REG message387, sllv, $a5, $s3, $a3, 16, 4, test_words, test_shifts
   THREE_REG message388, sllv, $a2, $s6, $a7, 8, 24, test_words, test_shifts
   THREE_REG message389, sllv, $a2, $a2, $s6, 8, 20, test_words, test_shifts
   THREE_REG message390, sllv, $t0, $t3, $t1, 28, 0, test_words, test_shifts
   THREE_REG message391, sllv, $s2, $s5, $a1, 24, 24, test_words, test_shifts
   THREE_REG message392, sllv, $t3, $a1, $s2, 24, 28, test_words, test_shifts
   THREE_REG message393, sllv, $s0, $t3, $a2, 12, 0, test_words, test_shifts
   THREE_REG message394, sllv, $s7, $t2, $a6, 24, 0, test_words, test_shifts
   THREE_REG message395, sllv, $s4, $t0, $s2, 28, 28, test_words, test_shifts
   THREE_REG message396, sllv, $t3, $a3, $s6, 4, 8, test_words, test_shifts
   THREE_REG message397, sllv, $s3, $s3, $s0, 0, 20, test_words, test_shifts
   THREE_REG message398, sllv, $s2, $s3, $s2, 20, 20, test_words, test_shifts
   THREE_REG message399, sllv, $a1, $a1, $t0, 0, 16, test_words, test_shifts
   THREE_REG message400, sllv, $a0, $s3, $s6, 12, 24, test_words, test_shifts

# srav
   THREE_REG message401, srav, $s3, $s3, $a2, 4, 8, test_words, test_shifts
   THREE_REG message402, srav, $t1, $s5, $a5, 0, 24, test_words, test_shifts
   THREE_REG message403, srav, $a0, $a3, $s5, 16, 28, test_words, test_shifts
   THREE_REG message404, srav, $a2, $s2, $s2, 16, 12, test_words, test_shifts
   THREE_REG message405, srav, $a7, $s5, $s4, 12, 20, test_words, test_shifts
   THREE_REG message406, srav, $s1, $t0, $a1, 12, 28, test_words, test_shifts
   THREE_REG message407, srav, $a7, $s6, $t2, 20, 0, test_words, test_shifts
   THREE_REG message408, srav, $a1, $s3, $s5, 8, 28, test_words, test_shifts
   THREE_REG message409, srav, $s7, $t2, $s2, 0, 24, test_words, test_shifts
   THREE_REG message410, srav, $t1, $a2, $a1, 12, 24, test_words, test_shifts
   THREE_REG message411, srav, $s4, $a6, $a7, 20, 4, test_words, test_shifts
   THREE_REG message412, srav, $t1, $a6, $t1, 24, 4, test_words, test_shifts
   THREE_REG message413, srav, $s4, $s1, $t0, 28, 24, test_words, test_shifts
   THREE_REG message414, srav, $t0, $a4, $s5, 20, 24, test_words, test_shifts
   THREE_REG message415, srav, $s0, $t0, $s5, 8, 0, test_words, test_shifts
   THREE_REG message416, srav, $a3, $s7, $s2, 16, 8, test_words, test_shifts
   THREE_REG message417, srav, $a4, $s1, $s4, 12, 8, test_words, test_shifts
   THREE_REG message418, srav, $a2, $s4, $a4, 28, 8, test_words, test_shifts
   THREE_REG message419, srav, $s5, $t0, $s3, 4, 12, test_words, test_shifts
   THREE_REG message420, srav, $a2, $t1, $s7, 28, 24, test_words, test_shifts
   THREE_REG message421, srav, $s1, $t0, $s6, 24, 24, test_words, test_shifts
   THREE_REG message422, srav, $a2, $s2, $a2, 0, 8, test_words, test_shifts
   THREE_REG message423, srav, $s0, $s0, $a7, 4, 16, test_words, test_shifts
   THREE_REG message424, srav, $a5, $s7, $a4, 4, 8, test_words, test_shifts
   THREE_REG message425, srav, $t3, $s7, $a2, 24, 16, test_words, test_shifts
   THREE_REG message426, srav, $s5, $a4, $a6, 0, 0, test_words, test_shifts
   THREE_REG message427, srav, $a4, $t2, $s0, 12, 16, test_words, test_shifts
   THREE_REG message428, srav, $s6, $s5, $s6, 0, 8, test_words, test_shifts
   THREE_REG message429, srav, $a1, $t1, $a6, 16, 8, test_words, test_shifts
   THREE_REG message430, srav, $s2, $a6, $a5, 28, 28, test_words, test_shifts
   THREE_REG message431, srav, $s3, $a6, $a2, 24, 0, test_words, test_shifts
   THREE_REG message432, srav, $s3, $t3, $s5, 20, 0, test_words, test_shifts
   THREE_REG message433, srav, $a5, $a1, $t2, 20, 16, test_words, test_shifts
   THREE_REG message434, srav, $s6, $s3, $t2, 20, 0, test_words, test_shifts
   THREE_REG message435, srav, $s0, $a2, $t1, 8, 24, test_words, test_shifts
   THREE_REG message436, srav, $a7, $a5, $a0, 0, 0, test_words, test_shifts
   THREE_REG message437, srav, $t0, $s7, $s2, 24, 20, test_words, test_shifts
   THREE_REG message438, srav, $t2, $a1, $s5, 12, 8, test_words, test_shifts
   THREE_REG message439, srav, $s5, $s0, $s4, 12, 4, test_words, test_shifts
   THREE_REG message440, srav, $t0, $t1, $a5, 24, 8, test_words, test_shifts

# srlv
   THREE_REG message441, srlv, $a5, $s6, $s4, 8, 4, test_words, test_shifts
   THREE_REG message442, srlv, $a7, $a1, $a6, 28, 20, test_words, test_shifts
   THREE_REG message443, srlv, $s2, $s7, $a0, 16, 8, test_words, test_shifts
   THREE_REG message444, srlv, $s6, $s2, $a7, 28, 8, test_words, test_shifts
   THREE_REG message445, srlv, $a6, $a4, $s2, 24, 28, test_words, test_shifts
   THREE_REG message446, srlv, $a4, $a6, $s0, 4, 20, test_words, test_shifts
   THREE_REG message447, srlv, $a7, $t2, $a7, 12, 16, test_words, test_shifts
   THREE_REG message448, srlv, $s0, $t2, $s5, 24, 24, test_words, test_shifts
   THREE_REG message449, srlv, $a6, $t0, $a1, 8, 16, test_words, test_shifts
   THREE_REG message450, srlv, $a7, $s4, $t3, 12, 12, test_words, test_shifts
   THREE_REG message451, srlv, $s5, $a5, $a7, 12, 12, test_words, test_shifts
   THREE_REG message452, srlv, $a6, $t3, $s5, 12, 16, test_words, test_shifts
   THREE_REG message453, srlv, $t2, $s6, $t2, 8, 20, test_words, test_shifts
   THREE_REG message454, srlv, $s2, $a6, $a4, 12, 20, test_words, test_shifts
   THREE_REG message455, srlv, $a2, $s5, $a1, 12, 16, test_words, test_shifts
   THREE_REG message456, srlv, $a2, $a3, $t0, 20, 8, test_words, test_shifts
   THREE_REG message457, srlv, $a0, $a6, $s7, 12, 20, test_words, test_shifts
   THREE_REG message458, srlv, $a2, $t2, $a1, 12, 20, test_words, test_shifts
   THREE_REG message459, srlv, $s5, $s6, $s3, 16, 16, test_words, test_shifts
   THREE_REG message460, srlv, $t0, $a6, $s2, 0, 8, test_words, test_shifts
   THREE_REG message461, srlv, $s7, $a6, $s7, 20, 24, test_words, test_shifts
   THREE_REG message462, srlv, $t3, $a3, $a5, 0, 16, test_words, test_shifts
   THREE_REG message463, srlv, $a7, $s0, $s6, 24, 28, test_words, test_shifts
   THREE_REG message464, srlv, $a7, $t0, $t1, 16, 12, test_words, test_shifts
   THREE_REG message465, srlv, $s6, $s6, $t1, 20, 24, test_words, test_shifts
   THREE_REG message466, srlv, $s1, $s6, $s0, 16, 8, test_words, test_shifts
   THREE_REG message467, srlv, $s2, $a7, $a5, 8, 0, test_words, test_shifts
   THREE_REG message468, srlv, $a7, $t1, $t3, 0, 4, test_words, test_shifts
   THREE_REG message469, srlv, $s7, $s3, $a5, 4, 20, test_words, test_shifts
   THREE_REG message470, srlv, $a5, $s1, $a1, 24, 4, test_words, test_shifts
   THREE_REG message471, srlv, $a4, $a1, $s7, 24, 8, test_words, test_shifts
   THREE_REG message472, srlv, $a5, $a3, $s4, 24, 12, test_words, test_shifts
   THREE_REG message473, srlv, $s3, $t0, $a3, 16, 24, test_words, test_shifts
   THREE_REG message474, srlv, $t3, $s7, $t0, 28, 28, test_words, test_shifts
   THREE_REG message475, srlv, $a1, $a6, $a2, 28, 16, test_words, test_shifts
   THREE_REG message476, srlv, $t3, $t0, $s1, 0, 8, test_words, test_shifts
   THREE_REG message477, srlv, $s2, $t0, $s3, 24, 24, test_words, test_shifts
   THREE_REG message478, srlv, $s2, $s7, $s6, 24, 20, test_words, test_shifts
   THREE_REG message479, srlv, $s1, $s5, $t2, 0, 4, test_words, test_shifts
   THREE_REG message480, srlv, $t0, $a4, $s0, 0, 28, test_words, test_shifts

# andi[32]
   TWO_REG_1_IMM message481, andi[32], $t3, $a6, 4, test_words, 45
   TWO_REG_1_IMM message482, andi[32], $a5, $a1, 0, test_words, 1133
   TWO_REG_1_IMM message483, andi[32], $a2, $a4, 28, test_words, 2489
   TWO_REG_1_IMM message484, andi[32], $s5, $a7, 28, test_words, 1327
   TWO_REG_1_IMM message485, andi[32], $t1, $t2, 20, test_words, 2223
   TWO_REG_1_IMM message486, andi[32], $a0, $s3, 20, test_words, 254
   TWO_REG_1_IMM message487, andi[32], $s7, $t2, 24, test_words, 591
   TWO_REG_1_IMM message488, andi[32], $t3, $s3, 24, test_words, 2590
   TWO_REG_1_IMM message489, andi[32], $t1, $s0, 16, test_words, 1498
   TWO_REG_1_IMM message490, andi[32], $s1, $s0, 28, test_words, 3900
   TWO_REG_1_IMM message491, andi[32], $a5, $t2, 20, test_words, 3646
   TWO_REG_1_IMM message492, andi[32], $t1, $s4, 20, test_words, 2454
   TWO_REG_1_IMM message493, andi[32], $a2, $a3, 24, test_words, 3134
   TWO_REG_1_IMM message494, andi[32], $a6, $a3, 16, test_words, 393
   TWO_REG_1_IMM message495, andi[32], $a1, $a3, 4, test_words, 3832
   TWO_REG_1_IMM message496, andi[32], $s6, $a7, 28, test_words, 787
   TWO_REG_1_IMM message497, andi[32], $s7, $t3, 20, test_words, 2541
   TWO_REG_1_IMM message498, andi[32], $a7, $s4, 4, test_words, 36
   TWO_REG_1_IMM message499, andi[32], $s2, $a2, 8, test_words, 2432
   TWO_REG_1_IMM message500, andi[32], $a6, $a0, 24, test_words, 1532
   TWO_REG_1_IMM message501, andi[32], $a3, $s4, 12, test_words, 1413
   TWO_REG_1_IMM message502, andi[32], $s7, $t3, 24, test_words, 2169
   TWO_REG_1_IMM message503, andi[32], $a2, $s7, 4, test_words, 260
   TWO_REG_1_IMM message504, andi[32], $s7, $a4, 28, test_words, 3694
   TWO_REG_1_IMM message505, andi[32], $a3, $t0, 12, test_words, 3019
   TWO_REG_1_IMM message506, andi[32], $s0, $s4, 28, test_words, 1739
   TWO_REG_1_IMM message507, andi[32], $s6, $s6, 12, test_words, 1092
   TWO_REG_1_IMM message508, andi[32], $s6, $a5, 4, test_words, 2781
   TWO_REG_1_IMM message509, andi[32], $t2, $t0, 8, test_words, 2309
   TWO_REG_1_IMM message510, andi[32], $s7, $a4, 24, test_words, 3094

# andi[16]
   TWO_REG_1_IMM message511, andi[16], $a0, $a0, 8, test_words, 7
   TWO_REG_1_IMM message512, andi[16], $a0, $a2, 20, test_words, 4
   TWO_REG_1_IMM message513, andi[16], $s2, $a1, 28, test_words, 15
   TWO_REG_1_IMM message514, andi[16], $s1, $s2, 8, test_words, 8
   TWO_REG_1_IMM message515, andi[16], $a0, $s1, 16, test_words, 2
   TWO_REG_1_IMM message516, andi[16], $s2, $s1, 28, test_words, 4
   TWO_REG_1_IMM message517, andi[16], $a1, $a1, 8, test_words, 65535
   TWO_REG_1_IMM message518, andi[16], $a2, $a0, 12, test_words, 10
   TWO_REG_1_IMM message519, andi[16], $a0, $s1, 4, test_words, 1
   TWO_REG_1_IMM message520, andi[16], $s3, $s2, 20, test_words, 6
   TWO_REG_1_IMM message521, andi[16], $s3, $s0, 20, test_words, 65535
   TWO_REG_1_IMM message522, andi[16], $a2, $s3, 20, test_words, 3
   TWO_REG_1_IMM message523, andi[16], $s0, $a1, 20, test_words, 10
   TWO_REG_1_IMM message524, andi[16], $s3, $s1, 28, test_words, 8
   TWO_REG_1_IMM message525, andi[16], $s2, $a1, 20, test_words, 8
   TWO_REG_1_IMM message526, andi[16], $a1, $a0, 8, test_words, 10
   TWO_REG_1_IMM message527, andi[16], $s2, $a3, 12, test_words, 5
   TWO_REG_1_IMM message528, andi[16], $a2, $a0, 12, test_words, 1
   TWO_REG_1_IMM message529, andi[16], $s0, $a0, 24, test_words, 6
   TWO_REG_1_IMM message530, andi[16], $s3, $a3, 4, test_words, 3
   TWO_REG_1_IMM message531, andi[16], $s1, $s3, 24, test_words, 4
   TWO_REG_1_IMM message532, andi[16], $s0, $s1, 16, test_words, 10
   TWO_REG_1_IMM message533, andi[16], $s2, $a2, 12, test_words, 7
   TWO_REG_1_IMM message534, andi[16], $a2, $s1, 4, test_words, 0
   TWO_REG_1_IMM message535, andi[16], $a1, $s0, 24, test_words, 3
   TWO_REG_1_IMM message536, andi[16], $s0, $a1, 16, test_words, 0
   TWO_REG_1_IMM message537, andi[16], $a2, $a3, 28, test_words, 1
   TWO_REG_1_IMM message538, andi[16], $s2, $a0, 20, test_words, 11
   TWO_REG_1_IMM message539, andi[16], $s3, $a3, 28, test_words, 3
   TWO_REG_1_IMM message540, andi[16], $a0, $s0, 24, test_words, 6

# ori
   TWO_REG_1_IMM message541, ori, $a6, $a1, 20, test_words, 1320
   TWO_REG_1_IMM message542, ori, $a2, $s3, 4, test_words, 1295
   TWO_REG_1_IMM message543, ori, $s3, $s3, 8, test_words, 3112
   TWO_REG_1_IMM message544, ori, $s4, $t3, 0, test_words, 658
   TWO_REG_1_IMM message545, ori, $s2, $s7, 16, test_words, 1024
   TWO_REG_1_IMM message546, ori, $a0, $s1, 16, test_words, 943
   TWO_REG_1_IMM message547, ori, $a4, $a7, 12, test_words, 3004
   TWO_REG_1_IMM message548, ori, $s3, $a5, 8, test_words, 2213
   TWO_REG_1_IMM message549, ori, $a6, $t3, 24, test_words, 3480
   TWO_REG_1_IMM message550, ori, $a2, $s3, 0, test_words, 3354
   TWO_REG_1_IMM message551, ori, $s6, $s6, 8, test_words, 1430
   TWO_REG_1_IMM message552, ori, $s5, $a6, 4, test_words, 3179
   TWO_REG_1_IMM message553, ori, $s6, $a1, 16, test_words, 2250
   TWO_REG_1_IMM message554, ori, $s2, $a0, 4, test_words, 2
   TWO_REG_1_IMM message555, ori, $a7, $a0, 24, test_words, 331
   TWO_REG_1_IMM message556, ori, $a5, $s4, 0, test_words, 303
   TWO_REG_1_IMM message557, ori, $s3, $a2, 0, test_words, 2126
   TWO_REG_1_IMM message558, ori, $a5, $s4, 0, test_words, 859
   TWO_REG_1_IMM message559, ori, $a6, $a6, 8, test_words, 2076
   TWO_REG_1_IMM message560, ori, $a4, $t3, 28, test_words, 3182
   TWO_REG_1_IMM message561, ori, $a0, $t3, 0, test_words, 1870
   TWO_REG_1_IMM message562, ori, $s3, $a6, 0, test_words, 1359
   TWO_REG_1_IMM message563, ori, $a6, $a2, 8, test_words, 2940
   TWO_REG_1_IMM message564, ori, $s6, $a2, 12, test_words, 3341
   TWO_REG_1_IMM message565, ori, $t1, $s3, 12, test_words, 2238
   TWO_REG_1_IMM message566, ori, $a3, $a7, 8, test_words, 654
   TWO_REG_1_IMM message567, ori, $s5, $t0, 8, test_words, 1250
   TWO_REG_1_IMM message568, ori, $a7, $t1, 0, test_words, 3231
   TWO_REG_1_IMM message569, ori, $t1, $s5, 20, test_words, 2400
   TWO_REG_1_IMM message570, ori, $t3, $a1, 28, test_words, 2537

# xori
   TWO_REG_1_IMM message571, xori, $a3, $s5, 20, test_words, 3161
   TWO_REG_1_IMM message572, xori, $t0, $s1, 24, test_words, 3613
   TWO_REG_1_IMM message573, xori, $s4, $s2, 12, test_words, 2664
   TWO_REG_1_IMM message574, xori, $t1, $s5, 24, test_words, 755
   TWO_REG_1_IMM message575, xori, $a1, $s0, 20, test_words, 3624
   TWO_REG_1_IMM message576, xori, $a5, $a1, 28, test_words, 2806
   TWO_REG_1_IMM message577, xori, $s6, $a4, 28, test_words, 2025
   TWO_REG_1_IMM message578, xori, $a5, $s6, 12, test_words, 2108
   TWO_REG_1_IMM message579, xori, $s0, $a4, 20, test_words, 3620
   TWO_REG_1_IMM message580, xori, $s5, $a4, 4, test_words, 1742
   TWO_REG_1_IMM message581, xori, $s6, $a0, 24, test_words, 1603
   TWO_REG_1_IMM message582, xori, $a6, $s0, 24, test_words, 3603
   TWO_REG_1_IMM message583, xori, $t0, $s3, 16, test_words, 3229
   TWO_REG_1_IMM message584, xori, $a5, $a3, 16, test_words, 2527
   TWO_REG_1_IMM message585, xori, $s0, $s7, 4, test_words, 4009
   TWO_REG_1_IMM message586, xori, $a1, $s0, 24, test_words, 1857
   TWO_REG_1_IMM message587, xori, $s4, $s7, 20, test_words, 1054
   TWO_REG_1_IMM message588, xori, $t3, $t2, 16, test_words, 1845
   TWO_REG_1_IMM message589, xori, $s3, $a2, 4, test_words, 2773
   TWO_REG_1_IMM message590, xori, $s2, $t3, 0, test_words, 26
   TWO_REG_1_IMM message591, xori, $s3, $s0, 28, test_words, 492
   TWO_REG_1_IMM message592, xori, $s4, $a3, 12, test_words, 480
   TWO_REG_1_IMM message593, xori, $t2, $a4, 4, test_words, 3880
   TWO_REG_1_IMM message594, xori, $s4, $s7, 4, test_words, 3077
   TWO_REG_1_IMM message595, xori, $s3, $t3, 12, test_words, 98
   TWO_REG_1_IMM message596, xori, $s1, $s3, 0, test_words, 568
   TWO_REG_1_IMM message597, xori, $s5, $a1, 20, test_words, 383
   TWO_REG_1_IMM message598, xori, $a0, $t2, 4, test_words, 891
   TWO_REG_1_IMM message599, xori, $t2, $t0, 28, test_words, 1858
   TWO_REG_1_IMM message600, xori, $a0, $t3, 8, test_words, 722

# rotr
   TWO_REG_1_IMM message601, rotr, $s7, $a4, 8, test_words, 24
   TWO_REG_1_IMM message602, rotr, $s3, $a4, 20, test_words, 6
   TWO_REG_1_IMM message603, rotr, $s7, $s4, 4, test_words, 8
   TWO_REG_1_IMM message604, rotr, $s7, $t1, 4, test_words, 0
   TWO_REG_1_IMM message605, rotr, $t2, $t2, 28, test_words, 18
   TWO_REG_1_IMM message606, rotr, $s0, $a0, 24, test_words, 31
   TWO_REG_1_IMM message607, rotr, $t0, $t1, 4, test_words, 4
   TWO_REG_1_IMM message608, rotr, $s0, $s0, 28, test_words, 16
   TWO_REG_1_IMM message609, rotr, $a4, $a1, 0, test_words, 11
   TWO_REG_1_IMM message610, rotr, $a5, $a1, 8, test_words, 8
   TWO_REG_1_IMM message611, rotr, $s5, $a7, 4, test_words, 20
   TWO_REG_1_IMM message612, rotr, $a4, $a6, 16, test_words, 14
   TWO_REG_1_IMM message613, rotr, $t0, $a7, 0, test_words, 5
   TWO_REG_1_IMM message614, rotr, $a7, $s2, 16, test_words, 19
   TWO_REG_1_IMM message615, rotr, $s0, $t1, 0, test_words, 12
   TWO_REG_1_IMM message616, rotr, $t1, $a3, 16, test_words, 25
   TWO_REG_1_IMM message617, rotr, $a4, $a4, 20, test_words, 22
   TWO_REG_1_IMM message618, rotr, $a5, $s3, 24, test_words, 14
   TWO_REG_1_IMM message619, rotr, $t2, $t3, 8, test_words, 14
   TWO_REG_1_IMM message620, rotr, $a5, $a2, 16, test_words, 2
   TWO_REG_1_IMM message621, rotr, $s1, $t0, 28, test_words, 20
   TWO_REG_1_IMM message622, rotr, $a7, $s7, 0, test_words, 15
   TWO_REG_1_IMM message623, rotr, $a4, $t0, 12, test_words, 26
   TWO_REG_1_IMM message624, rotr, $s3, $s0, 12, test_words, 15
   TWO_REG_1_IMM message625, rotr, $a4, $s0, 20, test_words, 6
   TWO_REG_1_IMM message626, rotr, $t3, $a0, 16, test_words, 1
   TWO_REG_1_IMM message627, rotr, $t3, $a7, 28, test_words, 21
   TWO_REG_1_IMM message628, rotr, $s1, $s7, 28, test_words, 23
   TWO_REG_1_IMM message629, rotr, $s4, $s2, 12, test_words, 19
   TWO_REG_1_IMM message630, rotr, $a5, $a3, 8, test_words, 25

# sll
   TWO_REG_1_IMM message631, sll, $s3, $t1, 12, test_words, 2
   TWO_REG_1_IMM message632, sll, $t1, $a3, 8, test_words, 18
   TWO_REG_1_IMM message633, sll, $s3, $a7, 0, test_words, 15
   TWO_REG_1_IMM message634, sll, $a7, $a0, 0, test_words, 15
   TWO_REG_1_IMM message635, sll, $t3, $a0, 16, test_words, 17
   TWO_REG_1_IMM message636, sll, $s7, $a7, 0, test_words, 19
   TWO_REG_1_IMM message637, sll, $s1, $s3, 24, test_words, 2
   TWO_REG_1_IMM message638, sll, $a7, $a4, 12, test_words, 26
   TWO_REG_1_IMM message639, sll, $a2, $a3, 20, test_words, 27
   TWO_REG_1_IMM message640, sll, $a6, $s3, 20, test_words, 21
   TWO_REG_1_IMM message641, sll, $t2, $a1, 16, test_words, 2
   TWO_REG_1_IMM message642, sll, $s6, $t1, 4, test_words, 21
   TWO_REG_1_IMM message643, sll, $t1, $t1, 24, test_words, 0
   TWO_REG_1_IMM message644, sll, $t0, $a6, 16, test_words, 9
   TWO_REG_1_IMM message645, sll, $s2, $t2, 12, test_words, 9
   TWO_REG_1_IMM message646, sll, $a3, $s2, 12, test_words, 9
   TWO_REG_1_IMM message647, sll, $s5, $t0, 16, test_words, 11
   TWO_REG_1_IMM message648, sll, $t3, $a2, 4, test_words, 22
   TWO_REG_1_IMM message649, sll, $a3, $a5, 0, test_words, 9
   TWO_REG_1_IMM message650, sll, $a2, $t1, 28, test_words, 15
   TWO_REG_1_IMM message651, sll, $a6, $a5, 0, test_words, 14
   TWO_REG_1_IMM message652, sll, $a4, $s4, 28, test_words, 22
   TWO_REG_1_IMM message653, sll, $s2, $a2, 28, test_words, 1
   TWO_REG_1_IMM message654, sll, $s4, $t2, 8, test_words, 30
   TWO_REG_1_IMM message655, sll, $a7, $t3, 4, test_words, 18
   TWO_REG_1_IMM message656, sll, $a1, $a2, 0, test_words, 24
   TWO_REG_1_IMM message657, sll, $a0, $a4, 8, test_words, 10
   TWO_REG_1_IMM message658, sll, $s5, $t1, 8, test_words, 7
   TWO_REG_1_IMM message659, sll, $s6, $a2, 20, test_words, 26
   TWO_REG_1_IMM message660, sll, $a6, $s4, 0, test_words, 12

# sra
   TWO_REG_1_IMM message661, sra, $s6, $a7, 24, test_words, 23
   TWO_REG_1_IMM message662, sra, $t2, $a4, 20, test_words, 21
   TWO_REG_1_IMM message663, sra, $a3, $a6, 28, test_words, 8
   TWO_REG_1_IMM message664, sra, $a1, $a0, 4, test_words, 25
   TWO_REG_1_IMM message665, sra, $t0, $a7, 12, test_words, 26
   TWO_REG_1_IMM message666, sra, $t0, $s5, 4, test_words, 10
   TWO_REG_1_IMM message667, sra, $a7, $s7, 20, test_words, 21
   TWO_REG_1_IMM message668, sra, $t3, $a5, 8, test_words, 18
   TWO_REG_1_IMM message669, sra, $s5, $a4, 4, test_words, 23
   TWO_REG_1_IMM message670, sra, $a0, $s2, 16, test_words, 16
   TWO_REG_1_IMM message671, sra, $s0, $a3, 0, test_words, 5
   TWO_REG_1_IMM message672, sra, $s7, $t1, 24, test_words, 20
   TWO_REG_1_IMM message673, sra, $t0, $a2, 24, test_words, 0
   TWO_REG_1_IMM message674, sra, $s7, $s7, 12, test_words, 7
   TWO_REG_1_IMM message675, sra, $a2, $a4, 16, test_words, 10
   TWO_REG_1_IMM message676, sra, $s1, $s2, 16, test_words, 10
   TWO_REG_1_IMM message677, sra, $a6, $s5, 4, test_words, 23
   TWO_REG_1_IMM message678, sra, $s7, $a5, 28, test_words, 31
   TWO_REG_1_IMM message679, sra, $a1, $t3, 20, test_words, 4
   TWO_REG_1_IMM message680, sra, $s1, $s3, 0, test_words, 21
   TWO_REG_1_IMM message681, sra, $s5, $s6, 24, test_words, 5
   TWO_REG_1_IMM message682, sra, $a2, $s5, 16, test_words, 28
   TWO_REG_1_IMM message683, sra, $t1, $a4, 24, test_words, 22
   TWO_REG_1_IMM message684, sra, $s7, $t2, 4, test_words, 21
   TWO_REG_1_IMM message685, sra, $s3, $t2, 16, test_words, 10
   TWO_REG_1_IMM message686, sra, $t0, $a7, 8, test_words, 17
   TWO_REG_1_IMM message687, sra, $a7, $t3, 20, test_words, 12
   TWO_REG_1_IMM message688, sra, $s2, $s6, 4, test_words, 24
   TWO_REG_1_IMM message689, sra, $s0, $a3, 20, test_words, 10
   TWO_REG_1_IMM message690, sra, $t0, $a1, 28, test_words, 9

# srl
   TWO_REG_1_IMM message691, srl, $t1, $t1, 0, test_words, 24
   TWO_REG_1_IMM message692, srl, $t0, $a5, 24, test_words, 23
   TWO_REG_1_IMM message693, srl, $s7, $t2, 8, test_words, 19
   TWO_REG_1_IMM message694, srl, $a6, $s0, 16, test_words, 1
   TWO_REG_1_IMM message695, srl, $a7, $a6, 20, test_words, 13
   TWO_REG_1_IMM message696, srl, $s4, $s6, 20, test_words, 28
   TWO_REG_1_IMM message697, srl, $t2, $s6, 28, test_words, 30
   TWO_REG_1_IMM message698, srl, $s7, $a2, 0, test_words, 29
   TWO_REG_1_IMM message699, srl, $t3, $t0, 20, test_words, 11
   TWO_REG_1_IMM message700, srl, $a1, $a3, 12, test_words, 12
   TWO_REG_1_IMM message701, srl, $a2, $s5, 0, test_words, 12
   TWO_REG_1_IMM message702, srl, $a1, $a4, 20, test_words, 12
   TWO_REG_1_IMM message703, srl, $a2, $a6, 4, test_words, 6
   TWO_REG_1_IMM message704, srl, $s4, $t3, 12, test_words, 14
   TWO_REG_1_IMM message705, srl, $s5, $a2, 20, test_words, 13
   TWO_REG_1_IMM message706, srl, $a4, $a1, 8, test_words, 19
   TWO_REG_1_IMM message707, srl, $t1, $a3, 28, test_words, 6
   TWO_REG_1_IMM message708, srl, $t3, $s2, 8, test_words, 1
   TWO_REG_1_IMM message709, srl, $s7, $a2, 20, test_words, 24
   TWO_REG_1_IMM message710, srl, $a7, $t2, 16, test_words, 21
   TWO_REG_1_IMM message711, srl, $a4, $s1, 16, test_words, 28
   TWO_REG_1_IMM message712, srl, $a0, $a3, 12, test_words, 30
   TWO_REG_1_IMM message713, srl, $t1, $s0, 12, test_words, 1
   TWO_REG_1_IMM message714, srl, $s1, $a1, 16, test_words, 10
   TWO_REG_1_IMM message715, srl, $t0, $a3, 0, test_words, 23
   TWO_REG_1_IMM message716, srl, $a5, $t2, 0, test_words, 28
   TWO_REG_1_IMM message717, srl, $a1, $s5, 16, test_words, 12
   TWO_REG_1_IMM message718, srl, $a3, $t0, 4, test_words, 19
   TWO_REG_1_IMM message719, srl, $t2, $s1, 0, test_words, 18
   TWO_REG_1_IMM message720, srl, $a4, $a7, 0, test_words, 5

# ext
   TWO_REG_2_IMM message721, ext, $a7, $t3, 24, test_words, 0, 17
   TWO_REG_2_IMM message722, ext, $a3, $a6, 20, test_words, 14, 1
   TWO_REG_2_IMM message723, ext, $t0, $a4, 16, test_words, 21, 9
   TWO_REG_2_IMM message724, ext, $a5, $a2, 20, test_words, 29, 2
   TWO_REG_2_IMM message725, ext, $s5, $s3, 20, test_words, 3, 28
   TWO_REG_2_IMM message726, ext, $s5, $s6, 4, test_words, 24, 7
   TWO_REG_2_IMM message727, ext, $t2, $t3, 8, test_words, 5, 7
   TWO_REG_2_IMM message728, ext, $s2, $s0, 20, test_words, 23, 2
   TWO_REG_2_IMM message729, ext, $s3, $t0, 8, test_words, 12, 6
   TWO_REG_2_IMM message730, ext, $t3, $s5, 12, test_words, 8, 12
   TWO_REG_2_IMM message731, ext, $s5, $a1, 8, test_words, 11, 9
   TWO_REG_2_IMM message732, ext, $s6, $a4, 28, test_words, 15, 10
   TWO_REG_2_IMM message733, ext, $s1, $s2, 16, test_words, 23, 6
   TWO_REG_2_IMM message734, ext, $t0, $s2, 12, test_words, 13, 4
   TWO_REG_2_IMM message735, ext, $a1, $a5, 20, test_words, 20, 2
   TWO_REG_2_IMM message736, ext, $a2, $a7, 24, test_words, 2, 24
   TWO_REG_2_IMM message737, ext, $t1, $s7, 16, test_words, 27, 2
   TWO_REG_2_IMM message738, ext, $a1, $a6, 12, test_words, 12, 10
   TWO_REG_2_IMM message739, ext, $s0, $a2, 28, test_words, 0, 26
   TWO_REG_2_IMM message740, ext, $s6, $s4, 12, test_words, 21, 11
   TWO_REG_2_IMM message741, ext, $s5, $a3, 12, test_words, 26, 6
   TWO_REG_2_IMM message742, ext, $s4, $t1, 8, test_words, 18, 8
   TWO_REG_2_IMM message743, ext, $a1, $s0, 8, test_words, 5, 26
   TWO_REG_2_IMM message744, ext, $a1, $t3, 4, test_words, 24, 8
   TWO_REG_2_IMM message745, ext, $t3, $s4, 4, test_words, 18, 1
   TWO_REG_2_IMM message746, ext, $t3, $a5, 20, test_words, 14, 3
   TWO_REG_2_IMM message747, ext, $a7, $a7, 4, test_words, 14, 7
   TWO_REG_2_IMM message748, ext, $s0, $s2, 28, test_words, 30, 1
   TWO_REG_2_IMM message749, ext, $a6, $s7, 0, test_words, 28, 2
   TWO_REG_2_IMM message750, ext, $a0, $s5, 0, test_words, 17, 13

# ins
   TWO_REG_2_IMM_B message751, ins, $a7, $s0, 20, 4, test_words, test_words, 7, 1
   TWO_REG_2_IMM_B message752, ins, $t0, $a4, 28, 8, test_words, test_words, 6, 17
   TWO_REG_2_IMM_B message753, ins, $t2, $a0, 0, 16, test_words, test_words, 20, 11
   TWO_REG_2_IMM_B message754, ins, $t3, $s7, 24, 20, test_words, test_words, 18, 8
   TWO_REG_2_IMM_B message755, ins, $s1, $a7, 28, 12, test_words, test_words, 31, 1
   TWO_REG_2_IMM_B message756, ins, $s4, $s3, 20, 8, test_words, test_words, 0, 21
   TWO_REG_2_IMM_B message757, ins, $t0, $a4, 0, 12, test_words, test_words, 22, 9
   TWO_REG_2_IMM_B message758, ins, $s1, $a4, 24, 20, test_words, test_words, 16, 4
   TWO_REG_2_IMM_B message759, ins, $s7, $a7, 24, 20, test_words, test_words, 8, 1
   TWO_REG_2_IMM_B message760, ins, $a0, $a6, 0, 12, test_words, test_words, 29, 1
   TWO_REG_2_IMM_B message761, ins, $t0, $a5, 24, 4, test_words, test_words, 11, 1
   TWO_REG_2_IMM_B message762, ins, $a5, $a7, 8, 20, test_words, test_words, 26, 4
   TWO_REG_2_IMM_B message763, ins, $a3, $a7, 20, 8, test_words, test_words, 20, 7
   TWO_REG_2_IMM_B message764, ins, $s5, $t3, 4, 12, test_words, test_words, 9, 6
   TWO_REG_2_IMM_B message765, ins, $s3, $a5, 16, 12, test_words, test_words, 9, 10
   TWO_REG_2_IMM_B message766, ins, $s3, $a1, 8, 4, test_words, test_words, 14, 13
   TWO_REG_2_IMM_B message767, ins, $s0, $t3, 20, 8, test_words, test_words, 24, 1
   TWO_REG_2_IMM_B message768, ins, $t3, $t3, 28, 4, test_words, test_words, 29, 1
   TWO_REG_2_IMM_B message769, ins, $t3, $s2, 28, 0, test_words, test_words, 13, 7
   TWO_REG_2_IMM_B message770, ins, $a6, $s5, 24, 24, test_words, test_words, 4, 28
   TWO_REG_2_IMM_B message771, ins, $a7, $s7, 16, 8, test_words, test_words, 17, 5
   TWO_REG_2_IMM_B message772, ins, $a0, $a1, 20, 20, test_words, test_words, 11, 7
   TWO_REG_2_IMM_B message773, ins, $s1, $a7, 0, 20, test_words, test_words, 0, 31
   TWO_REG_2_IMM_B message774, ins, $s1, $a7, 0, 0, test_words, test_words, 23, 3
   TWO_REG_2_IMM_B message775, ins, $a1, $s2, 28, 28, test_words, test_words, 0, 4
   TWO_REG_2_IMM_B message776, ins, $a6, $t0, 12, 8, test_words, test_words, 18, 7
   TWO_REG_2_IMM_B message777, ins, $a4, $a6, 20, 4, test_words, test_words, 23, 1
   TWO_REG_2_IMM_B message778, ins, $a2, $s1, 28, 24, test_words, test_words, 18, 4
   TWO_REG_2_IMM_B message779, ins, $a4, $a7, 24, 20, test_words, test_words, 11, 3
   TWO_REG_2_IMM_B message780, ins, $a7, $s4, 0, 8, test_words, test_words, 3, 17
   TWO_REG_2_IMM_B message781, ins, $a5, $a5, 0, 0, test_words, test_words, 19, 8
   TWO_REG_2_IMM_B message782, ins, $s4, $t3, 16, 4, test_words, test_words, 8, 5
   TWO_REG_2_IMM_B message783, ins, $s6, $s6, 4, 4, test_words, test_words, 21, 10
   TWO_REG_2_IMM_B message784, ins, $a1, $a5, 24, 28, test_words, test_words, 14, 8
   TWO_REG_2_IMM_B message785, ins, $t1, $a1, 24, 20, test_words, test_words, 19, 8
   TWO_REG_2_IMM_B message786, ins, $s2, $s5, 12, 24, test_words, test_words, 25, 4
   TWO_REG_2_IMM_B message787, ins, $s1, $s2, 24, 20, test_words, test_words, 15, 2
   TWO_REG_2_IMM_B message788, ins, $t1, $a1, 20, 8, test_words, test_words, 19, 10
   TWO_REG_2_IMM_B message789, ins, $t1, $t0, 12, 12, test_words, test_words, 11, 15
   TWO_REG_2_IMM_B message790, ins, $a1, $s5, 12, 12, test_words, test_words, 14, 16

# extw
   THREE_REG_1_IMM message791, extw, $s4, $a4, $s4, 28, 12, test_words, test_words, 25
   THREE_REG_1_IMM message792, extw, $a6, $s4, $s3, 16, 4, test_words, test_words, 15
   THREE_REG_1_IMM message793, extw, $t2, $s3, $a0, 28, 20, test_words, test_words, 3
   THREE_REG_1_IMM message794, extw, $a5, $a3, $a3, 20, 24, test_words, test_words, 19
   THREE_REG_1_IMM message795, extw, $s3, $s7, $a4, 8, 24, test_words, test_words, 2
   THREE_REG_1_IMM message796, extw, $s5, $a7, $s7, 4, 8, test_words, test_words, 2
   THREE_REG_1_IMM message797, extw, $a7, $a0, $s2, 8, 16, test_words, test_words, 31
   THREE_REG_1_IMM message798, extw, $s1, $s7, $s6, 8, 24, test_words, test_words, 8
   THREE_REG_1_IMM message799, extw, $s1, $s0, $s7, 20, 20, test_words, test_words, 25
   THREE_REG_1_IMM message800, extw, $a4, $a4, $a5, 16, 24, test_words, test_words, 3
   THREE_REG_1_IMM message801, extw, $t2, $s4, $a6, 20, 4, test_words, test_words, 17
   THREE_REG_1_IMM message802, extw, $s3, $t0, $a1, 20, 28, test_words, test_words, 14
   THREE_REG_1_IMM message803, extw, $s1, $t0, $t1, 28, 8, test_words, test_words, 19
   THREE_REG_1_IMM message804, extw, $s7, $a0, $t3, 8, 20, test_words, test_words, 8
   THREE_REG_1_IMM message805, extw, $a0, $a1, $t0, 4, 4, test_words, test_words, 11
   THREE_REG_1_IMM message806, extw, $a4, $s3, $a7, 12, 20, test_words, test_words, 12
   THREE_REG_1_IMM message807, extw, $a4, $a0, $s5, 20, 8, test_words, test_words, 0
   THREE_REG_1_IMM message808, extw, $a7, $s7, $s0, 0, 24, test_words, test_words, 27
   THREE_REG_1_IMM message809, extw, $t0, $a6, $t3, 28, 0, test_words, test_words, 16
   THREE_REG_1_IMM message810, extw, $a0, $s0, $t1, 16, 20, test_words, test_words, 6
   THREE_REG_1_IMM message811, extw, $a3, $s2, $a1, 8, 20, test_words, test_words, 10
   THREE_REG_1_IMM message812, extw, $a7, $s1, $a3, 0, 24, test_words, test_words, 17
   THREE_REG_1_IMM message813, extw, $a4, $a6, $t0, 0, 8, test_words, test_words, 14
   THREE_REG_1_IMM message814, extw, $a3, $s2, $s4, 24, 24, test_words, test_words, 29
   THREE_REG_1_IMM message815, extw, $s2, $s2, $s1, 12, 8, test_words, test_words, 15
   THREE_REG_1_IMM message816, extw, $s1, $a2, $a5, 28, 16, test_words, test_words, 30
   THREE_REG_1_IMM message817, extw, $s5, $s3, $t3, 0, 12, test_words, test_words, 13
   THREE_REG_1_IMM message818, extw, $t1, $t3, $s7, 4, 16, test_words, test_words, 10
   THREE_REG_1_IMM message819, extw, $a0, $t3, $s0, 16, 24, test_words, test_words, 14
   THREE_REG_1_IMM message820, extw, $a5, $t0, $s0, 12, 0, test_words, test_words, 23
   THREE_REG_1_IMM message821, extw, $a2, $t1, $s1, 12, 0, test_words, test_words, 29
   THREE_REG_1_IMM message822, extw, $s1, $a6, $a5, 0, 8, test_words, test_words, 28
   THREE_REG_1_IMM message823, extw, $s1, $a4, $a0, 20, 20, test_words, test_words, 28
   THREE_REG_1_IMM message824, extw, $s7, $s1, $a7, 28, 8, test_words, test_words, 1
   THREE_REG_1_IMM message825, extw, $s6, $s3, $a2, 8, 24, test_words, test_words, 22
   THREE_REG_1_IMM message826, extw, $a1, $s4, $a7, 8, 12, test_words, test_words, 0
   THREE_REG_1_IMM message827, extw, $t0, $s4, $s6, 4, 16, test_words, test_words, 12
   THREE_REG_1_IMM message828, extw, $s5, $s6, $a4, 20, 28, test_words, test_words, 22
   THREE_REG_1_IMM message829, extw, $s5, $t2, $a3, 20, 20, test_words, test_words, 29
   THREE_REG_1_IMM message830, extw, $a6, $t3, $s5, 16, 24, test_words, test_words, 7

   move $a0, $zero
   restore 16, $ra
   jrc   $ra
   .set  macro
   .set  reorder
   .end  main
   .size main, .-main
