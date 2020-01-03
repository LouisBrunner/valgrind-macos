   .data
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

   .macro ONE_REG_1_IMM label, instruction, reg, offset, mem, imm
       .data
       &label:
         .ascii "&instruction &reg %08x old &reg %08x, &imm\n"
         .byte 0
       .text
         li $ra, &mem
         lw &reg, &offset($ra)
         &instruction &reg, &imm
         move $a1, &reg
         li $a0, &label
         lw $a2, &offset($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .macro TWO_REG_1_IMM_SPECIAL label, instruction, special, reg, offset, mem, imm
       .data
       &label:
         .ascii "&instruction &reg %08x &special %08x, &imm\n"
         .byte 0
       .text
         move $ra, $sp
         sw &special, 4($ra)
         li &reg, &mem
         lw &special, &offset(&reg)
         &instruction &reg, &special, &imm
         move $a1, &reg
         li $a0, &label
         move $a2, &special
         lw &special, 4($ra)
         lapc[48] $t9, mwrap_printf
         jalrc $t9
   .endm

   .globl main
   .ent   main
   .type  main, @function
main:
   .set  noreorder
   .set  nomacro
   addiu $sp, $sp, -32
   save[32] 48,$30,$31,$16-$24,$28

# add
   THREE_REG message1, add, $s5, $a6, $a1, 20, 40, test_ints, test_ints
   THREE_REG message2, add, $t0, $s2, $t2, 36, 28, test_ints, test_ints
   THREE_REG message3, add, $s2, $a2, $t0, 36, 40, test_ints, test_ints
   THREE_REG message4, add, $s4, $a5, $a3, 20, 16, test_ints, test_ints
   THREE_REG message5, add, $s1, $t1, $t2, 0, 24, test_ints, test_ints
   THREE_REG message6, add, $a2, $t3, $a7, 32, 4, test_ints, test_ints
   THREE_REG message7, add, $s0, $s5, $a7, 36, 28, test_ints, test_ints
   THREE_REG message9, add, $s4, $a7, $t1, 32, 0, test_ints, test_ints
   THREE_REG message10, add, $t3, $a4, $t1, 40, 20, test_ints, test_ints
   THREE_REG message11, add, $s6, $a3, $s3, 32, 12, test_ints, test_ints
   THREE_REG message12, add, $s5, $a6, $a7, 36, 12, test_ints, test_ints
   THREE_REG message13, add, $t0, $s1, $a0, 0, 8, test_ints, test_ints
   THREE_REG message14, add, $t3, $a6, $t3, 0, 40, test_ints, test_ints
   THREE_REG message15, add, $s6, $a0, $s5, 28, 4, test_ints, test_ints
   THREE_REG message16, add, $a5, $a2, $t1, 44, 4, test_ints, test_ints
   THREE_REG message17, add, $s3, $s1, $s4, 24, 36, test_ints, test_ints
   THREE_REG message18, add, $s0, $a7, $s4, 44, 16, test_ints, test_ints
   THREE_REG message19, add, $t3, $s7, $s1, 28, 12, test_ints, test_ints
   THREE_REG message20, add, $s3, $s3, $t1, 12, 44, test_ints, test_ints
   THREE_REG message21, add, $a4, $t1, $a0, 36, 0, test_ints, test_ints
   THREE_REG message22, add, $s5, $a3, $s6, 8, 40, test_ints, test_ints
   THREE_REG message23, add, $a3, $s5, $t3, 0, 12, test_ints, test_ints
   THREE_REG message24, add, $a4, $s0, $s7, 0, 44, test_ints, test_ints
   THREE_REG message25, add, $s7, $t3, $s2, 36, 12, test_ints, test_ints
   THREE_REG message26, add, $s6, $a0, $s6, 28, 28, test_ints, test_ints
   THREE_REG message27, add, $s5, $a7, $s4, 4, 4, test_ints, test_ints
   THREE_REG message28, add, $a4, $t2, $a0, 12, 36, test_ints, test_ints
   THREE_REG message29, add, $s2, $a6, $a6, 36, 40, test_ints, test_ints
   THREE_REG message30, add, $s6, $a5, $s6, 4, 40, test_ints, test_ints
   THREE_REG message31, add, $a1, $s0, $a5, 32, 36, test_ints, test_ints
   THREE_REG message32, add, $s0, $a6, $a6, 8, 20, test_ints, test_ints
   THREE_REG message33, add, $a1, $s0, $s1, 8, 4, test_ints, test_ints
   THREE_REG message34, add, $a6, $s2, $s4, 24, 4, test_ints, test_ints
   THREE_REG message35, add, $a5, $t0, $a0, 12, 4, test_ints, test_ints
   THREE_REG message36, add, $a6, $a5, $a6, 20, 24, test_ints, test_ints
   THREE_REG message37, add, $s4, $a2, $a7, 8, 40, test_ints, test_ints
   THREE_REG message38, add, $s4, $t2, $a4, 40, 36, test_ints, test_ints
   THREE_REG message39, add, $a6, $a4, $a1, 12, 40, test_ints, test_ints
   THREE_REG message40, add, $s6, $s5, $a0, 40, 44, test_ints, test_ints

# addu[32]
   THREE_REG message41, addu[32], $s6, $a3, $a7, 40, 12, test_ints, test_ints
   THREE_REG message42, addu[32], $t0, $s4, $a4, 8, 36, test_ints, test_ints
   THREE_REG message43, addu[32], $a3, $a7, $s0, 40, 36, test_ints, test_ints
   THREE_REG message44, addu[32], $a6, $s6, $s7, 28, 36, test_ints, test_ints
   THREE_REG message45, addu[32], $s0, $t1, $a1, 4, 16, test_ints, test_ints
   THREE_REG message46, addu[32], $a3, $t0, $s1, 44, 8, test_ints, test_ints
   THREE_REG message47, addu[32], $a0, $s1, $s2, 12, 28, test_ints, test_ints
   THREE_REG message48, addu[32], $t1, $s7, $a0, 20, 20, test_ints, test_ints
   THREE_REG message49, addu[32], $a5, $t0, $s0, 20, 40, test_ints, test_ints
   THREE_REG message50, addu[32], $a5, $s4, $a5, 4, 12, test_ints, test_ints
   THREE_REG message51, addu[32], $a2, $s1, $a4, 44, 28, test_ints, test_ints
   THREE_REG message52, addu[32], $a0, $t2, $a7, 20, 4, test_ints, test_ints
   THREE_REG message53, addu[32], $a1, $s1, $s7, 44, 0, test_ints, test_ints
   THREE_REG message54, addu[32], $a2, $a0, $s7, 24, 8, test_ints, test_ints
   THREE_REG message55, addu[32], $t0, $s4, $a2, 4, 4, test_ints, test_ints
   THREE_REG message56, addu[32], $s5, $s2, $a1, 40, 12, test_ints, test_ints
   THREE_REG message57, addu[32], $s0, $s4, $a5, 16, 28, test_ints, test_ints
   THREE_REG message58, addu[32], $t0, $a4, $t1, 12, 36, test_ints, test_ints
   THREE_REG message59, addu[32], $a2, $a4, $s6, 20, 32, test_ints, test_ints
   THREE_REG message60, addu[32], $s6, $s4, $a0, 20, 40, test_ints, test_ints
   THREE_REG message61, addu[32], $a6, $t2, $s2, 36, 44, test_ints, test_ints
   THREE_REG message62, addu[32], $a0, $s6, $t1, 20, 0, test_ints, test_ints
   THREE_REG message63, addu[32], $a1, $a2, $t0, 24, 8, test_ints, test_ints
   THREE_REG message64, addu[32], $s7, $a2, $s3, 32, 4, test_ints, test_ints
   THREE_REG message65, addu[32], $t0, $s6, $a1, 12, 0, test_ints, test_ints
   THREE_REG message66, addu[32], $a1, $t1, $t0, 24, 44, test_ints, test_ints
   THREE_REG message67, addu[32], $s6, $a0, $s1, 4, 4, test_ints, test_ints
   THREE_REG message68, addu[32], $a4, $a5, $t3, 4, 8, test_ints, test_ints
   THREE_REG message69, addu[32], $a3, $a6, $s4, 12, 32, test_ints, test_ints
   THREE_REG message70, addu[32], $t3, $a6, $t2, 8, 40, test_ints, test_ints
   THREE_REG message71, addu[32], $s7, $t2, $t1, 20, 20, test_ints, test_ints
   THREE_REG message72, addu[32], $a1, $s2, $t3, 4, 0, test_ints, test_ints
   THREE_REG message73, addu[32], $s2, $s7, $t1, 44, 0, test_ints, test_ints
   THREE_REG message74, addu[32], $a2, $s7, $t1, 8, 0, test_ints, test_ints
   THREE_REG message75, addu[32], $s0, $t1, $s7, 32, 4, test_ints, test_ints
   THREE_REG message76, addu[32], $t3, $t3, $a7, 8, 4, test_ints, test_ints
   THREE_REG message77, addu[32], $s2, $s5, $s4, 44, 24, test_ints, test_ints
   THREE_REG message78, addu[32], $s5, $a0, $s5, 0, 36, test_ints, test_ints
   THREE_REG message79, addu[32], $s1, $s6, $a4, 40, 20, test_ints, test_ints
   THREE_REG message80, addu[32], $t1, $s1, $a0, 8, 12, test_ints, test_ints

# addu[16]
   THREE_REG message81, addu[16], $a1, $a2, $s0, 32, 44, test_ints, test_ints
   THREE_REG message82, addu[16], $s1, $a3, $a2, 36, 4, test_ints, test_ints
   THREE_REG message83, addu[16], $a0, $s3, $a3, 0, 24, test_ints, test_ints
   THREE_REG message84, addu[16], $s1, $s1, $a2, 8, 8, test_ints, test_ints
   THREE_REG message85, addu[16], $s3, $a0, $a0, 12, 40, test_ints, test_ints
   THREE_REG message86, addu[16], $s2, $a0, $s3, 24, 44, test_ints, test_ints
   THREE_REG message87, addu[16], $s2, $a3, $s1, 28, 44, test_ints, test_ints
   THREE_REG message88, addu[16], $s0, $a0, $s3, 12, 4, test_ints, test_ints
   THREE_REG message89, addu[16], $a0, $s3, $a0, 12, 44, test_ints, test_ints
   THREE_REG message90, addu[16], $a2, $a1, $s0, 0, 28, test_ints, test_ints
   THREE_REG message91, addu[16], $s2, $a3, $a3, 28, 28, test_ints, test_ints
   THREE_REG message92, addu[16], $s2, $s1, $a3, 36, 28, test_ints, test_ints
   THREE_REG message93, addu[16], $s2, $s0, $a3, 16, 44, test_ints, test_ints
   THREE_REG message94, addu[16], $s2, $a0, $s3, 4, 44, test_ints, test_ints
   THREE_REG message95, addu[16], $a1, $s2, $s2, 20, 36, test_ints, test_ints
   THREE_REG message96, addu[16], $s1, $s0, $a2, 40, 16, test_ints, test_ints
   THREE_REG message97, addu[16], $s2, $a0, $s0, 4, 44, test_ints, test_ints
   THREE_REG message98, addu[16], $s3, $s3, $s0, 8, 4, test_ints, test_ints
   THREE_REG message99, addu[16], $s0, $a1, $a1, 44, 4, test_ints, test_ints
   THREE_REG message100, addu[16], $a0, $s1, $s1, 12, 28, test_ints, test_ints
   THREE_REG message101, addu[16], $s1, $a0, $a1, 28, 40, test_ints, test_ints
   THREE_REG message102, addu[16], $a2, $a1, $s2, 36, 44, test_ints, test_ints
   THREE_REG message103, addu[16], $s2, $a3, $a3, 40, 0, test_ints, test_ints
   THREE_REG message104, addu[16], $a3, $a1, $s0, 44, 0, test_ints, test_ints
   THREE_REG message105, addu[16], $a1, $a3, $s1, 8, 40, test_ints, test_ints
   THREE_REG message106, addu[16], $a2, $a2, $a0, 44, 20, test_ints, test_ints
   THREE_REG message107, addu[16], $a3, $s0, $a2, 0, 0, test_ints, test_ints
   THREE_REG message108, addu[16], $s0, $s2, $a1, 40, 44, test_ints, test_ints
   THREE_REG message109, addu[16], $s0, $a0, $s2, 28, 28, test_ints, test_ints
   THREE_REG message110, addu[16], $a3, $a2, $s0, 44, 24, test_ints, test_ints
   THREE_REG message111, addu[16], $a0, $a0, $s1, 36, 8, test_ints, test_ints
   THREE_REG message112, addu[16], $a0, $s3, $s0, 32, 12, test_ints, test_ints
   THREE_REG message113, addu[16], $s1, $a3, $a3, 44, 44, test_ints, test_ints
   THREE_REG message114, addu[16], $a3, $a3, $s1, 0, 20, test_ints, test_ints
   THREE_REG message115, addu[16], $a1, $a0, $s2, 44, 0, test_ints, test_ints
   THREE_REG message116, addu[16], $a1, $a2, $a2, 4, 36, test_ints, test_ints
   THREE_REG message117, addu[16], $s0, $s2, $a1, 40, 44, test_ints, test_ints
   THREE_REG message118, addu[16], $a3, $a2, $a3, 12, 40, test_ints, test_ints
   THREE_REG message119, addu[16], $a2, $s1, $s1, 20, 32, test_ints, test_ints
   THREE_REG message120, addu[16], $s0, $a1, $a0, 4, 36, test_ints, test_ints

# addu[4x4]
   TWO_REG_16 message121, addu[4x4], $s5, $s2, 8, 12, test_ints, test_ints
   TWO_REG_16 message122, addu[4x4], $s2, $s6, 32, 16, test_ints, test_ints
   TWO_REG_16 message123, addu[4x4], $a0, $a2, 8, 20, test_ints, test_ints
   TWO_REG_16 message124, addu[4x4], $s0, $s7, 12, 12, test_ints, test_ints
   TWO_REG_16 message125, addu[4x4], $a3, $s1, 24, 40, test_ints, test_ints
   TWO_REG_16 message126, addu[4x4], $a3, $s0, 28, 32, test_ints, test_ints
   TWO_REG_16 message127, addu[4x4], $s6, $a4, 32, 44, test_ints, test_ints
   TWO_REG_16 message128, addu[4x4], $s4, $s5, 0, 36, test_ints, test_ints
   TWO_REG_16 message129, addu[4x4], $s0, $a2, 4, 24, test_ints, test_ints
   TWO_REG_16 message130, addu[4x4], $a0, $a1, 8, 4, test_ints, test_ints
   TWO_REG_16 message131, addu[4x4], $a3, $a4, 40, 12, test_ints, test_ints
   TWO_REG_16 message132, addu[4x4], $a4, $s5, 28, 12, test_ints, test_ints
   TWO_REG_16 message133, addu[4x4], $a6, $a5, 4, 4, test_ints, test_ints
   TWO_REG_16 message134, addu[4x4], $s6, $s1, 36, 16, test_ints, test_ints
   TWO_REG_16 message135, addu[4x4], $s5, $s5, 12, 4, test_ints, test_ints
   TWO_REG_16 message136, addu[4x4], $s2, $a7, 40, 24, test_ints, test_ints
   TWO_REG_16 message137, addu[4x4], $s2, $s7, 16, 40, test_ints, test_ints
   TWO_REG_16 message138, addu[4x4], $a1, $a3, 12, 32, test_ints, test_ints
   TWO_REG_16 message139, addu[4x4], $a7, $s1, 0, 28, test_ints, test_ints
   TWO_REG_16 message140, addu[4x4], $a6, $a7, 8, 32, test_ints, test_ints

# div
   THREE_REG message141, div, $a4, $a0, $a1, 8, 32, test_ints, test_ints+4
   THREE_REG message142, div, $s2, $s2, $s6, 28, 28, test_ints, test_ints+4
   THREE_REG message143, div, $a3, $a6, $a1, 4, 24, test_ints, test_ints+4
   THREE_REG message144, div, $a3, $s1, $a1, 36, 40, test_ints, test_ints+4
   THREE_REG message145, div, $t0, $s5, $a2, 28, 24, test_ints, test_ints+4
   THREE_REG message146, div, $a6, $t3, $a1, 4, 0, test_ints, test_ints+4
   THREE_REG message147, div, $t1, $s2, $a1, 28, 24, test_ints, test_ints+4
   THREE_REG message148, div, $a6, $s1, $t3, 16, 8, test_ints, test_ints+4
   THREE_REG message149, div, $t0, $s7, $a7, 20, 36, test_ints, test_ints+4
   THREE_REG message150, div, $t3, $a4, $t2, 0, 16, test_ints, test_ints+4
   THREE_REG message151, div, $t0, $s0, $a3, 40, 28, test_ints, test_ints+4
   THREE_REG message152, div, $a1, $t1, $s7, 40, 8, test_ints, test_ints+4
   THREE_REG message153, div, $t0, $s4, $a4, 40, 12, test_ints, test_ints+4
   THREE_REG message154, div, $a5, $t0, $s4, 32, 40, test_ints, test_ints+4
   THREE_REG message155, div, $s5, $s4, $t0, 32, 0, test_ints, test_ints+4
   THREE_REG message156, div, $a5, $s4, $t2, 44, 4, test_ints, test_ints+4
   THREE_REG message157, div, $t0, $s3, $a5, 28, 16, test_ints, test_ints+4
   THREE_REG message158, div, $s1, $s5, $s3, 32, 4, test_ints, test_ints+4
   THREE_REG message159, div, $a5, $a0, $a7, 8, 32, test_ints, test_ints+4
   THREE_REG message160, div, $a2, $a7, $t3, 28, 4, test_ints, test_ints+4
   THREE_REG message161, div, $a0, $a0, $a7, 0, 24, test_ints, test_ints+4
   THREE_REG message162, div, $a1, $a2, $t1, 16, 28, test_ints, test_ints+4
   THREE_REG message163, div, $s5, $a0, $s1, 8, 24, test_ints, test_ints+4
   THREE_REG message164, div, $a3, $t3, $a5, 8, 28, test_ints, test_ints+4
   THREE_REG message165, div, $s4, $s3, $t0, 32, 40, test_ints, test_ints+4
   THREE_REG message166, div, $t2, $s6, $a0, 36, 12, test_ints, test_ints+4
   THREE_REG message167, div, $s7, $t1, $a2, 12, 32, test_ints, test_ints+4
   THREE_REG message168, div, $t3, $t0, $a0, 16, 16, test_ints, test_ints+4
   THREE_REG message169, div, $t1, $a1, $s1, 24, 0, test_ints, test_ints+4
   THREE_REG message170, div, $a1, $a1, $t3, 8, 40, test_ints, test_ints+4
   THREE_REG message171, div, $s7, $s2, $t3, 44, 36, test_ints, test_ints+4
   THREE_REG message172, div, $a4, $a1, $t1, 4, 16, test_ints, test_ints+4
   THREE_REG message173, div, $t2, $s4, $a0, 16, 4, test_ints, test_ints+4
   THREE_REG message174, div, $a6, $s3, $s4, 8, 16, test_ints, test_ints+4
   THREE_REG message175, div, $s4, $a4, $a1, 20, 24, test_ints, test_ints+4
   THREE_REG message176, div, $s5, $s6, $t0, 32, 40, test_ints, test_ints+4
   THREE_REG message177, div, $s7, $s3, $a3, 40, 8, test_ints, test_ints+4
   THREE_REG message178, div, $a5, $s3, $t0, 8, 36, test_ints, test_ints+4
   THREE_REG message179, div, $a7, $a1, $a0, 44, 28, test_ints, test_ints+4
   THREE_REG message180, div, $s0, $a6, $t1, 32, 40, test_ints, test_ints+4

# divu
   THREE_REG message181, divu, $t0, $a4, $s0, 4, 0, test_ints, test_ints+4
   THREE_REG message182, divu, $a3, $s6, $s4, 44, 4, test_ints, test_ints+4
   THREE_REG message183, divu, $t1, $t2, $a2, 32, 0, test_ints, test_ints+4
   THREE_REG message184, divu, $t1, $t1, $s0, 20, 28, test_ints, test_ints+4
   THREE_REG message185, divu, $s0, $a4, $a5, 0, 16, test_ints, test_ints+4
   THREE_REG message186, divu, $s2, $a0, $s1, 44, 20, test_ints, test_ints+4
   THREE_REG message187, divu, $a2, $t0, $t1, 24, 28, test_ints, test_ints+4
   THREE_REG message188, divu, $s7, $t1, $a4, 44, 32, test_ints, test_ints+4
   THREE_REG message189, divu, $s7, $t0, $t3, 36, 40, test_ints, test_ints+4
   THREE_REG message190, divu, $t0, $t2, $a6, 0, 12, test_ints, test_ints+4
   THREE_REG message191, divu, $t2, $a4, $t3, 28, 0, test_ints, test_ints+4
   THREE_REG message192, divu, $a3, $s5, $t1, 0, 32, test_ints, test_ints+4
   THREE_REG message193, divu, $a1, $a6, $s1, 8, 40, test_ints, test_ints+4
   THREE_REG message194, divu, $t2, $s5, $t2, 40, 0, test_ints, test_ints+4
   THREE_REG message195, divu, $t3, $s1, $a1, 40, 16, test_ints, test_ints+4
   THREE_REG message196, divu, $t2, $s6, $t1, 32, 0, test_ints, test_ints+4
   THREE_REG message197, divu, $s5, $s6, $a6, 16, 16, test_ints, test_ints+4
   THREE_REG message198, divu, $a2, $a7, $s2, 28, 36, test_ints, test_ints+4
   THREE_REG message199, divu, $a3, $s5, $t2, 16, 8, test_ints, test_ints+4
   THREE_REG message200, divu, $t3, $a3, $s1, 40, 28, test_ints, test_ints+4
   THREE_REG message201, divu, $s6, $t1, $a7, 28, 4, test_ints, test_ints+4
   THREE_REG message202, divu, $a6, $a5, $a1, 44, 8, test_ints, test_ints+4
   THREE_REG message203, divu, $s3, $a0, $s7, 4, 8, test_ints, test_ints+4
   THREE_REG message204, divu, $s4, $a3, $s4, 12, 16, test_ints, test_ints+4
   THREE_REG message205, divu, $a4, $s2, $a0, 44, 24, test_ints, test_ints+4
   THREE_REG message206, divu, $s7, $s6, $s2, 0, 20, test_ints, test_ints+4
   THREE_REG message207, divu, $t3, $s2, $s5, 8, 8, test_ints, test_ints+4
   THREE_REG message208, divu, $a0, $a4, $s3, 4, 12, test_ints, test_ints+4
   THREE_REG message209, divu, $a5, $a4, $s4, 16, 0, test_ints, test_ints+4
   THREE_REG message210, divu, $t1, $a5, $s1, 40, 12, test_ints, test_ints+4
   THREE_REG message211, divu, $t0, $a6, $a2, 16, 4, test_ints, test_ints+4
   THREE_REG message212, divu, $s1, $a3, $t0, 28, 20, test_ints, test_ints+4
   THREE_REG message213, divu, $a0, $s3, $s2, 24, 40, test_ints, test_ints+4
   THREE_REG message214, divu, $s0, $a6, $a6, 32, 32, test_ints, test_ints+4
   THREE_REG message215, divu, $t2, $t1, $t0, 40, 16, test_ints, test_ints+4
   THREE_REG message216, divu, $a2, $s4, $a2, 28, 4, test_ints, test_ints+4
   THREE_REG message217, divu, $s6, $t3, $a0, 20, 4, test_ints, test_ints+4
   THREE_REG message218, divu, $a2, $a6, $a2, 40, 20, test_ints, test_ints+4
   THREE_REG message219, divu, $a2, $a2, $s1, 36, 20, test_ints, test_ints+4
   THREE_REG message220, divu, $t2, $s5, $a7, 32, 4, test_ints, test_ints+4

# mod
   THREE_REG message221, mod, $t3, $s6, $s6, 12, 0, test_ints, test_ints+4
   THREE_REG message222, mod, $s4, $s1, $a1, 24, 20, test_ints, test_ints+4
   THREE_REG message223, mod, $a7, $a1, $s3, 12, 8, test_ints, test_ints+4
   THREE_REG message224, mod, $s2, $a1, $a4, 4, 20, test_ints, test_ints+4
   THREE_REG message225, mod, $s6, $s7, $a6, 28, 12, test_ints, test_ints+4
   THREE_REG message226, mod, $a6, $s6, $a6, 4, 0, test_ints, test_ints+4
   THREE_REG message227, mod, $a3, $a4, $t1, 40, 8, test_ints, test_ints+4
   THREE_REG message228, mod, $a6, $s2, $a1, 12, 40, test_ints, test_ints+4
   THREE_REG message229, mod, $a6, $a6, $s1, 4, 16, test_ints, test_ints+4
   THREE_REG message230, mod, $t3, $a0, $t3, 0, 20, test_ints, test_ints+4
   THREE_REG message231, mod, $a2, $s2, $a0, 16, 40, test_ints, test_ints+4
   THREE_REG message232, mod, $s5, $a7, $a0, 28, 24, test_ints, test_ints+4
   THREE_REG message233, mod, $a2, $a2, $s1, 12, 20, test_ints, test_ints+4
   THREE_REG message234, mod, $a1, $s1, $a2, 8, 16, test_ints, test_ints+4
   THREE_REG message235, mod, $s3, $a0, $a6, 32, 20, test_ints, test_ints+4
   THREE_REG message236, mod, $a4, $s4, $a2, 44, 0, test_ints, test_ints+4
   THREE_REG message237, mod, $a7, $s1, $t2, 12, 16, test_ints, test_ints+4
   THREE_REG message238, mod, $t0, $a0, $s1, 0, 0, test_ints, test_ints+4
   THREE_REG message239, mod, $s5, $s3, $a6, 8, 40, test_ints, test_ints+4
   THREE_REG message240, mod, $t2, $a3, $a4, 4, 16, test_ints, test_ints+4
   THREE_REG message241, mod, $a4, $a4, $s2, 44, 20, test_ints, test_ints+4
   THREE_REG message242, mod, $a4, $a7, $a0, 8, 32, test_ints, test_ints+4
   THREE_REG message243, mod, $a0, $a5, $s3, 28, 12, test_ints, test_ints+4
   THREE_REG message244, mod, $s5, $a7, $t1, 8, 12, test_ints, test_ints+4
   THREE_REG message245, mod, $s1, $t0, $t3, 28, 40, test_ints, test_ints+4
   THREE_REG message246, mod, $s5, $a5, $s6, 36, 40, test_ints, test_ints+4
   THREE_REG message247, mod, $a4, $s6, $s6, 8, 4, test_ints, test_ints+4
   THREE_REG message248, mod, $a3, $s3, $a4, 12, 36, test_ints, test_ints+4
   THREE_REG message249, mod, $t2, $a4, $s7, 4, 8, test_ints, test_ints+4
   THREE_REG message250, mod, $a0, $s7, $s6, 4, 40, test_ints, test_ints+4
   THREE_REG message251, mod, $a2, $t2, $a5, 36, 36, test_ints, test_ints+4
   THREE_REG message252, mod, $t0, $s7, $s3, 24, 32, test_ints, test_ints+4
   THREE_REG message253, mod, $s1, $a6, $s2, 16, 8, test_ints, test_ints+4
   THREE_REG message254, mod, $a7, $a7, $a3, 44, 40, test_ints, test_ints+4
   THREE_REG message255, mod, $s5, $a1, $t2, 32, 20, test_ints, test_ints+4
   THREE_REG message256, mod, $a5, $a0, $t2, 16, 36, test_ints, test_ints+4
   THREE_REG message257, mod, $a7, $s6, $s0, 20, 24, test_ints, test_ints+4
   THREE_REG message258, mod, $a5, $a1, $a3, 32, 24, test_ints, test_ints+4
   THREE_REG message259, mod, $t3, $t2, $s1, 40, 24, test_ints, test_ints+4
   THREE_REG message260, mod, $a1, $t1, $a1, 36, 8, test_ints, test_ints+4

# modu
   THREE_REG message261, modu, $s7, $a2, $a2, 8, 40, test_ints, test_ints+4
   THREE_REG message262, modu, $t1, $t3, $s6, 12, 16, test_ints, test_ints+4
   THREE_REG message263, modu, $s7, $a3, $t2, 12, 32, test_ints, test_ints+4
   THREE_REG message264, modu, $s6, $a1, $a1, 4, 4, test_ints, test_ints+4
   THREE_REG message265, modu, $a4, $s0, $s7, 36, 4, test_ints, test_ints+4
   THREE_REG message266, modu, $s1, $t2, $s0, 8, 4, test_ints, test_ints+4
   THREE_REG message267, modu, $s1, $t2, $s1, 44, 16, test_ints, test_ints+4
   THREE_REG message268, modu, $a3, $a1, $a3, 4, 32, test_ints, test_ints+4
   THREE_REG message269, modu, $a7, $a5, $a3, 20, 24, test_ints, test_ints+4
   THREE_REG message270, modu, $a3, $s3, $s5, 32, 24, test_ints, test_ints+4
   THREE_REG message271, modu, $a3, $a0, $s0, 24, 8, test_ints, test_ints+4
   THREE_REG message272, modu, $t3, $a0, $t0, 44, 8, test_ints, test_ints+4
   THREE_REG message273, modu, $a3, $s0, $a0, 36, 24, test_ints, test_ints+4
   THREE_REG message274, modu, $a4, $s0, $a4, 0, 28, test_ints, test_ints+4
   THREE_REG message275, modu, $s5, $a7, $a7, 32, 28, test_ints, test_ints+4
   THREE_REG message276, modu, $t3, $s4, $s7, 4, 40, test_ints, test_ints+4
   THREE_REG message277, modu, $a7, $s0, $s1, 44, 12, test_ints, test_ints+4
   THREE_REG message278, modu, $a6, $a6, $t2, 40, 8, test_ints, test_ints+4
   THREE_REG message279, modu, $s5, $t2, $s5, 4, 24, test_ints, test_ints+4
   THREE_REG message280, modu, $t1, $s2, $a3, 36, 16, test_ints, test_ints+4
   THREE_REG message281, modu, $s5, $t2, $t1, 0, 20, test_ints, test_ints+4
   THREE_REG message282, modu, $a1, $s4, $t3, 32, 0, test_ints, test_ints+4
   THREE_REG message283, modu, $a7, $s7, $t1, 32, 12, test_ints, test_ints+4
   THREE_REG message284, modu, $s7, $a3, $s1, 24, 40, test_ints, test_ints+4
   THREE_REG message285, modu, $s6, $t3, $s7, 32, 32, test_ints, test_ints+4
   THREE_REG message286, modu, $s2, $t1, $s6, 8, 40, test_ints, test_ints+4
   THREE_REG message287, modu, $t0, $t3, $s1, 36, 36, test_ints, test_ints+4
   THREE_REG message288, modu, $s0, $a6, $t0, 12, 32, test_ints, test_ints+4
   THREE_REG message289, modu, $a5, $s6, $s5, 8, 20, test_ints, test_ints+4
   THREE_REG message290, modu, $t0, $s1, $a6, 20, 28, test_ints, test_ints+4
   THREE_REG message291, modu, $s7, $s3, $s6, 44, 16, test_ints, test_ints+4
   THREE_REG message292, modu, $s3, $a1, $a0, 36, 40, test_ints, test_ints+4
   THREE_REG message293, modu, $s3, $s6, $a7, 0, 0, test_ints, test_ints+4
   THREE_REG message294, modu, $s3, $a0, $a6, 28, 12, test_ints, test_ints+4
   THREE_REG message295, modu, $a4, $a4, $s5, 40, 32, test_ints, test_ints+4
   THREE_REG message296, modu, $t0, $a6, $s7, 8, 32, test_ints, test_ints+4
   THREE_REG message297, modu, $a3, $s2, $s6, 20, 40, test_ints, test_ints+4
   THREE_REG message298, modu, $t2, $s0, $s6, 8, 28, test_ints, test_ints+4
   THREE_REG message299, modu, $a2, $a1, $a0, 36, 0, test_ints, test_ints+4
   THREE_REG message300, modu, $s3, $s4, $a5, 4, 36, test_ints, test_ints+4

# muh
   THREE_REG message301, muh, $a4, $s6, $a3, 8, 0, test_ints, test_ints
   THREE_REG message302, muh, $a1, $a6, $t2, 16, 4, test_ints, test_ints
   THREE_REG message303, muh, $a1, $s3, $a3, 28, 20, test_ints, test_ints
   THREE_REG message304, muh, $t0, $t1, $a1, 8, 0, test_ints, test_ints
   THREE_REG message305, muh, $t3, $t0, $s1, 44, 4, test_ints, test_ints
   THREE_REG message306, muh, $s7, $a3, $s1, 32, 32, test_ints, test_ints
   THREE_REG message307, muh, $a0, $t1, $a6, 12, 44, test_ints, test_ints
   THREE_REG message308, muh, $s2, $s0, $a5, 16, 20, test_ints, test_ints
   THREE_REG message309, muh, $t2, $s5, $a0, 4, 20, test_ints, test_ints
   THREE_REG message310, muh, $s5, $s1, $s2, 12, 32, test_ints, test_ints
   THREE_REG message311, muh, $t2, $a6, $s0, 0, 24, test_ints, test_ints
   THREE_REG message312, muh, $a6, $a7, $s5, 44, 12, test_ints, test_ints
   THREE_REG message313, muh, $t1, $a0, $s0, 32, 12, test_ints, test_ints
   THREE_REG message314, muh, $a7, $a6, $t0, 36, 28, test_ints, test_ints
   THREE_REG message315, muh, $a5, $s3, $a0, 4, 36, test_ints, test_ints
   THREE_REG message316, muh, $s5, $s3, $s6, 16, 24, test_ints, test_ints
   THREE_REG message317, muh, $t2, $s6, $s0, 28, 40, test_ints, test_ints
   THREE_REG message318, muh, $t2, $t1, $a1, 12, 0, test_ints, test_ints
   THREE_REG message319, muh, $s5, $s1, $t0, 20, 4, test_ints, test_ints
   THREE_REG message320, muh, $s4, $s1, $s3, 16, 40, test_ints, test_ints
   THREE_REG message321, muh, $t2, $a5, $a5, 12, 8, test_ints, test_ints
   THREE_REG message322, muh, $a2, $s4, $a5, 4, 0, test_ints, test_ints
   THREE_REG message323, muh, $a7, $t3, $a3, 16, 8, test_ints, test_ints
   THREE_REG message324, muh, $s1, $s6, $s7, 28, 24, test_ints, test_ints
   THREE_REG message325, muh, $a0, $a4, $t3, 32, 4, test_ints, test_ints
   THREE_REG message326, muh, $a4, $a0, $a6, 28, 16, test_ints, test_ints
   THREE_REG message327, muh, $a0, $t2, $t1, 24, 20, test_ints, test_ints
   THREE_REG message328, muh, $s4, $a0, $a1, 36, 20, test_ints, test_ints
   THREE_REG message329, muh, $a2, $t1, $a1, 4, 20, test_ints, test_ints
   THREE_REG message330, muh, $t3, $s6, $a7, 28, 4, test_ints, test_ints
   THREE_REG message331, muh, $t1, $a7, $s5, 0, 12, test_ints, test_ints
   THREE_REG message332, muh, $s3, $s4, $s0, 4, 0, test_ints, test_ints
   THREE_REG message333, muh, $a0, $t2, $s2, 8, 16, test_ints, test_ints
   THREE_REG message334, muh, $t3, $a2, $s0, 32, 12, test_ints, test_ints
   THREE_REG message335, muh, $s2, $s6, $s0, 28, 12, test_ints, test_ints
   THREE_REG message336, muh, $s5, $a2, $a6, 36, 36, test_ints, test_ints
   THREE_REG message337, muh, $s7, $s2, $t0, 4, 44, test_ints, test_ints
   THREE_REG message338, muh, $s4, $a0, $s3, 32, 36, test_ints, test_ints
   THREE_REG message339, muh, $a3, $s4, $a7, 4, 8, test_ints, test_ints
   THREE_REG message340, muh, $s7, $s4, $a4, 16, 20, test_ints, test_ints

# muhu
   THREE_REG message341, muhu, $t0, $a6, $s3, 32, 20, test_ints, test_ints
   THREE_REG message342, muhu, $a3, $a2, $a3, 36, 44, test_ints, test_ints
   THREE_REG message343, muhu, $s5, $t0, $t1, 20, 36, test_ints, test_ints
   THREE_REG message344, muhu, $a4, $s5, $t1, 0, 4, test_ints, test_ints
   THREE_REG message345, muhu, $s7, $s3, $t2, 40, 4, test_ints, test_ints
   THREE_REG message346, muhu, $a0, $s2, $s1, 4, 40, test_ints, test_ints
   THREE_REG message347, muhu, $a6, $a5, $s4, 24, 4, test_ints, test_ints
   THREE_REG message348, muhu, $a1, $s5, $s7, 0, 24, test_ints, test_ints
   THREE_REG message349, muhu, $s2, $t1, $s2, 32, 12, test_ints, test_ints
   THREE_REG message350, muhu, $s0, $a4, $a4, 4, 16, test_ints, test_ints
   THREE_REG message351, muhu, $s2, $s4, $t0, 0, 44, test_ints, test_ints
   THREE_REG message352, muhu, $s5, $s4, $t1, 40, 20, test_ints, test_ints
   THREE_REG message353, muhu, $a3, $a1, $s6, 28, 28, test_ints, test_ints
   THREE_REG message354, muhu, $s0, $s0, $s4, 44, 32, test_ints, test_ints
   THREE_REG message355, muhu, $t2, $a6, $s6, 16, 40, test_ints, test_ints
   THREE_REG message356, muhu, $s5, $s0, $a2, 36, 8, test_ints, test_ints
   THREE_REG message357, muhu, $a7, $s3, $a6, 44, 12, test_ints, test_ints
   THREE_REG message358, muhu, $a5, $s0, $t0, 40, 40, test_ints, test_ints
   THREE_REG message359, muhu, $t1, $t1, $a3, 32, 32, test_ints, test_ints
   THREE_REG message360, muhu, $a2, $s4, $s4, 24, 12, test_ints, test_ints
   THREE_REG message361, muhu, $s1, $a4, $a5, 28, 20, test_ints, test_ints
   THREE_REG message362, muhu, $s3, $s0, $s5, 24, 20, test_ints, test_ints
   THREE_REG message363, muhu, $a7, $s5, $a1, 8, 32, test_ints, test_ints
   THREE_REG message364, muhu, $a4, $s3, $t0, 32, 40, test_ints, test_ints
   THREE_REG message365, muhu, $t2, $s2, $s7, 8, 8, test_ints, test_ints
   THREE_REG message366, muhu, $a0, $t0, $a6, 0, 44, test_ints, test_ints
   THREE_REG message367, muhu, $a5, $s5, $t3, 12, 0, test_ints, test_ints
   THREE_REG message368, muhu, $t0, $s6, $t0, 8, 32, test_ints, test_ints
   THREE_REG message369, muhu, $a6, $s1, $s5, 12, 28, test_ints, test_ints
   THREE_REG message370, muhu, $a1, $s3, $t3, 20, 16, test_ints, test_ints
   THREE_REG message371, muhu, $t1, $t0, $s6, 16, 24, test_ints, test_ints
   THREE_REG message372, muhu, $s0, $a4, $s6, 24, 20, test_ints, test_ints
   THREE_REG message373, muhu, $s1, $s3, $s6, 20, 40, test_ints, test_ints
   THREE_REG message374, muhu, $s7, $t1, $s1, 44, 28, test_ints, test_ints
   THREE_REG message375, muhu, $a1, $s1, $s1, 44, 0, test_ints, test_ints
   THREE_REG message376, muhu, $t0, $t0, $a0, 44, 24, test_ints, test_ints
   THREE_REG message377, muhu, $a4, $a4, $a2, 40, 4, test_ints, test_ints
   THREE_REG message378, muhu, $s4, $t2, $s5, 40, 32, test_ints, test_ints
   THREE_REG message379, muhu, $s6, $a0, $a7, 20, 36, test_ints, test_ints
   THREE_REG message380, muhu, $a2, $t0, $t2, 12, 28, test_ints, test_ints

# mul[32]
   THREE_REG message381, mul[32], $t2, $a4, $s1, 28, 12, test_ints, test_ints
   THREE_REG message382, mul[32], $a5, $a7, $s0, 20, 12, test_ints, test_ints
   THREE_REG message383, mul[32], $s6, $a1, $s3, 32, 28, test_ints, test_ints
   THREE_REG message384, mul[32], $a0, $s4, $t1, 24, 24, test_ints, test_ints
   THREE_REG message385, mul[32], $s5, $s0, $s2, 4, 20, test_ints, test_ints
   THREE_REG message386, mul[32], $s3, $s7, $t1, 40, 8, test_ints, test_ints
   THREE_REG message387, mul[32], $t1, $s4, $a6, 40, 28, test_ints, test_ints
   THREE_REG message388, mul[32], $t2, $s3, $s2, 8, 20, test_ints, test_ints
   THREE_REG message389, mul[32], $a5, $a4, $s2, 4, 32, test_ints, test_ints
   THREE_REG message390, mul[32], $a2, $s1, $t0, 12, 44, test_ints, test_ints
   THREE_REG message391, mul[32], $a6, $a5, $a4, 20, 24, test_ints, test_ints
   THREE_REG message392, mul[32], $a5, $s4, $a5, 44, 24, test_ints, test_ints
   THREE_REG message393, mul[32], $s7, $s4, $t2, 20, 40, test_ints, test_ints
   THREE_REG message394, mul[32], $a1, $s3, $a1, 0, 20, test_ints, test_ints
   THREE_REG message395, mul[32], $s2, $s1, $a5, 36, 24, test_ints, test_ints
   THREE_REG message396, mul[32], $s1, $s7, $s0, 20, 24, test_ints, test_ints
   THREE_REG message397, mul[32], $a3, $s4, $a3, 28, 20, test_ints, test_ints
   THREE_REG message398, mul[32], $s5, $t1, $a5, 8, 32, test_ints, test_ints
   THREE_REG message399, mul[32], $s4, $t1, $t0, 40, 28, test_ints, test_ints
   THREE_REG message400, mul[32], $s6, $a4, $a2, 12, 32, test_ints, test_ints
   THREE_REG message401, mul[32], $a4, $t2, $s1, 36, 28, test_ints, test_ints
   THREE_REG message402, mul[32], $a0, $s3, $a6, 16, 0, test_ints, test_ints
   THREE_REG message403, mul[32], $a4, $s7, $s0, 0, 44, test_ints, test_ints
   THREE_REG message404, mul[32], $a1, $a5, $t0, 12, 0, test_ints, test_ints
   THREE_REG message405, mul[32], $a0, $s3, $s5, 0, 20, test_ints, test_ints
   THREE_REG message406, mul[32], $t0, $a2, $s1, 12, 4, test_ints, test_ints
   THREE_REG message407, mul[32], $s1, $s7, $s7, 44, 32, test_ints, test_ints
   THREE_REG message408, mul[32], $a6, $s3, $a7, 16, 12, test_ints, test_ints
   THREE_REG message409, mul[32], $t0, $a1, $t2, 16, 4, test_ints, test_ints
   THREE_REG message410, mul[32], $a5, $t2, $t2, 4, 20, test_ints, test_ints
   THREE_REG message411, mul[32], $a6, $a1, $a0, 32, 20, test_ints, test_ints
   THREE_REG message412, mul[32], $a5, $s0, $t3, 12, 44, test_ints, test_ints
   THREE_REG message413, mul[32], $s1, $t0, $a6, 0, 44, test_ints, test_ints
   THREE_REG message414, mul[32], $s7, $s7, $t2, 40, 28, test_ints, test_ints
   THREE_REG message415, mul[32], $a1, $s2, $s4, 0, 12, test_ints, test_ints
   THREE_REG message416, mul[32], $a5, $a5, $t1, 32, 28, test_ints, test_ints
   THREE_REG message417, mul[32], $a6, $a2, $a0, 8, 24, test_ints, test_ints
   THREE_REG message418, mul[32], $s2, $a3, $s7, 36, 24, test_ints, test_ints
   THREE_REG message419, mul[32], $s6, $s2, $a7, 36, 44, test_ints, test_ints
   THREE_REG message420, mul[32], $s6, $t0, $s6, 36, 40, test_ints, test_ints

# mul[4x4]
   TWO_REG_16 message421, mul[4x4], $a1, $s6, 20, 8, test_ints, test_ints
   TWO_REG_16 message422, mul[4x4], $a2, $s4, 44, 0, test_ints, test_ints
   TWO_REG_16 message423, mul[4x4], $a5, $s3, 44, 12, test_ints, test_ints
   TWO_REG_16 message424, mul[4x4], $s2, $s7, 20, 32, test_ints, test_ints
   TWO_REG_16 message425, mul[4x4], $s5, $a0, 12, 28, test_ints, test_ints
   TWO_REG_16 message426, mul[4x4], $a3, $s6, 20, 40, test_ints, test_ints
   TWO_REG_16 message427, mul[4x4], $a3, $a4, 32, 12, test_ints, test_ints
   TWO_REG_16 message428, mul[4x4], $s6, $s5, 4, 16, test_ints, test_ints
   TWO_REG_16 message429, mul[4x4], $s4, $s6, 40, 40, test_ints, test_ints
   TWO_REG_16 message430, mul[4x4], $s2, $s5, 8, 44, test_ints, test_ints
   TWO_REG_16 message431, mul[4x4], $s1, $s1, 24, 44, test_ints, test_ints
   TWO_REG_16 message432, mul[4x4], $s1, $s3, 44, 40, test_ints, test_ints
   TWO_REG_16 message433, mul[4x4], $s4, $s3, 36, 44, test_ints, test_ints
   TWO_REG_16 message434, mul[4x4], $s1, $s3, 4, 0, test_ints, test_ints
   TWO_REG_16 message435, mul[4x4], $s7, $a5, 12, 40, test_ints, test_ints
   TWO_REG_16 message436, mul[4x4], $a3, $s0, 24, 12, test_ints, test_ints
   TWO_REG_16 message437, mul[4x4], $a7, $a4, 4, 20, test_ints, test_ints
   TWO_REG_16 message438, mul[4x4], $a1, $s4, 36, 8, test_ints, test_ints
   TWO_REG_16 message439, mul[4x4], $a5, $s3, 20, 8, test_ints, test_ints
   TWO_REG_16 message440, mul[4x4], $a7, $s5, 4, 12, test_ints, test_ints

# mulu
   THREE_REG message441, mulu, $a0, $a2, $s6, 4, 4, test_ints, test_ints
   THREE_REG message442, mulu, $a3, $s5, $s1, 16, 32, test_ints, test_ints
   THREE_REG message443, mulu, $a3, $a3, $a0, 4, 24, test_ints, test_ints
   THREE_REG message444, mulu, $s3, $s1, $s0, 20, 40, test_ints, test_ints
   THREE_REG message445, mulu, $a0, $a6, $a5, 20, 36, test_ints, test_ints
   THREE_REG message446, mulu, $s6, $t0, $t0, 28, 4, test_ints, test_ints
   THREE_REG message447, mulu, $a7, $t3, $s7, 36, 16, test_ints, test_ints
   THREE_REG message448, mulu, $t1, $t0, $a5, 8, 36, test_ints, test_ints
   THREE_REG message449, mulu, $a5, $a1, $t0, 8, 40, test_ints, test_ints
   THREE_REG message450, mulu, $a7, $s1, $s3, 12, 8, test_ints, test_ints
   THREE_REG message451, mulu, $a5, $a7, $a0, 8, 0, test_ints, test_ints
   THREE_REG message452, mulu, $a2, $t1, $a0, 40, 0, test_ints, test_ints
   THREE_REG message453, mulu, $a5, $a6, $s0, 20, 28, test_ints, test_ints
   THREE_REG message454, mulu, $a0, $s2, $t0, 40, 16, test_ints, test_ints
   THREE_REG message455, mulu, $s1, $s7, $s1, 20, 4, test_ints, test_ints
   THREE_REG message456, mulu, $s3, $a4, $a3, 40, 12, test_ints, test_ints
   THREE_REG message457, mulu, $a5, $t3, $s2, 40, 8, test_ints, test_ints
   THREE_REG message458, mulu, $a7, $s0, $t3, 28, 24, test_ints, test_ints
   THREE_REG message459, mulu, $s7, $a5, $a4, 28, 8, test_ints, test_ints
   THREE_REG message460, mulu, $a4, $t0, $s0, 0, 24, test_ints, test_ints
   THREE_REG message461, mulu, $s0, $a1, $a1, 36, 40, test_ints, test_ints
   THREE_REG message462, mulu, $s7, $a0, $a3, 8, 8, test_ints, test_ints
   THREE_REG message463, mulu, $a6, $s7, $s5, 36, 4, test_ints, test_ints
   THREE_REG message464, mulu, $s7, $a4, $a1, 24, 44, test_ints, test_ints
   THREE_REG message465, mulu, $a0, $a2, $s0, 32, 20, test_ints, test_ints
   THREE_REG message466, mulu, $a6, $a0, $a5, 8, 32, test_ints, test_ints
   THREE_REG message467, mulu, $a3, $s6, $a5, 36, 28, test_ints, test_ints
   THREE_REG message468, mulu, $t0, $s4, $s7, 12, 40, test_ints, test_ints
   THREE_REG message469, mulu, $a1, $t1, $s1, 28, 24, test_ints, test_ints
   THREE_REG message470, mulu, $a3, $s6, $s2, 32, 20, test_ints, test_ints
   THREE_REG message471, mulu, $a6, $t0, $a3, 8, 4, test_ints, test_ints
   THREE_REG message472, mulu, $s4, $s5, $a5, 8, 44, test_ints, test_ints
   THREE_REG message473, mulu, $s2, $s5, $a6, 28, 8, test_ints, test_ints
   THREE_REG message474, mulu, $a1, $a3, $a2, 4, 40, test_ints, test_ints
   THREE_REG message475, mulu, $t0, $s2, $s4, 40, 20, test_ints, test_ints
   THREE_REG message476, mulu, $t2, $a1, $a4, 4, 20, test_ints, test_ints
   THREE_REG message477, mulu, $t1, $a7, $s6, 0, 36, test_ints, test_ints
   THREE_REG message478, mulu, $a7, $a4, $a6, 0, 24, test_ints, test_ints
   THREE_REG message479, mulu, $a6, $s6, $t0, 32, 24, test_ints, test_ints
   THREE_REG message480, mulu, $t2, $s5, $a5, 36, 24, test_ints, test_ints

# sub
   THREE_REG message481, sub, $s4, $s1, $a5, 16, 28, test_ints, test_ints
   THREE_REG message482, sub, $s6, $a6, $a0, 8, 12, test_ints, test_ints
   THREE_REG message483, sub, $a6, $a7, $a6, 16, 12, test_ints, test_ints
   THREE_REG message484, sub, $a0, $t3, $a4, 8, 12, test_ints, test_ints
   THREE_REG message485, sub, $t2, $s4, $t2, 40, 0, test_ints, test_ints
   THREE_REG message486, sub, $a0, $a5, $a2, 4, 8, test_ints, test_ints
   THREE_REG message487, sub, $a4, $a1, $t3, 36, 4, test_ints, test_ints
   THREE_REG message488, sub, $t3, $s0, $t0, 44, 24, test_ints, test_ints
   THREE_REG message489, sub, $s7, $t1, $s2, 24, 4, test_ints, test_ints
   THREE_REG message490, sub, $s1, $a2, $s0, 4, 0, test_ints, test_ints
   THREE_REG message491, sub, $s4, $t0, $s5, 40, 40, test_ints, test_ints
   THREE_REG message492, sub, $s1, $s6, $t3, 12, 44, test_ints, test_ints
   THREE_REG message493, sub, $t1, $a0, $a5, 4, 20, test_ints, test_ints
   THREE_REG message494, sub, $s6, $s0, $s1, 8, 28, test_ints, test_ints
   THREE_REG message495, sub, $a0, $a6, $a5, 40, 16, test_ints, test_ints
   THREE_REG message496, sub, $a6, $s3, $a2, 44, 36, test_ints, test_ints
   THREE_REG message497, sub, $s2, $s3, $a5, 44, 20, test_ints, test_ints
   THREE_REG message498, sub, $t3, $t1, $a7, 28, 0, test_ints, test_ints
   THREE_REG message499, sub, $s2, $t0, $a0, 28, 36, test_ints, test_ints
   THREE_REG message500, sub, $a2, $s2, $s5, 44, 32, test_ints, test_ints
   THREE_REG message501, sub, $a1, $s7, $s2, 24, 20, test_ints, test_ints
   THREE_REG message502, sub, $s2, $t0, $s1, 0, 12, test_ints, test_ints
   THREE_REG message503, sub, $s2, $s7, $s6, 28, 24, test_ints, test_ints
   THREE_REG message504, sub, $t3, $s6, $a7, 40, 20, test_ints, test_ints
   THREE_REG message505, sub, $s0, $s1, $a2, 0, 32, test_ints, test_ints
   THREE_REG message506, sub, $a7, $a2, $t2, 20, 24, test_ints, test_ints
   THREE_REG message507, sub, $s3, $a6, $s5, 36, 32, test_ints, test_ints
   THREE_REG message508, sub, $s3, $a4, $s4, 32, 0, test_ints, test_ints
   THREE_REG message509, sub, $s0, $a2, $a3, 24, 20, test_ints, test_ints
   THREE_REG message510, sub, $a6, $s6, $s7, 36, 32, test_ints, test_ints
   THREE_REG message511, sub, $a1, $s5, $a1, 28, 40, test_ints, test_ints
   THREE_REG message512, sub, $s2, $a2, $t0, 32, 12, test_ints, test_ints
   THREE_REG message513, sub, $s6, $s3, $s5, 32, 36, test_ints, test_ints
   THREE_REG message514, sub, $s1, $s3, $s1, 8, 44, test_ints, test_ints
   THREE_REG message515, sub, $s1, $a2, $a5, 36, 32, test_ints, test_ints
   THREE_REG message517, sub, $a3, $a2, $s2, 32, 4, test_ints, test_ints
   THREE_REG message518, sub, $t0, $a6, $a0, 32, 12, test_ints, test_ints
   THREE_REG message519, sub, $a7, $t3, $t2, 20, 28, test_ints, test_ints
   THREE_REG message520, sub, $a3, $a2, $a2, 32, 16, test_ints, test_ints

# subu[32]
   THREE_REG message521, subu[32], $a5, $a6, $s2, 40, 28, test_ints, test_ints
   THREE_REG message522, subu[32], $a7, $s0, $a2, 36, 36, test_ints, test_ints
   THREE_REG message523, subu[32], $t2, $a4, $a4, 16, 36, test_ints, test_ints
   THREE_REG message524, subu[32], $a1, $t0, $t3, 4, 4, test_ints, test_ints
   THREE_REG message525, subu[32], $t2, $s5, $s4, 4, 40, test_ints, test_ints
   THREE_REG message526, subu[32], $a3, $s4, $a0, 20, 4, test_ints, test_ints
   THREE_REG message527, subu[32], $s1, $a2, $a3, 28, 32, test_ints, test_ints
   THREE_REG message528, subu[32], $s6, $t2, $a4, 32, 0, test_ints, test_ints
   THREE_REG message529, subu[32], $t1, $a6, $s4, 4, 12, test_ints, test_ints
   THREE_REG message530, subu[32], $a5, $s7, $a3, 36, 0, test_ints, test_ints
   THREE_REG message531, subu[32], $a4, $s7, $s5, 16, 0, test_ints, test_ints
   THREE_REG message532, subu[32], $s3, $s3, $t1, 16, 0, test_ints, test_ints
   THREE_REG message533, subu[32], $a6, $s5, $t2, 4, 16, test_ints, test_ints
   THREE_REG message534, subu[32], $s6, $a7, $s7, 8, 28, test_ints, test_ints
   THREE_REG message535, subu[32], $s7, $s0, $s5, 12, 36, test_ints, test_ints
   THREE_REG message536, subu[32], $s4, $a5, $t0, 0, 40, test_ints, test_ints
   THREE_REG message537, subu[32], $t1, $s4, $a1, 8, 20, test_ints, test_ints
   THREE_REG message538, subu[32], $a6, $a6, $s4, 12, 24, test_ints, test_ints
   THREE_REG message539, subu[32], $a1, $a1, $s7, 28, 24, test_ints, test_ints
   THREE_REG message540, subu[32], $s3, $a6, $s1, 40, 0, test_ints, test_ints
   THREE_REG message541, subu[32], $t0, $a1, $t0, 4, 36, test_ints, test_ints
   THREE_REG message542, subu[32], $t2, $a2, $t2, 40, 40, test_ints, test_ints
   THREE_REG message543, subu[32], $a4, $a3, $a2, 36, 40, test_ints, test_ints
   THREE_REG message544, subu[32], $s7, $s3, $a4, 0, 8, test_ints, test_ints
   THREE_REG message545, subu[32], $a6, $s5, $a7, 4, 16, test_ints, test_ints
   THREE_REG message546, subu[32], $s1, $a4, $s2, 40, 12, test_ints, test_ints
   THREE_REG message547, subu[32], $t3, $a6, $a4, 28, 16, test_ints, test_ints
   THREE_REG message548, subu[32], $t1, $a5, $s2, 28, 32, test_ints, test_ints
   THREE_REG message549, subu[32], $a0, $t3, $a7, 8, 20, test_ints, test_ints
   THREE_REG message550, subu[32], $t1, $a2, $s0, 36, 8, test_ints, test_ints
   THREE_REG message551, subu[32], $s7, $t3, $s7, 24, 0, test_ints, test_ints
   THREE_REG message552, subu[32], $s3, $t0, $t1, 24, 40, test_ints, test_ints
   THREE_REG message553, subu[32], $s0, $s5, $a5, 32, 32, test_ints, test_ints
   THREE_REG message554, subu[32], $a1, $a2, $s2, 12, 4, test_ints, test_ints
   THREE_REG message555, subu[32], $a2, $a3, $s1, 4, 20, test_ints, test_ints
   THREE_REG message556, subu[32], $a6, $s3, $t3, 8, 32, test_ints, test_ints
   THREE_REG message557, subu[32], $s5, $a1, $a4, 16, 0, test_ints, test_ints
   THREE_REG message558, subu[32], $t0, $s4, $a4, 4, 40, test_ints, test_ints
   THREE_REG message559, subu[32], $s2, $a5, $s3, 28, 40, test_ints, test_ints
   THREE_REG message560, subu[32], $a3, $a0, $s4, 20, 44, test_ints, test_ints

# subu[16]
   THREE_REG message561, subu[16], $a1, $a3, $a2, 40, 36, test_ints, test_ints
   THREE_REG message562, subu[16], $a0, $s0, $s0, 44, 12, test_ints, test_ints
   THREE_REG message563, subu[16], $a0, $s1, $a0, 0, 4, test_ints, test_ints
   THREE_REG message564, subu[16], $a0, $a1, $s1, 32, 40, test_ints, test_ints
   THREE_REG message565, subu[16], $a3, $a3, $s0, 24, 24, test_ints, test_ints
   THREE_REG message566, subu[16], $a2, $a2, $a3, 40, 44, test_ints, test_ints
   THREE_REG message567, subu[16], $s2, $a3, $s3, 36, 40, test_ints, test_ints
   THREE_REG message568, subu[16], $a0, $a1, $a2, 32, 16, test_ints, test_ints
   THREE_REG message569, subu[16], $s1, $s0, $s1, 8, 20, test_ints, test_ints
   THREE_REG message570, subu[16], $s3, $s2, $s2, 16, 44, test_ints, test_ints
   THREE_REG message571, subu[16], $a0, $a0, $s2, 0, 24, test_ints, test_ints
   THREE_REG message572, subu[16], $s0, $s2, $a0, 44, 0, test_ints, test_ints
   THREE_REG message573, subu[16], $s0, $s2, $s0, 12, 28, test_ints, test_ints
   THREE_REG message574, subu[16], $a2, $a3, $a0, 0, 44, test_ints, test_ints
   THREE_REG message575, subu[16], $a0, $a2, $a3, 8, 0, test_ints, test_ints
   THREE_REG message576, subu[16], $a0, $s1, $s2, 24, 36, test_ints, test_ints
   THREE_REG message577, subu[16], $a1, $s3, $a1, 44, 28, test_ints, test_ints
   THREE_REG message578, subu[16], $a0, $s0, $a2, 0, 44, test_ints, test_ints
   THREE_REG message579, subu[16], $a2, $s0, $a1, 24, 28, test_ints, test_ints
   THREE_REG message580, subu[16], $a0, $a0, $a2, 16, 4, test_ints, test_ints
   THREE_REG message581, subu[16], $a1, $a1, $s3, 20, 44, test_ints, test_ints
   THREE_REG message582, subu[16], $s3, $s1, $s0, 20, 16, test_ints, test_ints
   THREE_REG message583, subu[16], $s1, $s3, $a3, 28, 40, test_ints, test_ints
   THREE_REG message584, subu[16], $s2, $s3, $a2, 32, 28, test_ints, test_ints
   THREE_REG message585, subu[16], $s2, $a3, $s0, 28, 36, test_ints, test_ints
   THREE_REG message586, subu[16], $a3, $a0, $a2, 8, 16, test_ints, test_ints
   THREE_REG message587, subu[16], $s3, $s3, $a1, 8, 32, test_ints, test_ints
   THREE_REG message588, subu[16], $a0, $s1, $a2, 32, 8, test_ints, test_ints
   THREE_REG message589, subu[16], $s2, $a2, $a1, 36, 4, test_ints, test_ints
   THREE_REG message590, subu[16], $a0, $s3, $a0, 24, 16, test_ints, test_ints
   THREE_REG message591, subu[16], $s3, $a0, $a3, 28, 16, test_ints, test_ints
   THREE_REG message592, subu[16], $s0, $s3, $a0, 24, 4, test_ints, test_ints
   THREE_REG message593, subu[16], $a0, $s1, $s0, 24, 12, test_ints, test_ints
   THREE_REG message594, subu[16], $a1, $a2, $a0, 12, 28, test_ints, test_ints
   THREE_REG message595, subu[16], $a3, $a1, $a1, 16, 8, test_ints, test_ints
   THREE_REG message596, subu[16], $a2, $s0, $a1, 8, 44, test_ints, test_ints
   THREE_REG message597, subu[16], $a1, $a1, $s3, 0, 20, test_ints, test_ints
   THREE_REG message598, subu[16], $s3, $a1, $s0, 44, 28, test_ints, test_ints
   THREE_REG message599, subu[16], $a1, $a0, $s1, 36, 8, test_ints, test_ints
   THREE_REG message600, subu[16], $a0, $s2, $s0, 4, 4, test_ints, test_ints

# addiu[32]
   TWO_REG_1_IMM message601, addiu[32], $a3, $a0, 24, test_ints, 7140
   TWO_REG_1_IMM message602, addiu[32], $a4, $a4, 40, test_ints, 49885
   TWO_REG_1_IMM message603, addiu[32], $a6, $a0, 16, test_ints, 1959
   TWO_REG_1_IMM message604, addiu[32], $a6, $a7, 0, test_ints, 45791
   TWO_REG_1_IMM message605, addiu[32], $s3, $a5, 12, test_ints, 19034
   TWO_REG_1_IMM message606, addiu[32], $s0, $a4, 40, test_ints, 12529
   TWO_REG_1_IMM message607, addiu[32], $a5, $a4, 8, test_ints, 40680
   TWO_REG_1_IMM message608, addiu[32], $t1, $s3, 4, test_ints, 828
   TWO_REG_1_IMM message609, addiu[32], $a3, $a0, 20, test_ints, 20951
   TWO_REG_1_IMM message610, addiu[32], $t0, $a3, 16, test_ints, 55726
   TWO_REG_1_IMM message611, addiu[32], $a4, $s0, 8, test_ints, 53562
   TWO_REG_1_IMM message612, addiu[32], $s0, $s6, 20, test_ints, 42143
   TWO_REG_1_IMM message613, addiu[32], $s3, $s0, 4, test_ints, 16555
   TWO_REG_1_IMM message614, addiu[32], $a4, $a0, 20, test_ints, 24593
   TWO_REG_1_IMM message615, addiu[32], $t0, $s7, 36, test_ints, 44457
   TWO_REG_1_IMM message616, addiu[32], $a6, $s7, 8, test_ints, 61417
   TWO_REG_1_IMM message617, addiu[32], $a7, $s3, 0, test_ints, 62551
   TWO_REG_1_IMM message618, addiu[32], $a6, $s5, 40, test_ints, 24670
   TWO_REG_1_IMM message619, addiu[32], $a1, $a4, 16, test_ints, 41561
   TWO_REG_1_IMM message620, addiu[32], $s2, $s5, 32, test_ints, 39341
   TWO_REG_1_IMM message621, addiu[32], $s5, $a2, 16, test_ints, 36181
   TWO_REG_1_IMM message622, addiu[32], $s6, $s5, 12, test_ints, 36862
   TWO_REG_1_IMM message623, addiu[32], $s0, $s0, 0, test_ints, 21558
   TWO_REG_1_IMM message624, addiu[32], $a3, $s2, 28, test_ints, 28810
   TWO_REG_1_IMM message625, addiu[32], $s5, $s0, 24, test_ints, 42131
   TWO_REG_1_IMM message626, addiu[32], $s5, $s0, 40, test_ints, 57450
   TWO_REG_1_IMM message627, addiu[32], $a4, $t2, 0, test_ints, 20082
   TWO_REG_1_IMM message628, addiu[32], $s4, $a0, 44, test_ints, 17217
   TWO_REG_1_IMM message629, addiu[32], $s6, $s7, 44, test_ints, 11556
   TWO_REG_1_IMM message630, addiu[32], $a1, $t2, 12, test_ints, 54065

# addiu[48]
   ONE_REG_1_IMM message631, addiu[48], $a6, 28, test_ints, -465671856
   ONE_REG_1_IMM message632, addiu[48], $a7, 32, test_ints, 1143693630
   ONE_REG_1_IMM message633, addiu[48], $t1, 16, test_ints, 1561085778
   ONE_REG_1_IMM message634, addiu[48], $t2, 8, test_ints, -617608244
   ONE_REG_1_IMM message635, addiu[48], $a7, 44, test_ints, 1288814930
   ONE_REG_1_IMM message636, addiu[48], $t2, 8, test_ints, -1256568320
   ONE_REG_1_IMM message637, addiu[48], $a5, 12, test_ints, 236779048
   ONE_REG_1_IMM message638, addiu[48], $a5, 20, test_ints, -42931667
   ONE_REG_1_IMM message639, addiu[48], $a0, 4, test_ints, 1928300342
   ONE_REG_1_IMM message640, addiu[48], $s4, 36, test_ints, -1529565618
   ONE_REG_1_IMM message641, addiu[48], $a4, 8, test_ints, -500125624
   ONE_REG_1_IMM message642, addiu[48], $a0, 24, test_ints, 850131496
   ONE_REG_1_IMM message643, addiu[48], $s0, 36, test_ints, -587248398
   ONE_REG_1_IMM message644, addiu[48], $s7, 24, test_ints, -921250080
   ONE_REG_1_IMM message645, addiu[48], $a4, 32, test_ints, -1791276953
   ONE_REG_1_IMM message646, addiu[48], $s7, 36, test_ints, 1476596520
   ONE_REG_1_IMM message647, addiu[48], $a1, 40, test_ints, -1625976710
   ONE_REG_1_IMM message648, addiu[48], $a7, 24, test_ints, -2040070494
   ONE_REG_1_IMM message649, addiu[48], $t0, 12, test_ints, 1223202760
   ONE_REG_1_IMM message650, addiu[48], $a3, 36, test_ints, -45966582
   ONE_REG_1_IMM message651, addiu[48], $a0, 44, test_ints, -918691842
   ONE_REG_1_IMM message652, addiu[48], $s5, 44, test_ints, -1213869130
   ONE_REG_1_IMM message653, addiu[48], $t3, 24, test_ints, 762950234
   ONE_REG_1_IMM message654, addiu[48], $s6, 20, test_ints, -192620833
   ONE_REG_1_IMM message655, addiu[48], $s6, 28, test_ints, -660880494
   ONE_REG_1_IMM message656, addiu[48], $t3, 4, test_ints, 291054978
   ONE_REG_1_IMM message657, addiu[48], $a7, 8, test_ints, -744642069
   ONE_REG_1_IMM message658, addiu[48], $s4, 24, test_ints, -323771572
   ONE_REG_1_IMM message659, addiu[48], $a1, 40, test_ints, -1592932734
   ONE_REG_1_IMM message660, addiu[48], $s7, 12, test_ints, -670559898

# addiu[gp48]
   TWO_REG_1_IMM_SPECIAL message661, addiu[gp48], $gp, $s1, 16, test_ints, 1311809568
   TWO_REG_1_IMM_SPECIAL message662, addiu[gp48], $gp, $t2, 12, test_ints, -1070341696
   TWO_REG_1_IMM_SPECIAL message663, addiu[gp48], $gp, $a7, 12, test_ints, 1874428017
   TWO_REG_1_IMM_SPECIAL message664, addiu[gp48], $gp, $s0, 28, test_ints, -2105214727
   TWO_REG_1_IMM_SPECIAL message665, addiu[gp48], $gp, $s7, 24, test_ints, 1295869856
   TWO_REG_1_IMM_SPECIAL message666, addiu[gp48], $gp, $s0, 16, test_ints, -245720420
   TWO_REG_1_IMM_SPECIAL message667, addiu[gp48], $gp, $a4, 12, test_ints, 631876925
   TWO_REG_1_IMM_SPECIAL message668, addiu[gp48], $gp, $a5, 44, test_ints, -2073354357
   TWO_REG_1_IMM_SPECIAL message669, addiu[gp48], $gp, $s0, 32, test_ints, 2112095053
   TWO_REG_1_IMM_SPECIAL message670, addiu[gp48], $gp, $t0, 0, test_ints, 813958861
   TWO_REG_1_IMM_SPECIAL message671, addiu[gp48], $gp, $s1, 40, test_ints, -1901698450
   TWO_REG_1_IMM_SPECIAL message672, addiu[gp48], $gp, $t3, 40, test_ints, -1349164963
   TWO_REG_1_IMM_SPECIAL message673, addiu[gp48], $gp, $t2, 40, test_ints, -1289376568
   TWO_REG_1_IMM_SPECIAL message674, addiu[gp48], $gp, $t0, 8, test_ints, 431061413
   TWO_REG_1_IMM_SPECIAL message675, addiu[gp48], $gp, $a2, 32, test_ints, 455103016
   TWO_REG_1_IMM_SPECIAL message676, addiu[gp48], $gp, $t3, 36, test_ints, 1292360594
   TWO_REG_1_IMM_SPECIAL message677, addiu[gp48], $gp, $t0, 0, test_ints, -111832532
   TWO_REG_1_IMM_SPECIAL message678, addiu[gp48], $gp, $s7, 12, test_ints, 291016344
   TWO_REG_1_IMM_SPECIAL message679, addiu[gp48], $gp, $a5, 32, test_ints, 182944358
   TWO_REG_1_IMM_SPECIAL message680, addiu[gp48], $gp, $s6, 20, test_ints, -381820280
   TWO_REG_1_IMM_SPECIAL message681, addiu[gp48], $gp, $a7, 44, test_ints, -62091552
   TWO_REG_1_IMM_SPECIAL message682, addiu[gp48], $gp, $t0, 28, test_ints, 382131998
   TWO_REG_1_IMM_SPECIAL message683, addiu[gp48], $gp, $a7, 0, test_ints, 681196701
   TWO_REG_1_IMM_SPECIAL message684, addiu[gp48], $gp, $t3, 40, test_ints, 328178348
   TWO_REG_1_IMM_SPECIAL message685, addiu[gp48], $gp, $a3, 4, test_ints, -1199714982
   TWO_REG_1_IMM_SPECIAL message686, addiu[gp48], $gp, $t2, 32, test_ints, 461175938
   TWO_REG_1_IMM_SPECIAL message687, addiu[gp48], $gp, $a3, 40, test_ints, -445495488
   TWO_REG_1_IMM_SPECIAL message688, addiu[gp48], $gp, $t1, 0, test_ints, -265869360
   TWO_REG_1_IMM_SPECIAL message689, addiu[gp48], $gp, $s6, 12, test_ints, 918571223
   TWO_REG_1_IMM_SPECIAL message690, addiu[gp48], $gp, $s3, 28, test_ints, -557718104

# addiu[gp.b]
   TWO_REG_1_IMM_SPECIAL message691, addiu[gp.b], $gp, $a1, 8, test_ints, 78600
   TWO_REG_1_IMM_SPECIAL message692, addiu[gp.b], $gp, $a4, 24, test_ints, 57200
   TWO_REG_1_IMM_SPECIAL message693, addiu[gp.b], $gp, $s5, 28, test_ints, 116132
   TWO_REG_1_IMM_SPECIAL message694, addiu[gp.b], $gp, $t1, 4, test_ints, 53256
   TWO_REG_1_IMM_SPECIAL message695, addiu[gp.b], $gp, $a3, 32, test_ints, 259917
   TWO_REG_1_IMM_SPECIAL message696, addiu[gp.b], $gp, $s4, 20, test_ints, 178338
   TWO_REG_1_IMM_SPECIAL message697, addiu[gp.b], $gp, $a4, 36, test_ints, 62233
   TWO_REG_1_IMM_SPECIAL message698, addiu[gp.b], $gp, $a1, 4, test_ints, 55788
   TWO_REG_1_IMM_SPECIAL message699, addiu[gp.b], $gp, $s7, 24, test_ints, 89684
   TWO_REG_1_IMM_SPECIAL message700, addiu[gp.b], $gp, $s0, 36, test_ints, 122755
   TWO_REG_1_IMM_SPECIAL message701, addiu[gp.b], $gp, $t0, 0, test_ints, 230802
   TWO_REG_1_IMM_SPECIAL message702, addiu[gp.b], $gp, $a6, 24, test_ints, 147527
   TWO_REG_1_IMM_SPECIAL message703, addiu[gp.b], $gp, $s7, 24, test_ints, 194424
   TWO_REG_1_IMM_SPECIAL message704, addiu[gp.b], $gp, $s7, 8, test_ints, 69000
   TWO_REG_1_IMM_SPECIAL message705, addiu[gp.b], $gp, $s3, 4, test_ints, 198343
   TWO_REG_1_IMM_SPECIAL message706, addiu[gp.b], $gp, $a7, 24, test_ints, 77218
   TWO_REG_1_IMM_SPECIAL message707, addiu[gp.b], $gp, $a4, 32, test_ints, 245490
   TWO_REG_1_IMM_SPECIAL message708, addiu[gp.b], $gp, $s1, 20, test_ints, 177642
   TWO_REG_1_IMM_SPECIAL message709, addiu[gp.b], $gp, $a2, 32, test_ints, 187600
   TWO_REG_1_IMM_SPECIAL message710, addiu[gp.b], $gp, $s3, 36, test_ints, 31198
   TWO_REG_1_IMM_SPECIAL message711, addiu[gp.b], $gp, $a0, 4, test_ints, 162979
   TWO_REG_1_IMM_SPECIAL message712, addiu[gp.b], $gp, $s7, 28, test_ints, 78097
   TWO_REG_1_IMM_SPECIAL message713, addiu[gp.b], $gp, $a5, 28, test_ints, 91168
   TWO_REG_1_IMM_SPECIAL message714, addiu[gp.b], $gp, $a1, 12, test_ints, 237458
   TWO_REG_1_IMM_SPECIAL message715, addiu[gp.b], $gp, $a3, 32, test_ints, 100760
   TWO_REG_1_IMM_SPECIAL message716, addiu[gp.b], $gp, $s6, 36, test_ints, 41880
   TWO_REG_1_IMM_SPECIAL message717, addiu[gp.b], $gp, $a5, 12, test_ints, 116608
   TWO_REG_1_IMM_SPECIAL message718, addiu[gp.b], $gp, $s5, 36, test_ints, 116086
   TWO_REG_1_IMM_SPECIAL message719, addiu[gp.b], $gp, $s2, 12, test_ints, 251668
   TWO_REG_1_IMM_SPECIAL message720, addiu[gp.b], $gp, $a7, 16, test_ints, 259488

# addiu[gp.w]
   TWO_REG_1_IMM_SPECIAL message721, addiu[gp.w], $gp, $a7, 16, test_ints, 305024
   TWO_REG_1_IMM_SPECIAL message722, addiu[gp.w], $gp, $a0, 24, test_ints, 539200
   TWO_REG_1_IMM_SPECIAL message723, addiu[gp.w], $gp, $t3, 32, test_ints, 1349636
   TWO_REG_1_IMM_SPECIAL message724, addiu[gp.w], $gp, $a1, 36, test_ints, 2010128
   TWO_REG_1_IMM_SPECIAL message725, addiu[gp.w], $gp, $s5, 36, test_ints, 1795192
   TWO_REG_1_IMM_SPECIAL message726, addiu[gp.w], $gp, $s6, 4, test_ints, 616692
   TWO_REG_1_IMM_SPECIAL message727, addiu[gp.w], $gp, $a1, 28, test_ints, 1923680
   TWO_REG_1_IMM_SPECIAL message728, addiu[gp.w], $gp, $s3, 8, test_ints, 962424
   TWO_REG_1_IMM_SPECIAL message729, addiu[gp.w], $gp, $a6, 16, test_ints, 1030460
   TWO_REG_1_IMM_SPECIAL message730, addiu[gp.w], $gp, $s6, 0, test_ints, 1735092
   TWO_REG_1_IMM_SPECIAL message731, addiu[gp.w], $gp, $s4, 24, test_ints, 677528
   TWO_REG_1_IMM_SPECIAL message732, addiu[gp.w], $gp, $a7, 44, test_ints, 755268
   TWO_REG_1_IMM_SPECIAL message733, addiu[gp.w], $gp, $t0, 24, test_ints, 86156
   TWO_REG_1_IMM_SPECIAL message734, addiu[gp.w], $gp, $s0, 4, test_ints, 1845232
   TWO_REG_1_IMM_SPECIAL message735, addiu[gp.w], $gp, $s5, 32, test_ints, 1762372
   TWO_REG_1_IMM_SPECIAL message736, addiu[gp.w], $gp, $a6, 4, test_ints, 1500128
   TWO_REG_1_IMM_SPECIAL message737, addiu[gp.w], $gp, $a1, 36, test_ints, 1627292
   TWO_REG_1_IMM_SPECIAL message738, addiu[gp.w], $gp, $t1, 4, test_ints, 750296
   TWO_REG_1_IMM_SPECIAL message739, addiu[gp.w], $gp, $s3, 44, test_ints, 414428
   TWO_REG_1_IMM_SPECIAL message740, addiu[gp.w], $gp, $s3, 8, test_ints, 1533636
   TWO_REG_1_IMM_SPECIAL message741, addiu[gp.w], $gp, $s0, 20, test_ints, 1731624
   TWO_REG_1_IMM_SPECIAL message742, addiu[gp.w], $gp, $a3, 0, test_ints, 453312
   TWO_REG_1_IMM_SPECIAL message743, addiu[gp.w], $gp, $s0, 40, test_ints, 1488200
   TWO_REG_1_IMM_SPECIAL message744, addiu[gp.w], $gp, $a3, 28, test_ints, 446292
   TWO_REG_1_IMM_SPECIAL message745, addiu[gp.w], $gp, $a1, 40, test_ints, 827552
   TWO_REG_1_IMM_SPECIAL message746, addiu[gp.w], $gp, $t3, 36, test_ints, 1891912
   TWO_REG_1_IMM_SPECIAL message747, addiu[gp.w], $gp, $t0, 8, test_ints, 402268
   TWO_REG_1_IMM_SPECIAL message748, addiu[gp.w], $gp, $a0, 28, test_ints, 878732
   TWO_REG_1_IMM_SPECIAL message749, addiu[gp.w], $gp, $t1, 20, test_ints, 438880
   TWO_REG_1_IMM_SPECIAL message750, addiu[gp.w], $gp, $t1, 28, test_ints, 1142592

# addiu[r1.sp]
   TWO_REG_1_IMM_SPECIAL message751, addiu[r1.sp], $sp, $s1, 0, test_ints, 92
   TWO_REG_1_IMM_SPECIAL message752, addiu[r1.sp], $sp, $s1, 0, test_ints, 212
   TWO_REG_1_IMM_SPECIAL message753, addiu[r1.sp], $sp, $a1, 36, test_ints, 32
   TWO_REG_1_IMM_SPECIAL message754, addiu[r1.sp], $sp, $a1, 28, test_ints, 152
   TWO_REG_1_IMM_SPECIAL message755, addiu[r1.sp], $sp, $a2, 12, test_ints, 76
   TWO_REG_1_IMM_SPECIAL message756, addiu[r1.sp], $sp, $s3, 40, test_ints, 208
   TWO_REG_1_IMM_SPECIAL message757, addiu[r1.sp], $sp, $s2, 40, test_ints, 240
   TWO_REG_1_IMM_SPECIAL message758, addiu[r1.sp], $sp, $s1, 44, test_ints, 20
   TWO_REG_1_IMM_SPECIAL message759, addiu[r1.sp], $sp, $a2, 20, test_ints, 244
   TWO_REG_1_IMM_SPECIAL message760, addiu[r1.sp], $sp, $a0, 0, test_ints, 148
   TWO_REG_1_IMM_SPECIAL message761, addiu[r1.sp], $sp, $a2, 20, test_ints, 236
   TWO_REG_1_IMM_SPECIAL message762, addiu[r1.sp], $sp, $s2, 8, test_ints, 128
   TWO_REG_1_IMM_SPECIAL message763, addiu[r1.sp], $sp, $s2, 32, test_ints, 240
   TWO_REG_1_IMM_SPECIAL message764, addiu[r1.sp], $sp, $s3, 36, test_ints, 28
   TWO_REG_1_IMM_SPECIAL message765, addiu[r1.sp], $sp, $a0, 32, test_ints, 244
   TWO_REG_1_IMM_SPECIAL message766, addiu[r1.sp], $sp, $s2, 28, test_ints, 216
   TWO_REG_1_IMM_SPECIAL message767, addiu[r1.sp], $sp, $a1, 4, test_ints, 156
   TWO_REG_1_IMM_SPECIAL message768, addiu[r1.sp], $sp, $s0, 28, test_ints, 168
   TWO_REG_1_IMM_SPECIAL message769, addiu[r1.sp], $sp, $a3, 4, test_ints, 72
   TWO_REG_1_IMM_SPECIAL message770, addiu[r1.sp], $sp, $a3, 4, test_ints, 144
   TWO_REG_1_IMM_SPECIAL message771, addiu[r1.sp], $sp, $a2, 44, test_ints, 180
   TWO_REG_1_IMM_SPECIAL message772, addiu[r1.sp], $sp, $s2, 8, test_ints, 200
   TWO_REG_1_IMM_SPECIAL message773, addiu[r1.sp], $sp, $s2, 16, test_ints, 132
   TWO_REG_1_IMM_SPECIAL message774, addiu[r1.sp], $sp, $s3, 44, test_ints, 120
   TWO_REG_1_IMM_SPECIAL message775, addiu[r1.sp], $sp, $s0, 44, test_ints, 160
   TWO_REG_1_IMM_SPECIAL message776, addiu[r1.sp], $sp, $a0, 24, test_ints, 152
   TWO_REG_1_IMM_SPECIAL message777, addiu[r1.sp], $sp, $a2, 28, test_ints, 84
   TWO_REG_1_IMM_SPECIAL message778, addiu[r1.sp], $sp, $s2, 28, test_ints, 240
   TWO_REG_1_IMM_SPECIAL message779, addiu[r1.sp], $sp, $s2, 44, test_ints, 140
   TWO_REG_1_IMM_SPECIAL message780, addiu[r1.sp], $sp, $s3, 24, test_ints, 172

# addiu[r2]
   TWO_REG_1_IMM message781, addiu[r2], $a0, $a0, 32, test_ints, 4
   TWO_REG_1_IMM message782, addiu[r2], $s3, $a2, 16, test_ints, 20
   TWO_REG_1_IMM message783, addiu[r2], $s2, $s1, 36, test_ints, 8
   TWO_REG_1_IMM message784, addiu[r2], $s3, $a1, 36, test_ints, 16
   TWO_REG_1_IMM message785, addiu[r2], $a1, $s3, 12, test_ints, 8
   TWO_REG_1_IMM message786, addiu[r2], $a3, $a2, 4, test_ints, 24
   TWO_REG_1_IMM message787, addiu[r2], $s0, $a0, 4, test_ints, 4
   TWO_REG_1_IMM message788, addiu[r2], $s3, $a3, 20, test_ints, 0
   TWO_REG_1_IMM message789, addiu[r2], $s0, $s1, 12, test_ints, 8
   TWO_REG_1_IMM message790, addiu[r2], $a0, $s3, 16, test_ints, 4
   TWO_REG_1_IMM message791, addiu[r2], $s0, $s1, 32, test_ints, 24
   TWO_REG_1_IMM message792, addiu[r2], $s2, $s2, 36, test_ints, 20
   TWO_REG_1_IMM message793, addiu[r2], $s1, $a1, 40, test_ints, 0
   TWO_REG_1_IMM message794, addiu[r2], $a3, $s0, 12, test_ints, 16
   TWO_REG_1_IMM message795, addiu[r2], $s0, $a0, 16, test_ints, 24
   TWO_REG_1_IMM message796, addiu[r2], $s0, $s1, 28, test_ints, 16
   TWO_REG_1_IMM message797, addiu[r2], $a2, $a2, 28, test_ints, 8
   TWO_REG_1_IMM message798, addiu[r2], $a1, $a3, 4, test_ints, 4
   TWO_REG_1_IMM message799, addiu[r2], $a1, $a1, 4, test_ints, 20
   TWO_REG_1_IMM message800, addiu[r2], $s3, $s2, 12, test_ints, 12
   TWO_REG_1_IMM message801, addiu[r2], $s3, $a1, 20, test_ints, 0
   TWO_REG_1_IMM message802, addiu[r2], $s1, $a0, 28, test_ints, 24
   TWO_REG_1_IMM message803, addiu[r2], $a1, $s3, 20, test_ints, 12
   TWO_REG_1_IMM message804, addiu[r2], $s0, $a0, 20, test_ints, 12
   TWO_REG_1_IMM message805, addiu[r2], $a3, $a0, 4, test_ints, 12
   TWO_REG_1_IMM message806, addiu[r2], $a3, $a2, 40, test_ints, 12
   TWO_REG_1_IMM message807, addiu[r2], $a3, $a3, 0, test_ints, 0
   TWO_REG_1_IMM message808, addiu[r2], $a1, $a3, 32, test_ints, 8
   TWO_REG_1_IMM message809, addiu[r2], $s0, $s1, 16, test_ints, 16
   TWO_REG_1_IMM message810, addiu[r2], $s2, $s3, 44, test_ints, 12

# addiu[rs5]
   ONE_REG_1_IMM message811, addiu[rs5], $a6, 16, test_ints, -4
   ONE_REG_1_IMM message812, addiu[rs5], $a6, 20, test_ints, 1
   ONE_REG_1_IMM message813, addiu[rs5], $s4, 32, test_ints, 1
   ONE_REG_1_IMM message814, addiu[rs5], $a5, 0, test_ints, -4
   ONE_REG_1_IMM message815, addiu[rs5], $t3, 8, test_ints, 1
   ONE_REG_1_IMM message816, addiu[rs5], $a7, 20, test_ints, -3
   ONE_REG_1_IMM message817, addiu[rs5], $s2, 44, test_ints, 0
   ONE_REG_1_IMM message818, addiu[rs5], $a2, 16, test_ints, -4
   ONE_REG_1_IMM message819, addiu[rs5], $a7, 0, test_ints, -2
   ONE_REG_1_IMM message820, addiu[rs5], $t1, 28, test_ints, 2
   ONE_REG_1_IMM message821, addiu[rs5], $a0, 4, test_ints, 2
   ONE_REG_1_IMM message822, addiu[rs5], $s4, 12, test_ints, 3
   ONE_REG_1_IMM message823, addiu[rs5], $s1, 12, test_ints, -1
   ONE_REG_1_IMM message824, addiu[rs5], $s6, 36, test_ints, 0
   ONE_REG_1_IMM message825, addiu[rs5], $a3, 16, test_ints, 2
   ONE_REG_1_IMM message826, addiu[rs5], $t0, 44, test_ints, 0
   ONE_REG_1_IMM message827, addiu[rs5], $t1, 20, test_ints, -1
   ONE_REG_1_IMM message828, addiu[rs5], $a5, 44, test_ints, -1
   ONE_REG_1_IMM message829, addiu[rs5], $t1, 12, test_ints, 3
   ONE_REG_1_IMM message830, addiu[rs5], $s4, 0, test_ints, -2
   ONE_REG_1_IMM message831, addiu[rs5], $s2, 36, test_ints, -1
   ONE_REG_1_IMM message832, addiu[rs5], $a4, 36, test_ints, 2
   ONE_REG_1_IMM message833, addiu[rs5], $s0, 12, test_ints, -3
   ONE_REG_1_IMM message834, addiu[rs5], $a3, 4, test_ints, 2
   ONE_REG_1_IMM message835, addiu[rs5], $s3, 32, test_ints, 3
   ONE_REG_1_IMM message836, addiu[rs5], $a2, 32, test_ints, 2
   ONE_REG_1_IMM message837, addiu[rs5], $s2, 36, test_ints, -4
   ONE_REG_1_IMM message838, addiu[rs5], $t1, 40, test_ints, -1
   ONE_REG_1_IMM message839, addiu[rs5], $s0, 0, test_ints, 2
   ONE_REG_1_IMM message840, addiu[rs5], $s3, 0, test_ints, 3

# addiu[neg]
   TWO_REG_1_IMM message841, addiu[neg], $t1, $a2, 0, test_ints, -3416
   TWO_REG_1_IMM message842, addiu[neg], $t2, $s5, 24, test_ints, -602
   TWO_REG_1_IMM message843, addiu[neg], $a4, $a7, 8, test_ints, -3422
   TWO_REG_1_IMM message844, addiu[neg], $s2, $a5, 40, test_ints, -933
   TWO_REG_1_IMM message845, addiu[neg], $a7, $t3, 12, test_ints, -3605
   TWO_REG_1_IMM message846, addiu[neg], $s4, $a7, 24, test_ints, -775
   TWO_REG_1_IMM message847, addiu[neg], $s7, $a6, 28, test_ints, -2205
   TWO_REG_1_IMM message848, addiu[neg], $t1, $a3, 16, test_ints, -1986
   TWO_REG_1_IMM message849, addiu[neg], $t1, $a4, 8, test_ints, -15
   TWO_REG_1_IMM message850, addiu[neg], $s2, $a4, 40, test_ints, -1810
   TWO_REG_1_IMM message851, addiu[neg], $a4, $s0, 16, test_ints, -2982
   TWO_REG_1_IMM message852, addiu[neg], $s5, $s3, 44, test_ints, -1481
   TWO_REG_1_IMM message853, addiu[neg], $s2, $s6, 24, test_ints, -2498
   TWO_REG_1_IMM message854, addiu[neg], $a2, $s1, 20, test_ints, -1505
   TWO_REG_1_IMM message855, addiu[neg], $s7, $s4, 24, test_ints, -1101
   TWO_REG_1_IMM message856, addiu[neg], $s7, $s3, 12, test_ints, -2128
   TWO_REG_1_IMM message857, addiu[neg], $s7, $t2, 16, test_ints, -1085
   TWO_REG_1_IMM message858, addiu[neg], $s6, $t2, 32, test_ints, -2482
   TWO_REG_1_IMM message859, addiu[neg], $a6, $a4, 16, test_ints, -379
   TWO_REG_1_IMM message860, addiu[neg], $s7, $a3, 0, test_ints, -1901
   TWO_REG_1_IMM message861, addiu[neg], $s2, $s7, 32, test_ints, -3732
   TWO_REG_1_IMM message862, addiu[neg], $a4, $s5, 20, test_ints, -3683
   TWO_REG_1_IMM message863, addiu[neg], $a6, $s7, 0, test_ints, -1321
   TWO_REG_1_IMM message864, addiu[neg], $a6, $s0, 40, test_ints, -2546
   TWO_REG_1_IMM message865, addiu[neg], $a6, $a2, 44, test_ints, -208
   TWO_REG_1_IMM message866, addiu[neg], $a0, $a7, 44, test_ints, -1786
   TWO_REG_1_IMM message867, addiu[neg], $s4, $a7, 36, test_ints, -2495
   TWO_REG_1_IMM message868, addiu[neg], $s3, $a2, 4, test_ints, -717
   TWO_REG_1_IMM message869, addiu[neg], $t1, $a1, 4, test_ints, -3781
   TWO_REG_1_IMM message870, addiu[neg], $a6, $s2, 32, test_ints, -2700

   move $a0, $zero
   restore 48, $30,$31,$16-$24,$28
   restore.jrc 32
   .set  macro
   .set  reorder
   .end  main
   .size main, .-main
