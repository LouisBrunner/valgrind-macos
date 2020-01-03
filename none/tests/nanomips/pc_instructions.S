   .data
   test_data:
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
   print_4_hex_num:
   .ascii "%08x %08x %08x %08x\n"
   .byte 0

   .text
   .align   2

   .macro ADDIU_PC  instruction, label, label2, reg, imm
      .data
      &label:
      .ascii "&instruction &reg %d, &imm\n"
      .byte 0
      .text
      &label2:
      &instruction &reg, &imm
      move $a1, &reg
      li $a0, &label
      li $t0, &label2
      subu $a1, $a1, $t0
      lapc[48] $t9, mwrap_printf
      jalrc $t9
   .endm

   .macro ALUI_PC label, label2, reg, imm
      .data
      &label:
      .ascii "aluipc &reg %x, %%pcrel_hi(&imm)\n"
      .byte 0
      .text
      &label2:
      aluipc &reg, %pcrel_hi(&imm)
      move $a1, &reg
      li $a0, &label
      lui $t0, %hi(&label2)
      subu $a1, $a1, $t0
      lapc[48] $t9, mwrap_printf
      jalrc $t9
   .endm

   .macro LOAD_PC label, reg, offset
      .data
      &label:
      .ascii "lwpc &reg %08x &offset\n"
      .byte 0
      .text
      lwpc &reg, test_data + &offset
      move $a1, &reg
      li $a0, &label
      lapc[48] $t9, mwrap_printf
      jalrc $t9
   .endm


   .macro STORE_PC label, reg, offset, val
      .data
      &label:
      .ascii "swpc &reg &val %08x &offset\n"
      .byte 0
      .text
      li &reg, &val
      swpc &reg, test_data + &offset
      li $a0, &label
      lapc[48] $t9, mwrap_printf
      jalrc $t9
      balc print_test_data
      balc cleanup_test_data
   .endm

   .globl cleanup_test_data
   .ent   cleanup_test_data
   .type  cleanup_test_data, @function
cleanup_test_data:
   li $t0, test_data
   li $t1, 0
loop:
   swxs $t1, $t1($t0)
   addiu $t1, $t1, 1
   bneic $t1, 16, loop
   jrc   $ra
   .end  cleanup_test_data
   .size cleanup_test_data, .-cleanup_test_data

   .globl print_test_data
   .ent   print_test_data
   .type  print_test_data, @function
print_test_data:
   save  16, $ra
   li $s0, test_data
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
   .end  print_test_data
   .size print_test_data, .-print_test_data



   .globl main
   .ent   main
   .type  main, @function
main:
   .set  noreorder
   .set  nomacro
   save  16, $ra

   # addiupc[32]
   ADDIU_PC addiupc[32],message1, label1, $s0,-883304
   ADDIU_PC addiupc[32],message2, label2, $a2,-1367040
   ADDIU_PC addiupc[32],message3, label3, $t1,-558914
   ADDIU_PC addiupc[32],message4, label4, $a1,-430560
   ADDIU_PC addiupc[32],message5, label5, $a5,-1160722
   ADDIU_PC addiupc[32],message6, label6, $s0,-1342112
   ADDIU_PC addiupc[32],message7, label7, $a5,1219852
   ADDIU_PC addiupc[32],message8, label8, $s7,-1244934
   ADDIU_PC addiupc[32],message9, label9, $t0,-1726688
   ADDIU_PC addiupc[32],message10, label10, $a5,-1163280
   ADDIU_PC addiupc[32],message11, label11, $a4,1211532
   ADDIU_PC addiupc[32],message12, label12, $a1,-1657340
   ADDIU_PC addiupc[32],message13, label13, $s6,2023420
   ADDIU_PC addiupc[32],message14, label14, $a7,1287430
   ADDIU_PC addiupc[32],message15, label15, $a4,-51996

   # addiupc[48]
   ADDIU_PC addiupc[48],message16, label16, $a1,177236788
   ADDIU_PC addiupc[48],message17, label17, $s1,706698569
   ADDIU_PC addiupc[48],message18, label18, $s2,804137936
   ADDIU_PC addiupc[48],message19, label19, $s5,1189146130
   ADDIU_PC addiupc[48],message20, label20, $s6,-444831196
   ADDIU_PC addiupc[48],message21, label21, $s3,1296895950
   ADDIU_PC addiupc[48],message22, label22, $a5,-533232603
   ADDIU_PC addiupc[48],message23, label23, $a5,-1238141567
   ADDIU_PC addiupc[48],message24, label24, $a5,1438695404
   ADDIU_PC addiupc[48],message25, label25, $a2,-8616062
   ADDIU_PC addiupc[48],message26, label26, $a6,-1713242258
   ADDIU_PC addiupc[48],message27, label27, $t1,766052933
   ADDIU_PC addiupc[48],message28, label28, $s1,1052359348
   ADDIU_PC addiupc[48],message29, label29, $a4,1696814736
   ADDIU_PC addiupc[48],message30, label30, $a5,257472883

   # aluipc
   ALUI_PC message31, label31, $t2,0xdbe13000
   ALUI_PC message32, label32, $a3,0x5c44b000
   ALUI_PC message33, label33, $s1,0xe4671000
   ALUI_PC message34, label34, $a4,0x23216000
   ALUI_PC message35, label35, $a3,0x27b94000
   ALUI_PC message36, label36, $a0,0x249d8000
   ALUI_PC message37, label37, $a3,0x4bf6a000
   ALUI_PC message38, label38, $a5,0x6aa69000
   ALUI_PC message39, label39, $s1,0xc1fa1000
   ALUI_PC message40, label40, $a6,0x9b957000
   ALUI_PC message41, label41, $s0,0x54718000
   ALUI_PC message42, label42, $s0,0x1c2ca000
   ALUI_PC message43, label43, $s6,0xd7088000
   ALUI_PC message44, label44, $s4,0xdfabd000
   ALUI_PC message45, label45, $a7,0x33879000

   # lwpc
   LOAD_PC message46, $s6, 24
   LOAD_PC message47, $s5, 8
   LOAD_PC message48, $s1, 12
   LOAD_PC message49, $s5, 44
   LOAD_PC message50, $s1, 32
   LOAD_PC message51, $t0, 16
   LOAD_PC message52, $s1, 28
   LOAD_PC message53, $t0, 44
   LOAD_PC message54, $a3, 24
   LOAD_PC message55, $t1, 32
   LOAD_PC message56, $s4, 52
   LOAD_PC message57, $t2, 0
   LOAD_PC message58, $s2, 24
   LOAD_PC message59, $t3, 0
   LOAD_PC message60, $a2, 8

   # swpc
   STORE_PC message61, $a4, 48, 0xc91aa0f8
   STORE_PC message62, $s2, 48, 0x6f730a24
   STORE_PC message63, $t0, 52, 0xc2c122ca
   STORE_PC message64, $a6, 16, 0x8e472ee0
   STORE_PC message65, $s0, 52, 0x027020a4
   STORE_PC message66, $t1, 20, 0x99a0cadc
   STORE_PC message67, $s6, 4, 0xbc8b7a8d
   STORE_PC message68, $s6, 36, 0xbef3a566
   STORE_PC message69, $t1, 40, 0xa373f6ed
   STORE_PC message70, $a4, 52, 0xfa483a90
   STORE_PC message71, $s0, 0, 0x8a788e97
   STORE_PC message72, $a0, 0, 0x401629e8
   STORE_PC message73, $a2, 28, 0x53ea1794
   STORE_PC message74, $s1, 32, 0xdba7309b
   STORE_PC message75, $a1, 24, 0x9bfd3f00

   move $a0, $zero
   restore 16, $ra
   jrc   $ra
   .set  macro
   .set  reorder
   .end  main
   .size main, .-main
