/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- MemCheck: some non-generic asm implementations of mc_main.c     */
/*--- functions                                                    ---*/
/*---                                                mc_main_asm.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2018 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Having these in mc_main.c gives undefined references at link time,
   when compiling with lto. Having them in a separate file solves this.
   Also, for some toolchain, we might maybe need to disable lto. */

// A bunch of include only needed for mc_include.h
#include "pub_tool_basics.h"
#include "pub_tool_poolalloc.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_tooliface.h"

#include "mc_include.h"

// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
__asm__( /* Derived from the 32 bit assembly helper */
".text                                  \n"
".align 2                               \n"
".global vgMemCheck_helperc_LOADV64le   \n"
".type   vgMemCheck_helperc_LOADV64le, %function \n"
"vgMemCheck_helperc_LOADV64le:          \n"
"      tst    r0, #7                    \n"
"      movw   r3, #:lower16:primary_map \n"
"      bne    .LLV64LEc4                \n" // if misaligned
"      lsr    r2, r0, #16               \n"
"      movt   r3, #:upper16:primary_map \n"
"      ldr    r2, [r3, r2, lsl #2]      \n"
"      uxth   r1, r0                    \n" // r1 is 0-(16)-0 X-(13)-X 000
"      movw   r3, #0xAAAA               \n"
"      lsr    r1, r1, #2                \n" // r1 is 0-(16)-0 00 X-(13)-X 0
"      ldrh   r1, [r2, r1]              \n"
"      cmp    r1, r3                    \n" // 0xAAAA == VA_BITS16_DEFINED
"      bne    .LLV64LEc0                \n" // if !all_defined
"      mov    r1, #0x0                  \n" // 0x0 == V_BITS32_DEFINED
"      mov    r0, #0x0                  \n" // 0x0 == V_BITS32_DEFINED
"      bx     lr                        \n"
".LLV64LEc0:                            \n"
"      movw   r3, #0x5555               \n"
"      cmp    r1, r3                    \n" // 0x5555 == VA_BITS16_UNDEFINED
"      bne    .LLV64LEc4                \n" // if !all_undefined
"      mov    r1, #0xFFFFFFFF           \n" // 0xFFFFFFFF == V_BITS32_UNDEFINED
"      mov    r0, #0xFFFFFFFF           \n" // 0xFFFFFFFF == V_BITS32_UNDEFINED
"      bx     lr                        \n"
".LLV64LEc4:                            \n"
"      push   {r4, lr}                  \n"
"      mov    r2, #0                    \n"
"      mov    r1, #64                   \n"
"      bl     mc_LOADVn_slow            \n"
"      pop    {r4, pc}                  \n"
".size vgMemCheck_helperc_LOADV64le, .-vgMemCheck_helperc_LOADV64le \n"
".previous\n"
);

#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd))
__asm__(
".text\n"
".align 16\n"
".global vgMemCheck_helperc_LOADV64le\n"
".type   vgMemCheck_helperc_LOADV64le, @function\n"
"vgMemCheck_helperc_LOADV64le:\n"
"      test   $0x7,  %eax\n"
"      jne    .LLV64LE2\n"          /* jump if not aligned */
"      mov    %eax,  %ecx\n"
"      movzwl %ax,   %edx\n"
"      shr    $0x10, %ecx\n"
"      mov    primary_map(,%ecx,4), %ecx\n"
"      shr    $0x3,  %edx\n"
"      movzwl (%ecx,%edx,2), %edx\n"
"      cmp    $0xaaaa, %edx\n"
"      jne    .LLV64LE1\n"          /* jump if not all defined */
"      xor    %eax, %eax\n"         /* return 0 in edx:eax */
"      xor    %edx, %edx\n"
"      ret\n"
".LLV64LE1:\n"
"      cmp    $0x5555, %edx\n"
"      jne    .LLV64LE2\n"         /* jump if not all undefined */
"      or     $0xffffffff, %eax\n" /* else return all bits set in edx:eax */
"      or     $0xffffffff, %edx\n"
"      ret\n"
".LLV64LE2:\n"
"      xor    %ecx,  %ecx\n"  /* tail call to mc_LOADVn_slow(a, 64, 0) */
"      mov    $64,   %edx\n"
"      jmp    mc_LOADVn_slow\n"
".size vgMemCheck_helperc_LOADV64le, .-vgMemCheck_helperc_LOADV64le\n"
".previous\n"
);

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
// is in mc_main.c
#endif


// Non-generic assembly for arm32-linux
#if ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
    && defined(VGP_arm_linux)
__asm__( /* Derived from NCode template */
".text                                  \n"
".align 2                               \n"
".global vgMemCheck_helperc_LOADV32le   \n"
".type   vgMemCheck_helperc_LOADV32le, %function \n"
"vgMemCheck_helperc_LOADV32le:          \n"
"      tst    r0, #3                    \n" // 1
"      movw   r3, #:lower16:primary_map \n" // 1
"      bne    .LLV32LEc4                \n" // 2  if misaligned
"      lsr    r2, r0, #16               \n" // 3
"      movt   r3, #:upper16:primary_map \n" // 3
"      ldr    r2, [r3, r2, lsl #2]      \n" // 4
"      uxth   r1, r0                    \n" // 4
"      ldrb   r1, [r2, r1, lsr #2]      \n" // 5
"      cmp    r1, #0xAA                 \n" // 6  0xAA == VA_BITS8_DEFINED
"      bne    .LLV32LEc0                \n" // 7  if !all_defined
"      mov    r0, #0x0                  \n" // 8  0x0 == V_BITS32_DEFINED
"      bx     lr                        \n" // 9
".LLV32LEc0:                            \n"
"      cmp    r1, #0x55                 \n" // 0x55 == VA_BITS8_UNDEFINED
"      bne    .LLV32LEc4                \n" // if !all_undefined
"      mov    r0, #0xFFFFFFFF           \n" // 0xFFFFFFFF == V_BITS32_UNDEFINED
"      bx     lr                        \n"
".LLV32LEc4:                            \n"
"      push   {r4, lr}                  \n"
"      mov    r2, #0                    \n"
"      mov    r1, #32                   \n"
"      bl     mc_LOADVn_slow            \n"
"      pop    {r4, pc}                  \n"
".size vgMemCheck_helperc_LOADV32le, .-vgMemCheck_helperc_LOADV32le \n"
".previous\n"
);

#elif ENABLE_ASSEMBLY_HELPERS && defined(PERF_FAST_LOADV) \
      && (defined(VGP_x86_linux) || defined(VGP_x86_solaris))
__asm__(
".text\n"
".align 16\n"
".global vgMemCheck_helperc_LOADV32le\n"
".type   vgMemCheck_helperc_LOADV32le, @function\n"
"vgMemCheck_helperc_LOADV32le:\n"
"      test   $0x3,  %eax\n"
"      jnz    .LLV32LE2\n"         /* jump if misaligned */
"      mov    %eax,  %edx\n"
"      shr    $16,   %edx\n"
"      mov    primary_map(,%edx,4), %ecx\n"
"      movzwl %ax,   %edx\n"
"      shr    $2,    %edx\n"
"      movzbl (%ecx,%edx,1), %edx\n"
"      cmp    $0xaa, %edx\n"       /* compare to VA_BITS8_DEFINED */
"      jne    .LLV32LE1\n"         /* jump if not completely defined */
"      xor    %eax,  %eax\n"       /* else return V_BITS32_DEFINED */
"      ret\n"
".LLV32LE1:\n"
"      cmp    $0x55, %edx\n"       /* compare to VA_BITS8_UNDEFINED */
"      jne    .LLV32LE2\n"         /* jump if not completely undefined */
"      or     $0xffffffff, %eax\n" /* else return V_BITS32_UNDEFINED */
"      ret\n"
".LLV32LE2:\n"
"      xor    %ecx,  %ecx\n"       /* tail call mc_LOADVn_slow(a, 32, 0) */
"      mov    $32,   %edx\n"
"      jmp    mc_LOADVn_slow\n"
".size vgMemCheck_helperc_LOADV32le, .-vgMemCheck_helperc_LOADV32le\n"
".previous\n"
);

#else
// Generic for all platforms except {arm32,x86}-linux and x86-solaris
// is in mc_main.c
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
