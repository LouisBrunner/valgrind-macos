
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "ume.h"

void jmp_with_stack(void(*rip)(void), Addr rsp)
{
   asm volatile (
      "movq  %1, %%rsp;"       // set rsp
      "pushq %%rax;"          // push rsp
      "xorq  %%rax,%%rax;"    // clear registers
      "xorq  %%rbx,%%rbx;"
      "xorq  %%rcx,%%rcx;"
      "xorq  %%rdx,%%rdx;"
      "xorq  %%rsi,%%rsi;"
      "xorq  %%rdi,%%rdi;"
      "xorq  %%rbp,%%rbp;"
      "xorq  %%r8, %%r8;"
      "xorq  %%r9, %%r9;"
      "xorq  %%r10,%%r10;"
      "xorq  %%r11,%%r11;"
      "xorq  %%r12,%%r12;"
      "xorq  %%r13,%%r13;"
      "xorq  %%r14,%%r14;"
      "xorq  %%r15,%%r15;"
      "ret"                   // return into entry
      : : "a" (rip), "r" (rsp));

   // we should never get here
   for(;;)
      asm volatile("ud2");
} 
