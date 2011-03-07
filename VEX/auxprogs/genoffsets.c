
/*--------------------------------------------------------------------*/
/*--- begin                                           genoffsets.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
      info@open-works.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include <stdio.h>

/* A program which, when compiled to assembly, exposes various guest
   state offsets.  The program isn't executed, since that breaks
   cross-compilation.

   It does rely on the assumption that 'my_offsetof(Ty,Field)' is
   folded to a constant at a compile time, which seems a bit dodgy
   to me.  On gcc4 it is possible to use __builtin_offsetof, which
   sounds safer, but that doesn't exist on older gccs.  Oh Well.
*/

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"
#include "../pub/libvex_guest_ppc32.h"
#include "../pub/libvex_guest_ppc64.h"
#include "../pub/libvex_guest_arm.h"
#include "../pub/libvex_guest_s390x.h"

#define VG_STRINGIFZ(__str)  #__str
#define VG_STRINGIFY(__str)  VG_STRINGIFZ(__str)

#define my_offsetof(__type,__field) (&((__type*)0)->__field)

/* This forces gcc to evaluate the my_offsetof call at compile time,
   and then emits it in the assembly, along with the nonsense string
   "xyzzy", for easy greppability.  Once this file is compiled to
   assembly, the lines containing "xyzzy" are grepped out and sed-ed
   to produce the final result.  See the Makefile rule for
   pub/libvex_guest_offsets.h. */
#define GENOFFSET(_structUppercase,_structLowercase,_fieldname)  \
   __asm__ __volatile__ ( \
      "\n#define OFFSET_" \
      VG_STRINGIFY(_structLowercase) "_" \
      VG_STRINGIFY(_fieldname) \
      " xyzzy%0\n" : /*out*/ \
                   : /*in*/ "n" \
         (my_offsetof(VexGuest##_structUppercase##State, \
          guest_##_fieldname)) \
   )

void foo ( void );
__attribute__((noinline))
void foo ( void )
{
   // x86
   GENOFFSET(X86,x86,EAX);
   GENOFFSET(X86,x86,EBX);
   GENOFFSET(X86,x86,ECX);
   GENOFFSET(X86,x86,EDX);
   GENOFFSET(X86,x86,ESI);
   GENOFFSET(X86,x86,EDI);
   GENOFFSET(X86,x86,EBP);
   GENOFFSET(X86,x86,ESP);
   GENOFFSET(X86,x86,EIP);
   GENOFFSET(X86,x86,CS);
   GENOFFSET(X86,x86,DS);
   GENOFFSET(X86,x86,ES);
   GENOFFSET(X86,x86,FS);
   GENOFFSET(X86,x86,GS);
   GENOFFSET(X86,x86,SS);

   // amd64
   GENOFFSET(AMD64,amd64,RAX);
   GENOFFSET(AMD64,amd64,RBX);
   GENOFFSET(AMD64,amd64,RCX);
   GENOFFSET(AMD64,amd64,RDX);
   GENOFFSET(AMD64,amd64,RSI);
   GENOFFSET(AMD64,amd64,RDI);
   GENOFFSET(AMD64,amd64,RSP);
   GENOFFSET(AMD64,amd64,RBP);
   GENOFFSET(AMD64,amd64,R8);
   GENOFFSET(AMD64,amd64,R9);
   GENOFFSET(AMD64,amd64,R10);
   GENOFFSET(AMD64,amd64,R11);
   GENOFFSET(AMD64,amd64,R12);
   GENOFFSET(AMD64,amd64,R13);
   GENOFFSET(AMD64,amd64,R14);
   GENOFFSET(AMD64,amd64,R15);
   GENOFFSET(AMD64,amd64,RIP);

   // ppc32
   GENOFFSET(PPC32,ppc32,GPR0);
   GENOFFSET(PPC32,ppc32,GPR1);
   GENOFFSET(PPC32,ppc32,GPR2);
   GENOFFSET(PPC32,ppc32,GPR3);
   GENOFFSET(PPC32,ppc32,GPR4);
   GENOFFSET(PPC32,ppc32,GPR5);
   GENOFFSET(PPC32,ppc32,GPR6);
   GENOFFSET(PPC32,ppc32,GPR7);
   GENOFFSET(PPC32,ppc32,GPR8);
   GENOFFSET(PPC32,ppc32,GPR9);
   GENOFFSET(PPC32,ppc32,GPR10);
   GENOFFSET(PPC32,ppc32,CIA);
   GENOFFSET(PPC32,ppc32,CR0_0);

   // ppc64
   GENOFFSET(PPC64,ppc64,GPR0);
   GENOFFSET(PPC64,ppc64,GPR1);
   GENOFFSET(PPC64,ppc64,GPR2);
   GENOFFSET(PPC64,ppc64,GPR3);
   GENOFFSET(PPC64,ppc64,GPR4);
   GENOFFSET(PPC64,ppc64,GPR5);
   GENOFFSET(PPC64,ppc64,GPR6);
   GENOFFSET(PPC64,ppc64,GPR7);
   GENOFFSET(PPC64,ppc64,GPR8);
   GENOFFSET(PPC64,ppc64,GPR9);
   GENOFFSET(PPC64,ppc64,GPR10);
   GENOFFSET(PPC64,ppc64,CIA);
   GENOFFSET(PPC64,ppc64,CR0_0);

   // arm
   GENOFFSET(ARM,arm,R0);
   GENOFFSET(ARM,arm,R1);
   GENOFFSET(ARM,arm,R2);
   GENOFFSET(ARM,arm,R3);
   GENOFFSET(ARM,arm,R4);
   GENOFFSET(ARM,arm,R5);
   GENOFFSET(ARM,arm,R7);
   GENOFFSET(ARM,arm,R13);
   GENOFFSET(ARM,arm,R14);
   GENOFFSET(ARM,arm,R15T);

   // s390x
   GENOFFSET(S390X,s390x,r2);
   GENOFFSET(S390X,s390x,r3);
   GENOFFSET(S390X,s390x,r4);
   GENOFFSET(S390X,s390x,r5);
   GENOFFSET(S390X,s390x,r6);
   GENOFFSET(S390X,s390x,r7);
   GENOFFSET(S390X,s390x,r15);
   GENOFFSET(S390X,s390x,IA);
   GENOFFSET(S390X,s390x,SYSNO);
   GENOFFSET(S390X,s390x,IP_AT_SYSCALL);
   GENOFFSET(S390X,s390x,fpc);
}

/*--------------------------------------------------------------------*/
/*--- end                                             genoffsets.c ---*/
/*--------------------------------------------------------------------*/
