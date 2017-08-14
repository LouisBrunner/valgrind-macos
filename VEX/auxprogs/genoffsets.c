
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (auxprogs/genoffsets.c) is                         ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2009 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

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
}
