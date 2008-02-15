
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (auxprogs/genoffsets.c) is                         ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

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

/* A program which generates various guest state offsets. */

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"
#include "../pub/libvex_guest_ppc32.h"
#include "../pub/libvex_guest_ppc64.h"

Int main ( void )
{
  // x86
  printf("#define OFFSET_x86_EAX %3d\n", 
         offsetof(VexGuestX86State,guest_EAX));

  printf("#define OFFSET_x86_EBX %3d\n", 
         offsetof(VexGuestX86State,guest_EBX));

  printf("#define OFFSET_x86_ECX %3d\n", 
         offsetof(VexGuestX86State,guest_ECX));

  printf("#define OFFSET_x86_EDX %3d\n", 
         offsetof(VexGuestX86State,guest_EDX));

  printf("#define OFFSET_x86_ESI %3d\n", 
         offsetof(VexGuestX86State,guest_ESI));

  printf("#define OFFSET_x86_EDI %3d\n", 
         offsetof(VexGuestX86State,guest_EDI));

  printf("#define OFFSET_x86_EBP %3d\n", 
         offsetof(VexGuestX86State,guest_EBP));

  printf("#define OFFSET_x86_ESP %3d\n", 
         offsetof(VexGuestX86State,guest_ESP));

  printf("#define OFFSET_x86_EIP %3d\n", 
         offsetof(VexGuestX86State,guest_EIP));

  printf("#define OFFSET_x86_CS %3d\n",
         offsetof(VexGuestX86State,guest_CS));

  printf("#define OFFSET_x86_DS %3d\n",
         offsetof(VexGuestX86State,guest_DS));

  printf("#define OFFSET_x86_ES %3d\n",
         offsetof(VexGuestX86State,guest_ES));

  printf("#define OFFSET_x86_FS %3d\n",
         offsetof(VexGuestX86State,guest_FS));

  printf("#define OFFSET_x86_GS %3d\n",
         offsetof(VexGuestX86State,guest_GS));

  printf("#define OFFSET_x86_SS %3d\n",
         offsetof(VexGuestX86State,guest_SS));

  printf("\n");

  // amd64
  printf("#define OFFSET_amd64_RAX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RAX));

  printf("#define OFFSET_amd64_RBX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RBX));

  printf("#define OFFSET_amd64_RCX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RCX));

  printf("#define OFFSET_amd64_RDX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RDX));

  printf("#define OFFSET_amd64_RSI %3d\n", 
         offsetof(VexGuestAMD64State,guest_RSI));

  printf("#define OFFSET_amd64_RDI %3d\n", 
         offsetof(VexGuestAMD64State,guest_RDI));

  printf("#define OFFSET_amd64_RSP %3d\n", 
         offsetof(VexGuestAMD64State,guest_RSP));

  printf("#define OFFSET_amd64_RBP %3d\n", 
         offsetof(VexGuestAMD64State,guest_RBP));

  printf("#define OFFSET_amd64_R8  %3d\n", 
         offsetof(VexGuestAMD64State,guest_R8));

  printf("#define OFFSET_amd64_R9  %3d\n", 
         offsetof(VexGuestAMD64State,guest_R9));

  printf("#define OFFSET_amd64_R10 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R10));

  printf("#define OFFSET_amd64_R11 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R11));

  printf("#define OFFSET_amd64_R12 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R12));

  printf("#define OFFSET_amd64_R13 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R13));

  printf("#define OFFSET_amd64_R14 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R14));

  printf("#define OFFSET_amd64_R15 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R15));

  printf("#define OFFSET_amd64_RIP %3d\n", 
         offsetof(VexGuestAMD64State,guest_RIP));

  printf("\n");

  // ppc32
  printf("#define OFFSET_ppc32_GPR0      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR0));

  printf("#define OFFSET_ppc32_GPR2      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR2));

  printf("#define OFFSET_ppc32_GPR3      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR3));

  printf("#define OFFSET_ppc32_GPR4      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR4));

  printf("#define OFFSET_ppc32_GPR5      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR5));

  printf("#define OFFSET_ppc32_GPR6      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR6));

  printf("#define OFFSET_ppc32_GPR7      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR7));

  printf("#define OFFSET_ppc32_GPR8      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR8));

  printf("#define OFFSET_ppc32_GPR9      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR9));

  printf("#define OFFSET_ppc32_GPR10     %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR10));

  printf("#define OFFSET_ppc32_CIA       %3d\n",
         offsetof(VexGuestPPC32State,guest_CIA));

  printf("#define OFFSET_ppc32_CR0_0     %3d\n",
         offsetof(VexGuestPPC32State,guest_CR0_0));

  printf("\n");

  // ppc64
  printf("#define OFFSET_ppc64_GPR0     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR0));

  printf("#define OFFSET_ppc64_GPR2     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR2));

  printf("#define OFFSET_ppc64_GPR3     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR3));

  printf("#define OFFSET_ppc64_GPR4     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR4));

  printf("#define OFFSET_ppc64_GPR5     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR5));

  printf("#define OFFSET_ppc64_GPR6     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR6));

  printf("#define OFFSET_ppc64_GPR7     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR7));

  printf("#define OFFSET_ppc64_GPR8     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR8));

  printf("#define OFFSET_ppc64_GPR9     %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR9));

  printf("#define OFFSET_ppc64_GPR10    %4d\n",
         offsetof(VexGuestPPC64State,guest_GPR10));

  printf("#define OFFSET_ppc64_CIA      %4d\n",
         offsetof(VexGuestPPC64State,guest_CIA));

  printf("#define OFFSET_ppc64_CR0_0    %4d\n",
         offsetof(VexGuestPPC64State,guest_CR0_0));

  printf("\n");

  return 0;
}
