
#include <stdio.h>

/* A program which generates various guest state offsets. */

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"
#include "../pub/libvex_guest_ppc32.h"

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
  printf("\n");

  // amd64
  printf("#define OFFSET_amd64_RAX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RAX));

  printf("#define OFFSET_amd64_RDI %3d\n", 
         offsetof(VexGuestAMD64State,guest_RDI));

  printf("#define OFFSET_amd64_RSI %3d\n", 
         offsetof(VexGuestAMD64State,guest_RSI));

  printf("#define OFFSET_amd64_RDX %3d\n", 
         offsetof(VexGuestAMD64State,guest_RDX));

  printf("#define OFFSET_amd64_R8  %3d\n", 
         offsetof(VexGuestAMD64State,guest_R8));

  printf("#define OFFSET_amd64_R9  %3d\n", 
         offsetof(VexGuestAMD64State,guest_R9));

  printf("#define OFFSET_amd64_R10 %3d\n", 
         offsetof(VexGuestAMD64State,guest_R10));

  printf("#define OFFSET_amd64_RIP %3d\n", 
         offsetof(VexGuestAMD64State,guest_RIP));

  printf("\n");

  // ppc32
  printf("#define OFFSET_ppc32_GPR0      %3d\n",
         offsetof(VexGuestPPC32State,guest_GPR0));

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

  printf("#define OFFSET_ppc32_CIA       %3d\n",
         offsetof(VexGuestPPC32State,guest_CIA));

  printf("#define OFFSET_ppc32_CR0_0     %3d\n",
         offsetof(VexGuestPPC32State,guest_CR0_0));

  printf("\n");

  return 0;
}
