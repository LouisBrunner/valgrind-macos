
#include <stdio.h>

/* A program which generates various guest state offsets. */

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"

Int main ( void )
{
  printf("#define OFFSET_x86_EAX %d\n", 
         offsetof(VexGuestX86State,guest_EAX));

  printf("#define OFFSET_x86_EBX %d\n", 
         offsetof(VexGuestX86State,guest_EBX));

  printf("#define OFFSET_x86_ECX %d\n", 
         offsetof(VexGuestX86State,guest_ECX));

  printf("#define OFFSET_x86_EDX %d\n", 
         offsetof(VexGuestX86State,guest_EDX));

  printf("#define OFFSET_x86_ESI %d\n", 
         offsetof(VexGuestX86State,guest_ESI));

  printf("#define OFFSET_x86_EDI %d\n", 
         offsetof(VexGuestX86State,guest_EDI));

  printf("#define OFFSET_x86_EBP %d\n", 
         offsetof(VexGuestX86State,guest_EBP));

  printf("#define OFFSET_x86_ESP %d\n", 
         offsetof(VexGuestX86State,guest_ESP));

  printf("#define OFFSET_x86_EIP %d\n", 
         offsetof(VexGuestX86State,guest_EIP));


  printf("#define OFFSET_amd64_RAX %d\n", 
         offsetof(VexGuestAMD64State,guest_RAX));

  printf("#define OFFSET_amd64_RDI %d\n", 
         offsetof(VexGuestAMD64State,guest_RDI));

  printf("#define OFFSET_amd64_RSI %d\n", 
         offsetof(VexGuestAMD64State,guest_RSI));

  printf("#define OFFSET_amd64_RDX %d\n", 
         offsetof(VexGuestAMD64State,guest_RDX));

  printf("#define OFFSET_amd64_R8 %d\n", 
         offsetof(VexGuestAMD64State,guest_R8));

  printf("#define OFFSET_amd64_R9 %d\n", 
         offsetof(VexGuestAMD64State,guest_R9));

  printf("#define OFFSET_amd64_R10 %d\n", 
         offsetof(VexGuestAMD64State,guest_R10));

  printf("#define OFFSET_amd64_RIP %d\n", 
         offsetof(VexGuestAMD64State,guest_RIP));

  return 0;
}
