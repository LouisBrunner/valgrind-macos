
#include <stdio.h>

/* A program which generates various guest state offsets. */

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"

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

  return 0;
}
