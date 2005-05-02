#include "core.h"

void VGP_(setup_redirects)(void)
{
   /* Redirect vsyscalls to local versions */
   VG_(add_redirect_addr_to_addr)(0xFFFFFFFFFF600000ULL,
                                  VG_(client_trampoline_code)+VG_(tramp_gettimeofday_offset));
   VG_(add_redirect_addr_to_addr)(0xFFFFFFFFFF600400ULL,
                                  VG_(client_trampoline_code)+VG_(tramp_time_offset));
}
