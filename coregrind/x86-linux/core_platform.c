#include "core.h"

void VGP_(setup_redirects)(void)
{
   /* Redirect _dl_sysinfo_int80, which is glibc's default system call
      routine, to the routine in our trampoline page so that the
      special sysinfo unwind hack in vg_execontext.c will kick in.
   */
   VG_(add_redirect_sym_to_addr)("soname:ld-linux.so.2", "_dl_sysinfo_int80",
                                 VG_(client_trampoline_code)+VG_(tramp_syscall_offset));
}
