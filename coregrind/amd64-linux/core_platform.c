#include "core.h"

/* Rerouted entry point for __NR_gettimeofday */
static void amd64_linux_rerouted__vgettimeofday(void)
{
asm(
"       movq    $96, %rax\n"
"       syscall\n"
);
}

/* Rerouted entry point for __NR_time */
static void amd64_linux_rerouted__vtime(void)
{
asm(
"       movq    $201, %rax\n"
"       syscall\n"
);
}

void VGP_(setup_redirects)(void)
{
   /* Redirect vsyscalls to local versions */
   VG_(add_redirect_addr_to_addr)(0xFFFFFFFFFF600000ULL,
                                  (Addr)amd64_linux_rerouted__vgettimeofday);
   VG_(add_redirect_addr_to_addr)(0xFFFFFFFFFF600400ULL,
                                  (Addr)amd64_linux_rerouted__vtime);
}
