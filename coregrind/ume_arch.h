#ifndef UME_ARCH
#define UME_ARCH

#include "ume.h"

void ume_go(addr_t eip, addr_t esp) __attribute__((noreturn));

extern void *ume_exec_esp;	/* esp on entry at exec time */

#endif /* UME_ARCH */
