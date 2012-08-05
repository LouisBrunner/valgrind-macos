
/*--------------------------------------------------------------------*/
/*--- Basic Mach interface functions                 mach_basics.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2012 Apple Inc.
      Greg Parker  gparker@apple.com

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGO_darwin) 

#include "pub_core_basics.h"
#include "pub_core_mach.h"

#include <mach/mach.h>
#include <mach/machine/ndr_def.h>

/* From mach_traps-<arch>-darwin.S */
//extern mach_port_name_t host_self_trap(void);
extern mach_port_name_t thread_self_trap(void);
extern mach_port_t mach_reply_port(void);

/* Global variables set in mach_init() */
vm_size_t vm_page_size = 0;
mach_port_name_t mach_task_self_ = 0;


mach_port_name_t mach_thread_self(void)
{
    return thread_self_trap();
}

static mach_port_t reply = 0;

mach_port_t mig_get_reply_port(void)
{
    if (!reply) reply = mach_reply_port();
    return reply;
    // GrP fixme is just one enough for valgrind's own use?
    // might work if valgrind never blocks in mig calls on 
    // its own behalf, and doesn't call mig outside the semaphore
}


void mig_dealloc_reply_port(mach_port_t reply_port)
{
}


void mig_put_reply_port(mach_port_t reply_port)
{
}


/* Initialize Mach global data. 
   Should be called early in main(). */
void VG_(mach_init)(void)
{
    reply = 0;
    mach_task_self_ = task_self_trap();

    // GrP fixme host_page_size(host_self_trap(), &vm_page_size);
    vm_page_size = 4096;
}

#endif // defined(VGO_darwin) 

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
