
/*--------------------------------------------------------------------*/
/*--- Machine-related stuff.                    pub_tool_machine.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
      jseward@acm.org

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

#ifndef __PUB_TOOL_MACHINE_H
#define __PUB_TOOL_MACHINE_H

/* VG_STACK_REDZONE_SZB: how many bytes below the stack pointer are validly
 * addressible? */
#if defined(VGA_x86)
#  define VG_MIN_INSTR_SZB          1
#  define VG_MAX_INSTR_SZB         16
#  define VG_STACK_REDZONE_SZB      0
#elif defined(VGA_amd64)
#  define VG_MIN_INSTR_SZB          1
#  define VG_MAX_INSTR_SZB         16
#  define VG_STACK_REDZONE_SZB    128
#elif defined(VGA_arm)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_STACK_REDZONE_SZB      0
#elif defined(VGA_ppc32)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_STACK_REDZONE_SZB      0
#else
#  error Unknown arch
#endif

// Guest state accessors
extern Addr VG_(get_SP) ( ThreadId tid );
extern Addr VG_(get_IP) ( ThreadId tid );
extern Addr VG_(get_FP) ( ThreadId tid );

extern void VG_(set_SP) ( ThreadId tid, Addr sp );
extern void VG_(set_IP) ( ThreadId tid, Addr ip );

// For get/set, 'area' is where the asked-for shadow state will be copied
// into/from.
extern void VG_(get_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, UChar* area );
extern void VG_(set_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, const UChar* area );

// Apply a function 'f' to all the general purpose registers in all the
// current threads.
// This is very Memcheck-specific -- it's used to find the roots when
// doing leak checking.
extern void VG_(apply_to_GP_regs)(void (*f)(UWord val));

// Searches through all thread stacks to see if any match.  Returns
// VG_INVALID_THREADID if none match.
extern ThreadId VG_(first_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max, void* d ),
                          void* d );

#endif   // __PUB_TOOL_MACHINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
