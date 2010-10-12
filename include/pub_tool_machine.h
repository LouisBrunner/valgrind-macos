
/*--------------------------------------------------------------------*/
/*--- Machine-related stuff.                    pub_tool_machine.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
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

#if defined(VGP_x86_linux)
#  define VG_MIN_INSTR_SZB          1  // min length of native instruction
#  define VG_MAX_INSTR_SZB         16  // max length of native instruction
#  define VG_CLREQ_SZB             14  // length of a client request, may
                                       //   be larger than VG_MAX_INSTR_SZB
#  define VG_STACK_REDZONE_SZB      0  // number of addressable bytes below %RSP

#elif defined(VGP_amd64_linux)
#  define VG_MIN_INSTR_SZB          1
#  define VG_MAX_INSTR_SZB         16
#  define VG_CLREQ_SZB             19
#  define VG_STACK_REDZONE_SZB    128

#elif defined(VGP_ppc32_linux)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_CLREQ_SZB             20
#  define VG_STACK_REDZONE_SZB      0

#elif defined(VGP_ppc64_linux)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_CLREQ_SZB             20
#  define VG_STACK_REDZONE_SZB    288  // number of addressable bytes below R1
                                       // from 64-bit PowerPC ELF ABI 
                                       // Supplement 1.7

#elif defined(VGP_arm_linux)
#  define VG_MIN_INSTR_SZB          2
#  define VG_MAX_INSTR_SZB          4 
#  define VG_CLREQ_SZB             20
#  define VG_STACK_REDZONE_SZB      0

#elif defined(VGP_ppc32_aix5)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_CLREQ_SZB             20
   /* The PowerOpen ABI actually says 220 bytes, but that is not an
      8-aligned number, and frequently forces Memcheck's
      mc_{new,die}_mem_stack_N routines into slow cases by losing
      8-alignment of the area to be messed with.  So let's just say
      224 instead.  Gdb has a similar kludge. */
#  define VG_STACK_REDZONE_SZB    224

#elif defined(VGP_ppc64_aix5)
#  define VG_MIN_INSTR_SZB          4
#  define VG_MAX_INSTR_SZB          4 
#  define VG_CLREQ_SZB             20
#  define VG_STACK_REDZONE_SZB    288 // is this right?

#elif defined(VGP_x86_darwin)
#  define VG_MIN_INSTR_SZB          1  // min length of native instruction
#  define VG_MAX_INSTR_SZB         16  // max length of native instruction
#  define VG_CLREQ_SZB             14  // length of a client request, may
                                       //   be larger than VG_MAX_INSTR_SZB
#  define VG_STACK_REDZONE_SZB      0  // number of addressable bytes below %RSP

#elif defined(VGP_amd64_darwin)
#  define VG_MIN_INSTR_SZB          1
#  define VG_MAX_INSTR_SZB         16
#  define VG_CLREQ_SZB             19
#  define VG_STACK_REDZONE_SZB    128

#else
#  error Unknown platform
#endif

// Guest state accessors
// Are mostly in the core_ header.
//  Only these two are available to tools.
Addr VG_(get_IP) ( ThreadId tid );
Addr VG_(get_SP) ( ThreadId tid );


// For get/set, 'area' is where the asked-for guest state will be copied
// into/from.  If shadowNo == 0, the real (non-shadow) guest state is
// accessed.  If shadowNo == 1, the first shadow area is accessed, and
// if shadowNo == 2, the second shadow area is accessed.  This gives a
// completely general way to read/modify a thread's guest register state
// providing you know the offsets you need.
void
VG_(get_shadow_regs_area) ( ThreadId tid, 
                            /*DST*/UChar* dst,
                            /*SRC*/Int shadowNo, PtrdiffT offset, SizeT size );
void
VG_(set_shadow_regs_area) ( ThreadId tid, 
                            /*DST*/Int shadowNo, PtrdiffT offset, SizeT size,
                            /*SRC*/const UChar* src );

// Sets the shadow values for the syscall return value register(s).
// This is platform specific.
void VG_(set_syscall_return_shadows) ( ThreadId tid,
                                       /* shadow vals for the result */
                                       UWord s1res, UWord s2res,
                                       /* shadow vals for the error val */
                                       UWord s1err, UWord s2err );

// Apply a function 'f' to all the general purpose registers in all the
// current threads.
// This is very Memcheck-specific -- it's used to find the roots when
// doing leak checking.
extern void VG_(apply_to_GP_regs)(void (*f)(UWord val));

// This iterator lets you inspect each live thread's stack bounds.
// Returns False at the end.  'tid' is the iterator and you can only
// safely change it by making calls to these functions.
extern void VG_(thread_stack_reset_iter) ( /*OUT*/ThreadId* tid );
extern Bool VG_(thread_stack_next)       ( /*MOD*/ThreadId* tid,
                                           /*OUT*/Addr* stack_min, 
                                           /*OUT*/Addr* stack_max );

// Returns .client_stack_highest_word for the given thread
extern Addr VG_(thread_get_stack_max) ( ThreadId tid );

// Returns how many bytes have been allocated for the stack of the given thread
extern SizeT VG_(thread_get_stack_size) ( ThreadId tid );

// Returns the bottommost address of the alternate signal stack.
// See also the man page of sigaltstack().
extern Addr VG_(thread_get_altstack_min) ( ThreadId tid );

// Returns how many bytes have been allocated for the alternate signal stack.
// See also the man page of sigaltstack().
extern SizeT VG_(thread_get_altstack_size) ( ThreadId tid );

// Given a pointer to a function as obtained by "& functionname" in C,
// produce a pointer to the actual entry point for the function.  For
// most platforms it's the identity function.  Unfortunately, on
// ppc64-linux it isn't (sigh).
extern void* VG_(fnptr_to_fnentry)( void* );

#endif   // __PUB_TOOL_MACHINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
