
/*--------------------------------------------------------------------*/
/*--- Debug info.                             pub_core_debuginfo.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward
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

#ifndef __PUB_CORE_DEBUGINFO_H
#define __PUB_CORE_DEBUGINFO_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with reading debug info and symbol tables
// to get file and function names, line numbers, variable types, and
// to help stack unwinding.
//--------------------------------------------------------------------

#include "pub_tool_debuginfo.h"

/* Initialise the entire module.  Must be called first of all. */
extern void VG_(di_initialise) ( void );

/* LINUX: Notify the debuginfo system about a new mapping, or the
   disappearance of such, or a permissions change on an existing
   mapping.  This is the way new debug information gets loaded.  If
   allow_SkFileV is True, it will try load debug info if the mapping
   at 'a' belongs to Valgrind; whereas normally (False) it will not do
   that.  This allows us to carefully control when the thing will read
   symbols from the Valgrind executable itself.

   If a call to VG_(di_notify_mmap) causes debug info to be read, then
   the returned ULong is an abstract handle which can later be used to
   refer to the debuginfo read as a result of this specific mapping,
   in later queries to m_debuginfo.  In this case the handle value
   will be one or above.  If the returned value is zero, no debug info
   was read.

   For VG_(di_notify_mmap), if use_fd is not -1, that is used instead
   of the filename; this avoids perturbing fcntl locks, which are
   released by simply re-opening and closing the same file (even via
   different fd!).
*/
#if defined(VGO_linux) || defined(VGO_darwin)
extern ULong VG_(di_notify_mmap)( Addr a, Bool allow_SkFileV, Int use_fd );

extern void VG_(di_notify_munmap)( Addr a, SizeT len );

extern void VG_(di_notify_mprotect)( Addr a, SizeT len, UInt prot );

/* this should really return ULong, as per VG_(di_notify_mmap). */
extern void VG_(di_notify_pdb_debuginfo)( Int fd, Addr avma,
                                          SizeT total_size,
                                          PtrdiffT bias );

/* this should also really return ULong */
extern void VG_(di_notify_vm_protect)( Addr a, SizeT len, UInt prot );
#endif

extern void VG_(di_discard_ALL_debuginfo)( void );

/* Like VG_(get_fnname), but it does not do C++ demangling nor Z-demangling
 * nor below-main renaming.
 * It should not be used for any names that will be shown to users.
 * It should only be used in cases where the names of interest will have
 * particular (ie. non-mangled) forms, or the mangled form is acceptable. */
extern
Bool VG_(get_fnname_raw) ( Addr a, Char* buf, Int nbuf );

/* Like VG_(get_fnname), but without C++ demangling.  (But it does
 * Z-demangling and below-main renaming.) */
extern
Bool VG_(get_fnname_no_cxx_demangle) ( Addr a, Char* buf, Int nbuf );

/* mips-linux only: find the offset of current address. This is needed for 
   stack unwinding for MIPS.
*/
extern
Bool VG_(get_inst_offset_in_function)( Addr a, /*OUT*/PtrdiffT* offset );


/* Use DWARF2/3 CFA information to do one step of stack unwinding.
   D3UnwindRegs holds the current register values, and is
   arch-specific.  Note that the x86 and amd64 definitions are shared
   and so the regs are named 'xip' etc rather than 'eip' and 'rip'. */
#if defined(VGA_amd64) || defined(VGA_x86)
typedef
   struct { Addr xip; Addr xsp; Addr xbp; }
   D3UnwindRegs;
#elif defined(VGA_arm)
typedef
   struct { Addr r15; Addr r14; Addr r13; Addr r12; Addr r11; Addr r7; }
   D3UnwindRegs;
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
typedef
   UChar  /* should be void, but gcc complains at use points */
   D3UnwindRegs;
#elif defined(VGA_s390x)
typedef
   struct { Addr ia; Addr sp; Addr fp; Addr lr;}
   D3UnwindRegs;
#elif defined(VGA_mips32)
typedef
   struct { Addr pc; Addr sp; Addr fp; Addr ra; }
   D3UnwindRegs;
#else
#  error "Unsupported arch"
#endif

extern Bool VG_(use_CF_info) ( /*MOD*/D3UnwindRegs* uregs,
                               Addr min_accessible,
                               Addr max_accessible );


/* Use MSVC FPO data to do one step of stack unwinding. */
extern Bool VG_(use_FPO_info) ( /*MOD*/Addr* ipP,
                                /*MOD*/Addr* spP,
                                /*MOD*/Addr* fpP,
                                Addr min_accessible,
                                Addr max_accessible );

/* ppc64-linux only: find the TOC pointer (R2 value) that should be in
   force at the entry point address of the function containing
   guest_code_addr.  Returns 0 if not known. */
extern Addr VG_(get_tocptr) ( Addr guest_code_addr );

/* Map a function name to its entry point and toc pointer.  Is done by
   sequential search of all symbol tables, so is very slow.  To
   mitigate the worst performance effects, you may specify a soname
   pattern, and only objects matching that pattern are searched.
   Therefore specify "*" to search all the objects.  On TOC-afflicted
   platforms, a symbol is deemed to be found only if it has a nonzero
   TOC pointer.  */
extern
Bool VG_(lookup_symbol_SLOW)(UChar* sopatt, UChar* name, Addr* pEnt, Addr* pToc);

#endif   // __PUB_CORE_DEBUGINFO_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
