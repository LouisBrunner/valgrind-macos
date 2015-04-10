
/*--------------------------------------------------------------------*/
/*--- Debug info.                             pub_core_debuginfo.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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
Bool VG_(get_fnname_raw) ( Addr a, const HChar** buf );

/* Like VG_(get_fnname), but without C++ demangling.  (But it does
 Z-demangling and below-main renaming.)
 iipc argument: same usage as in VG_(describe_IP) in pub_tool_debuginfo.h. */
extern
Bool VG_(get_fnname_no_cxx_demangle) ( Addr a, const HChar** buf,
                                       const InlIPCursor* iipc );

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
#elif defined(VGA_arm64)
typedef
   struct { Addr pc; Addr sp; Addr x30; Addr x29; } /* PC, SP, LR, FP */
   D3UnwindRegs;
#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
typedef
   UChar  /* should be void, but gcc complains at use points */
   D3UnwindRegs;
#elif defined(VGA_s390x)
typedef
   struct { Addr ia; Addr sp; Addr fp; Addr lr;}
   D3UnwindRegs;
#elif defined(VGA_mips32) || defined(VGA_mips64)
typedef
   struct { Addr pc; Addr sp; Addr fp; Addr ra; }
   D3UnwindRegs;
#elif defined(VGA_tilegx)
typedef
   struct { Addr pc; Addr sp; Addr fp; Addr lr; }
   D3UnwindRegs;
#else
#  error "Unsupported arch"
#endif

extern Bool VG_(use_CF_info) ( /*MOD*/D3UnwindRegs* uregs,
                               Addr min_accessible,
                               Addr max_accessible );

/* returns the "generation" of the CF info.
   Each time some debuginfo is changed (e.g. loaded or unloaded),
   the VG_(CF_info_generation) value returned will be increased.
   This can be used to flush cached information derived from the CF info. */
extern UInt VG_(CF_info_generation) (void);



/* Use MSVC FPO data to do one step of stack unwinding. */
extern Bool VG_(use_FPO_info) ( /*MOD*/Addr* ipP,
                                /*MOD*/Addr* spP,
                                /*MOD*/Addr* fpP,
                                Addr min_accessible,
                                Addr max_accessible );

/* AVMAs for a symbol. Usually only the lowest address of the entity.
   On ppc64 platforms, also contains tocptr and local_ep.
   These fields should only be accessed using the macros
   GET_TOCPTR_AVMA/SET_TOCPTR_AVMA/GET_LOCAL_EP_AVMA/SET_LOCAL_EP_AVMA. */
typedef
   struct {
      Addr main;      /* lowest address of entity */
#     if defined(VGA_ppc64be) || defined(VGA_ppc64le)
      Addr tocptr;    /* ppc64be/le-linux only: value that R2 should have */
#     endif
#     if defined(VGA_ppc64le)
      Addr local_ep;  /* address for local entry point, ppc64le only */
#     endif
   }
   SymAVMAs;

#if defined(VGA_ppc64be) || defined(VGA_ppc64le)
# define GET_TOCPTR_AVMA(_sym_avmas)          (_sym_avmas).tocptr
# define SET_TOCPTR_AVMA(_sym_avmas, _val)    (_sym_avmas).tocptr = (_val)
#else
# define GET_TOCPTR_AVMA(_sym_avmas)          ((Addr)0)
# define SET_TOCPTR_AVMA(_sym_avmas, _val)    /* */
#endif

#if defined(VGA_ppc64le)
# define GET_LOCAL_EP_AVMA(_sym_avmas)        (_sym_avmas).local_ep
# define SET_LOCAL_EP_AVMA(_sym_avmas, _val)  (_sym_avmas).local_ep = (_val)
#else
# define GET_LOCAL_EP_AVMA(_sym_avmas)        ((Addr)0)
# define SET_LOCAL_EP_AVMA(_sym_avmas, _val)  /* */
#endif

/* Functions for traversing all the symbols in a DebugInfo.  _howmany
   tells how many symbol table entries there are.  _getidx retrieves
   the n'th entry, for n in 0 .. _howmany-1.  You may not modify the
   function names thereby acquired; if you want to do so, first strdup
   them.  The primary name is returned in *pri_name, and *sec_names is
   set either to NULL or to a NULL terminated vector containing
   pointers to the secondary names. */
Int  VG_(DebugInfo_syms_howmany) ( const DebugInfo *di );
void VG_(DebugInfo_syms_getidx)  ( const DebugInfo *di, 
                                   Int idx,
                                   /*OUT*/SymAVMAs* ad,
                                   /*OUT*/UInt*     size,
                                   /*OUT*/const HChar**   pri_name,
                                   /*OUT*/const HChar***  sec_names,
                                   /*OUT*/Bool*     isText,
                                   /*OUT*/Bool*     isIFunc );
/* ppc64-linux only: find the TOC pointer (R2 value) that should be in
   force at the entry point address of the function containing
   guest_code_addr.  Returns 0 if not known. */
extern Addr VG_(get_tocptr) ( Addr guest_code_addr );

/* Map a function name to its SymAVMAs.  Is done by
   sequential search of all symbol tables, so is very slow.  To
   mitigate the worst performance effects, you may specify a soname
   pattern, and only objects matching that pattern are searched.
   Therefore specify "*" to search all the objects.  On TOC-afflicted
   platforms, a symbol is deemed to be found only if it has a nonzero
   TOC pointer.  */
extern
Bool VG_(lookup_symbol_SLOW)(const HChar* sopatt, const HChar* name,
                             SymAVMAs* avmas);

#endif   // __PUB_CORE_DEBUGINFO_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
