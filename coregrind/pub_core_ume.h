
/*--------------------------------------------------------------------*/
/*--- User-mode execve.                             pub_core_ume.h ---*/
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

#ifndef __PUB_CORE_UME_H
#define __PUB_CORE_UME_H

#include "pub_core_basics.h"   // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module implements user-mode execve, ie. program loading
// and exec'ing.
//--------------------------------------------------------------------

/*------------------------------------------------------------*/
/*--- Loading files                                        ---*/
/*------------------------------------------------------------*/

// Info needed to load and run a program.  IN/INOUT/OUT refers to the
// inputs/outputs of do_exec().
typedef
   struct {
      const HChar** argv;   // IN: the original argv

      Addr exe_base;     // INOUT: lowest (allowed) address of exe
      Addr exe_end;      // INOUT: highest (allowed) address

#if !defined(VGO_darwin)
      Addr     phdr;          // OUT: address phdr was mapped at
      Int      phnum;         // OUT: number of phdrs
      UInt     stack_prot;    // OUT: stack permissions
      PtrdiffT interp_offset; // OUT: relocation offset for ld.so
#else
      Addr  stack_start;      // OUT: address of start of stack segment (hot)
      Addr  stack_end;        // OUT: address of end of stack segment (cold)
      Addr  text;             // OUT: address of executable's Mach header
      Bool  dynamic;          // OUT: False iff executable is static
      char* executable_path;  // OUT: path passed to execve()
#endif

      Addr entry;        // OUT: entrypoint in main executable
      Addr init_ip;      // OUT: address of first instruction to execute
      Addr brkbase;      // OUT: base address of brk segment
      Addr init_toc;     // OUT: address of table-of-contents, on
                         // platforms for which that makes sense
                         // (ppc64-linux only)

      // These are the extra args added by #! scripts
      HChar*  interp_name;  // OUT: the interpreter name
      HChar*  interp_args;  // OUT: the args for the interpreter
   }
   ExeInfo;

// Do a number of appropriate checks to see if the file looks executable by
// the kernel: ie. it's a file, it's readable and executable, and it's in
// either binary or "#!" format.  On success, 'out_fd' gets the fd of the file
// if it's non-NULL.  Otherwise the fd is closed.
extern SysRes VG_(pre_exec_check)(const HChar* exe_name, Int* out_fd,
                                  Bool allow_setuid);

// Does everything short of actually running 'exe': finds the file,
// checks execute permissions, sets up interpreter if program is a script, 
// reads headers, maps file into memory, and returns important info about
// the program.
extern Int VG_(do_exec)(const HChar* exe, ExeInfo* info);

#endif /* __PUB_CORE_UME_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
