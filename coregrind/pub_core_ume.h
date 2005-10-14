
/*--------------------------------------------------------------------*/
/*--- User-mode execve.                             pub_core_ume.h ---*/
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

#ifndef __PUB_CORE_UME_H
#define __PUB_CORE_UME_H

//--------------------------------------------------------------------
// PURPOSE: This module implements user-mode execve, ie. program loading
// and exec'ing.  It is shared between stage1 and stage2.
//--------------------------------------------------------------------

#include <elf.h>
#include <sys/types.h>

/*------------------------------------------------------------*/
/*--- General stuff                                        ---*/
/*------------------------------------------------------------*/

/* This is only here so it can be shared between stage1 and stage2 */

/* JRS 9 Aug 05: both of these are apparently unused, except by
   memcheck/tests/vgtest_ume.c. */
//zz extern
//zz void VG_(foreach_map)(int (*fn)(char *start, char *end,
//zz 			        const char *perm, off_t offset,
//zz 			        int maj, int min, int ino, void* extra),
//zz                       void* extra);
//zz 
//zz /* Jump to 'dst', but first set the stack pointer to 'stack'.  Also,
//zz    clear all the integer registers before entering 'dst'.  It's
//zz    important that the stack pointer is set to exactly 'stack' and not
//zz    (eg) stack - apparently_harmless_looking_small_offset.  Basically
//zz    because the code at 'dst' might be wanting to scan the area above
//zz    'stack' (viz, the auxv array), and putting spurious words on the
//zz    stack confuses it.
//zz 
//zz    This is only exported so that vgtest_ume.c can use it.
//zz */
//zz extern
//zz __attribute__((noreturn))
//zz void VG_(jump_and_switch_stacks) ( Addr stack, Addr dst );


/*------------------------------------------------------------*/
/*--- Loading ELF files                                    ---*/
/*------------------------------------------------------------*/

// Info needed to load and run a program.  IN/INOUT/OUT refers to the
// inputs/outputs of do_exec().
struct exeinfo
{
   char** argv;         // IN: the original argv

   Addr exe_base;       // INOUT: lowest (allowed) address of exe
   Addr exe_end;        // INOUT: highest (allowed) address

   Addr phdr;           // OUT: address phdr was mapped at
   int  phnum;          // OUT: number of phdrs
   Addr interp_base;    // OUT: where interpreter (ld.so) was mapped
   Addr entry;          // OUT: entrypoint in main executable
   Addr init_eip;       // OUT: initial eip
   Addr brkbase;        // OUT: base address of brk segment

   // These are the extra args added by #! scripts
   char*  interp_name;  // OUT: the interpreter name
   char*  interp_args;  // OUT: the args for the interpreter
};

// Do a number of appropriate checks to see if the file looks executable by
// the kernel: ie. it's a file, it's readable and executable, and it's in
// either ELF or "#!" format.  On success, 'out_fd' gets the fd of the file
// if it's non-NULL.  Otherwise the fd is closed.
extern SysRes VG_(pre_exec_check)(const Char* exe_name, Int* out_fd);

// Does everything short of actually running 'exe': finds the file,
// checks execute permissions, sets up interpreter if program is a script, 
// reads headers, maps file into memory, and returns important info about
// the program.
extern Int VG_(do_exec)(const char *exe, struct exeinfo *info);

/*------------------------------------------------------------*/
/*--- Finding and dealing with auxv                        ---*/
/*------------------------------------------------------------*/

struct ume_auxv
{
   Word a_type;
   union {
      void *a_ptr;
      Word a_val;
   } u;
};

extern struct ume_auxv *VG_(find_auxv)(UWord* orig_esp);

#endif /* __PUB_CORE_UME_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
