
/*--------------------------------------------------------------------*/
/*--- A header file used by both stage1 and stage2.                ---*/
/*---                                                        ume.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#ifndef _COREGRIND_UME_H
#define _COREGRIND_UME_H

#include <elf.h>
#include <sys/types.h>

/*------------------------------------------------------------*/
/*--- General stuff                                        ---*/
/*------------------------------------------------------------*/

void foreach_map(int (*fn)(char *start, char *end,
			   const char *perm, off_t offset,
			   int maj, int min, int ino, void* extra),
                 void* extra);

/*------------------------------------------------------------*/
/*--- Loading ELF files                                    ---*/
/*------------------------------------------------------------*/

#if	ELFSZ == 64
#define ESZ(x)	Elf64_##x
#elif	ELFSZ == 32
#define ESZ(x)	Elf32_##x
#else
#error ELFSZ needs to ==32 or ==64
#endif

/* Integer type the same size as a pointer */
typedef ESZ(Addr) addr_t;

// Info needed to load and run a program.  IN/INOUT/OUT refers to the
// inputs/outputs of do_exec().
struct exeinfo
{
   addr_t map_base;     // IN: if non-zero, base address of mappings
   char** argv;         // IN: the original argv

   addr_t exe_base;     // INOUT: lowest (allowed) address of exe
   addr_t exe_end;      // INOUT: highest (allowed) address

   addr_t phdr;         // OUT: address phdr was mapped at
   int    phnum;        // OUT: number of phdrs
   addr_t interp_base;  // OUT: where interpreter (ld.so) was mapped
   addr_t entry;        // OUT: entrypoint in main executable
   addr_t init_eip;     // OUT: initial eip
   addr_t brkbase;      // OUT: base address of brk segment

   // These are the extra args added by #! scripts
   char*  interp_name;  // OUT: the interpreter name
   char*  interp_args;  // OUT: the args for the interpreter
};

// Does everything short of actually running 'exe': finds the file,
// checks execute permissions, sets up interpreter if program is a script, 
// reads headers, maps file into memory, and returns important info about
// the program.
int do_exec(const char *exe, struct exeinfo *info);

/*------------------------------------------------------------*/
/*--- Finding and dealing with auxv                        ---*/
/*------------------------------------------------------------*/

struct ume_auxv
{
   int	a_type;
   union {
      void *a_ptr;
      int   a_val;
      void (*a_fcn)(void);
   } u;
};

struct ume_auxv *find_auxv(int *orig_esp);

/* Our private auxv entries */
#define AT_UME_PADFD	0xff01	/* padding file fd */
#define AT_UME_EXECFD	0xff02	/* stage1 executable fd */

#endif /* _COREGRIND_UME_H */

/*--------------------------------------------------------------------*/
/*--- end                                                    ume.h ---*/
/*--------------------------------------------------------------------*/
