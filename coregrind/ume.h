
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

#if	ELFSZ == 64
#define ESZ(x)	Elf64_##x
#elif	ELFSZ == 32
#define ESZ(x)	Elf32_##x
#else
#error ELFSZ needs to ==32 or ==64
#endif

/* Integer type the same size as a pointer */
typedef ESZ(Addr) addr_t;

struct exeinfo
{
   int		setbrk;		/* INPUT: if true, set the brk segment base */
   addr_t	map_base;	/* INPUT: if non-zero, base address of mappings  */

   addr_t	exe_base;	/* INOUT: lowest (allowed) address of exe	*/
   addr_t	exe_end;	/* INOUT: highest (allowed) address	*/

   addr_t	phdr;		/* address phdr was mapped at		*/
   int		phnum;		/* number of phdrs			*/
   addr_t	interp_base;	/* where interpreter (ld.so) was mapped	*/
   addr_t	entry;		/* entrypoint in main executable	*/
   addr_t	init_eip;	/* initial eip				*/
   addr_t	brkbase;	/* base address of brk segment		*/

   /* these are the extra args added by #! scripts */
   char		*argv0;		/* INPUT: the interpreter name */
   char		*argv1;		/* INPUT: the args for the interpreter */

   char		**argv;		/* INPUT: the original argv */
};

int do_exec(const char *exe, struct exeinfo *info);

void foreach_map(int (*fn)(void *start, void *end,
			   const char *perm, off_t offset,
			   int maj, int min, int ino));
void as_pad(void *start, void *end);
void as_unpad(void *start, void *end);
void as_closepadfile(void);
int  as_getpadfd(void);
void as_setpadfd(int);

struct elfinfo
{
   ESZ(Ehdr)	e;
   ESZ(Phdr)	*p;
   int		fd;
};

struct elfinfo *readelf(int fd, const char *filename);
ESZ(Addr) mapelf(struct elfinfo *e, ESZ(Addr) base, int setbrk);

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
