
/*--------------------------------------------------------------------*/
/*--- Startup: preliminaries                              stage1.c ---*/
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

#define _FILE_OFFSET_BITS	64

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <unistd.h>

#include "core.h"
#include "ume.h"

static int stack[SIGSTKSZ*4];

// Initial stack pointer, which points to argc.
static void* init_sp;

/* Where we expect to find all our aux files (namely, stage2) */
static const char *valgrind_lib = VG_LIBDIR;

/* stage2's name */
static const char stage2[] = "stage2";

/*------------------------------------------------------------*/
/*--- Auxv modification                                    ---*/
/*------------------------------------------------------------*/

/* Modify the auxv the kernel gave us to make it look like we were
   execed as the shared object.

   This also inserts a new entry into the auxv table so we can
   communicate some extra information to stage2 (namely, the fd of the
   padding file, so it can identiry and remove the padding later).
*/
static void *fix_auxv(void *v_init_esp, const struct exeinfo *info,
                      int padfile)
{
   struct ume_auxv *auxv;
   int *newesp;
   int seen;
   int delta;
   int i;
   static const int new_entries = 2;

   /* make sure we're running on the private stack */
   assert(&delta >= stack && &delta < &stack[sizeof(stack)/sizeof(*stack)]);
   
   /* find the beginning of the AUXV table */
   auxv = find_auxv(v_init_esp);

   /* Work out how we should move things to make space for the new
      auxv entry. It seems that ld.so wants a 16-byte aligned stack on
      entry, so make sure that's the case. */
   newesp = (int *)(((unsigned long)v_init_esp - new_entries * sizeof(*auxv)) & ~0xf);
   delta = (char *)v_init_esp - (char *)newesp;

   memmove(newesp, v_init_esp, (char *)auxv - (char *)v_init_esp);
   
   v_init_esp = (void *)newesp;
   auxv -= delta/sizeof(*auxv);

   /* stage2 needs this so it can clean up the padding we leave in
      place when we start it */
   auxv[0].a_type = AT_UME_PADFD;
   auxv[0].u.a_val = padfile;

   /* This will be needed by valgrind itself so that it can
      subsequently execve() children.  This needs to be done here
      because /proc/self/exe will go away once we unmap stage1. */
   auxv[1].a_type = AT_UME_EXECFD;
   auxv[1].u.a_val = open("/proc/self/exe", O_RDONLY);

   /* make sure the rest are sane */
   for(i = new_entries; i < delta/sizeof(*auxv); i++) {
      auxv[i].a_type = AT_IGNORE;
      auxv[i].u.a_val = 0;
   }

   /* OK, go through and patch up the auxv entries to match the new
      executable */
   seen = 0;
   for(; auxv->a_type != AT_NULL; auxv++) {
      if (0)
	 printf("doing auxv %p %4lld: %lld %p\n",
                auxv, (ULong)auxv->a_type, (ULong)auxv->u.a_val, auxv->u.a_ptr);

      switch(auxv->a_type) {
      case AT_PHDR:
	 seen |= 1;
	 auxv->u.a_val = info->phdr;
	 break;

      case AT_PHNUM:
	 seen |= 2;
	 auxv->u.a_val = info->phnum;
	 break;

      case AT_BASE:
	 seen |= 4;
	 auxv->u.a_val = info->interp_base;
	 break;

      case AT_ENTRY:
	 seen |= 8;
	 auxv->u.a_val = info->entry;
	 break;

#if (defined(AT_SYSINFO) || defined(AT_SYSINFO_EHDR))
#ifdef AT_SYSINFO
      case AT_SYSINFO:
#endif
#ifdef AT_SYSINFO_EHDR
      case AT_SYSINFO_EHDR:
#endif
	 auxv->a_type = AT_IGNORE;
	 break;
#endif
      }
   }

   /* If we didn't see all the entries we need to fix up, then we
      can't make the new executable viable. */
   if (seen != 0xf) {
      fprintf(stderr, "valgrind: we didn't see enough auxv entries (seen=%x)\n", seen);
      exit(1);
   }

   return v_init_esp;
}


/*------------------------------------------------------------*/
/*--- Address space padding                                ---*/
/*------------------------------------------------------------*/

static void check_mmap(void* res, void* base, int len)
{
   if ((void*)-1 == res) {
      fprintf(stderr, "valgrind: padding mmap(%p, %d) failed during startup.\n"
                      "valgrind: is there a hard virtual memory limit set?\n",
                      base, len);
      exit(1);
   }
}

typedef struct {
   char* fillgap_start;
   char* fillgap_end;
   int   fillgap_padfile;
} fillgap_extra;

static int fillgap(char *segstart, char *segend, const char *perm, off_t off, 
                   int maj, int min, int ino, void* e)
{
   fillgap_extra* extra = e;

   if (segstart >= extra->fillgap_end)
      return 0;

   if (segstart > extra->fillgap_start) {
      void* res = mmap(extra->fillgap_start, segstart - extra->fillgap_start,
                       PROT_NONE, MAP_FIXED|MAP_PRIVATE, 
                       extra->fillgap_padfile, 0);
      check_mmap(res, extra->fillgap_start, segstart - extra->fillgap_start);
   }
   extra->fillgap_start = segend;
   
   return 1;
}

// Choose a name for the padfile, open it.
int as_openpadfile(void)
{
   char buf[256];
   int padfile;
   int seq = 1;
   do {
      snprintf(buf, 256, "/tmp/.pad.%d.%d", getpid(), seq++);
      padfile = open(buf, O_RDWR|O_CREAT|O_EXCL, 0);
      unlink(buf);
      if (padfile == -1 && errno != EEXIST) {
         fprintf(stderr, "valgrind: couldn't open padfile\n");
         exit(44);
      }
   } while(padfile == -1);

   return padfile;
}

// Pad all the empty spaces in a range of address space to stop interlopers.
void as_pad(void *start, void *end, int padfile)
{
   fillgap_extra extra;
   extra.fillgap_start   = start;
   extra.fillgap_end     = end;
   extra.fillgap_padfile = padfile;

   foreach_map(fillgap, &extra);
	
   if (extra.fillgap_start < extra.fillgap_end) {
      void* res = mmap(extra.fillgap_start, 
                       extra.fillgap_end - extra.fillgap_start,
                       PROT_NONE, MAP_FIXED|MAP_PRIVATE, padfile, 0);
      check_mmap(res, extra.fillgap_start, 
                 extra.fillgap_end - extra.fillgap_start);
   }
}


/*------------------------------------------------------------*/
/*--- main() and related pieces                            ---*/
/*------------------------------------------------------------*/

static int prmap(char *start, char *end, const char *perm, off_t off, int maj,
                 int min, int ino, void* dummy) {
   printf("mapping %10p-%10p %s %02x:%02x %d\n",
          start, end, perm, maj, min, ino);
   return 1;
}

static void main2(void)
{
   int err, padfile;
   struct exeinfo info;
   extern char _end;
   int *esp;
   char buf[strlen(valgrind_lib) + sizeof(stage2) + 16];

   info.exe_end  = PGROUNDDN(init_sp);
#ifdef HAVE_PIE
   info.exe_base = ROUNDDN(info.exe_end - 0x02000000, 0x10000000);
   assert(info.exe_base >= PGROUNDUP(&_end));
   info.map_base = info.exe_base + 0x01000000;
#else
   // If this system doesn't have PIE (position-independent executables),
   // we have to choose a hardwired location for stage2.
   info.exe_base = PGROUNDUP(&_end);
   info.map_base = KICKSTART_BASE + 0x01000000;
#endif

   info.argv = NULL;

   snprintf(buf, sizeof(buf), "%s/%s", valgrind_lib, stage2);

   err = do_exec(buf, &info);

   if (err != 0) {
      fprintf(stderr, "valgrind: failed to load %s: %s\n",
	      buf, strerror(err));
      exit(1);
   }

   /* Make sure stage2's dynamic linker can't tromp on the lower part
      of the address space. */
   padfile = as_openpadfile();
   as_pad(0, (void *)info.map_base, padfile);
   
   esp = fix_auxv(init_sp, &info, padfile);

   if (0) {
      printf("---------- launch stage 2 ----------\n");
      printf("eip=%p esp=%p\n", (void *)info.init_eip, esp);
      foreach_map(prmap, /*dummy*/NULL);
   }

   jmp_with_stack(info.init_eip, (addr_t)esp);   
}

int main(int argc, char** argv)
{
   struct rlimit rlim;
   const char *cp = getenv(VALGRINDLIB);

   if (cp != NULL)
      valgrind_lib = cp;

   // Initial stack pointer is to argc, which is immediately before argv[0]
   // on the stack.  Nb: Assumes argc is word-aligned.
   init_sp = argv - 1;

   /* Set the address space limit as high as it will go, since we make
      a lot of very large mappings. */
   getrlimit(RLIMIT_AS, &rlim);
   rlim.rlim_cur = rlim.rlim_max;
   setrlimit(RLIMIT_AS, &rlim);

   /* move onto another stack so we can play with the main one */
   jmp_with_stack((addr_t)main2, (addr_t)stack + sizeof(stack));
}

/*--------------------------------------------------------------------*/
/*--- end                                                 stage1.c ---*/
/*--------------------------------------------------------------------*/
