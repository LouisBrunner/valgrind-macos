
/*--------------------------------------------------------------------*/
/*--- User-mode execve()                                     ume.c ---*/
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

/*
  User-mode exec

  This bootstraps Valgrind.  This code decides on the layout of the
  client and Valgrind address spaces, loads valgrind.so and the
  skin.so into the valgrind part, loads the client executable (and the
  dynamic linker, if necessary) into the client part, and calls into
  Valgrind proper.

  The code is careful not to allow spurious mappings to appear in the
  wrong parts of the address space.  In particular, to make sure
  dlopen puts things in the right place, it will pad out the forbidden
  chunks of address space so that dlopen is forced to put things where
  we want them.

  The memory map it creates is:

  CLIENT_BASE    +-------------------------+
                 | client address space    |
	         :                         :
	         :                         :
		 | client stack            |
  client_end     +-------------------------+
                 | redzone                 |
  shadow_base    +-------------------------+
                 |                         |
	         : shadow memory for skins :
	         | (may be 0 sized)        |
  shadow_end     +-------------------------+
                 : gap (may be 0 sized)    :
  valgrind_base  +-------------------------+
                 | valgrind .so files      |
		 | and mappings            |
  valgrind_mmap_end                        -
                 | kickstart executable    |
                 -                         -
                 | valgrind heap  vvvvvvvvv|
  valgrind_end   -                         -
		 | valgrind stack ^^^^^^^^^|
                 +-------------------------+
		 : kernel                  :
 */

#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64

#include "vg_include.h"

#include <stddef.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <elf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <asm/unistd.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <dlfcn.h>
#include <assert.h>

#include "ume.h"

static int padfile = -1;
static struct stat padstat;

extern int kickstart_base;	/* linker created */

void foreach_map(int (*fn)(void *start, void *end,
			   const char *perm, off_t offset,
			   int maj, int min, int ino))
{
   static char buf[10240];
   char *bufptr = buf;
   int ret, fd;

   fd = open("/proc/self/maps", O_RDONLY);

   if (fd == -1) {
      perror("open /proc/self/maps");
      return;
   }

   ret = read(fd, buf, sizeof(buf));

   if (ret == -1) {
      perror("read /proc/self/maps");
      close(fd);
      return;
   }
   close(fd);

   if (ret == sizeof(buf)) {
      fprintf(stderr, "buf too small\n");
      return;
   }

   while(bufptr && bufptr < buf+ret) {
      char perm[5];
      off_t offset;
      int maj, min;
      int ino;
      void *segstart, *segend;

      sscanf(bufptr, "%p-%p %s %Lx %x:%x %d",
	     &segstart, &segend, perm, &offset, &maj, &min, &ino);
      bufptr = strchr(bufptr, '\n');
      if (bufptr != NULL)
	 bufptr++; /* skip \n */

      if (!(*fn)(segstart, segend, perm, offset, maj, min, ino))
	 break;
   }
}

/* pad all the empty spaces in a range of address space to stop
   interlopers */
void as_pad(void *start, void *end)
{
   char buf[1024];
   char *addr;

   int fillgap(void *segstart, void *segend, const char *perm, off_t off, 
	       int maj, int min, int ino) {
      if (segstart >= end)
	 return 0;

      if ((char *)segstart > addr)
	 mmap(addr, (char *)segstart-addr, PROT_NONE, MAP_FIXED|MAP_PRIVATE,
	      padfile, 0);
      addr = segend;
	 
      return 1;
   }

   if (padfile == -1) {
      int seq = 1;
      do {
	 sprintf(buf, "/tmp/.pad.%d.%d", getpid(), seq++);
	 padfile = open(buf, O_RDWR|O_CREAT|O_EXCL, 0);
	 unlink(buf);
	 if (padfile == -1 && errno != EEXIST)
	    exit(44);
      } while(padfile == -1);
      fstat(padfile, &padstat);
   }

   addr = start;

   foreach_map(fillgap);
	
   if (addr < (char *)end)
      mmap(addr, (char *)end-addr, PROT_NONE, MAP_FIXED|MAP_PRIVATE,
	   padfile, 0);
}

/* remove padding from a range of address space - padding is always a
   mapping of padfile*/
void as_unpad(void *start, void *end)
{
   int killpad(void *segstart, void *segend, const char *perm, off_t off, 
	       int maj, int min, int ino) {
      void *b, *e;

      if (padstat.st_dev != makedev(maj, min) || padstat.st_ino != ino)
	 return 1;

      if (segend <= start || segstart >= end)
	 return 1;

      if (segstart <= start)
	 b = start;
      else
	 b = segstart;

      if (segend >= end)
	 e = end;
      else
	 e = segend;

      munmap(b, (char *)e-(char *)b);

      return 1;
   }

   if (padfile == -1)	/* no padfile, no padding */
      return;

   foreach_map(killpad);
}

void as_closepadfile(void)
{
   /* don't unpad */
   close(padfile);
   padfile = -1;
}

int as_getpadfd(void)
{
   return padfile;
}

void as_setpadfd(int fd)
{
   as_closepadfile();
   padfile = fd;
   fstat(padfile, &padstat);
}

struct ume_auxv *find_auxv(int *esp)
{
   esp++;			/* skip argc */

   while(*esp != 0)		/* skip argv */
      esp++;
   esp++;

   while(*esp != 0)		/* skip env */
      esp++;
   esp++;
   
   return (struct ume_auxv *)esp;
}


struct elfinfo *readelf(int fd, const char *filename)
{
   struct elfinfo *e = malloc(sizeof(*e));
   int phsz;

   e->fd = fd;

   if (pread(fd, &e->e, sizeof(e->e), 0) != sizeof(e->e)) {
      fprintf(stderr, "%s: can't read elf header: %s\n", 
	      filename, strerror(errno));
      return NULL;
   }

   if (memcmp(&e->e.e_ident[0], ELFMAG, SELFMAG) != 0) {
      fprintf(stderr, "%s: bad ELF magic\n",
	      filename);
      return NULL;
   }
   if (e->e.e_ident[EI_CLASS] != ELFCLASS32) {
      fprintf(stderr, "Can only handle 32-bit executables\n");
      return NULL;
   }
   if (e->e.e_ident[EI_DATA] != ELFDATA2LSB) {
      fprintf(stderr, "Expecting little-endian\n");
      return NULL;
   }
   if (!(e->e.e_type == ET_EXEC || e->e.e_type == ET_DYN)) {
      fprintf(stderr, "need executable\n");
      return NULL;
   }

   if (e->e.e_machine != EM_386) {
      fprintf(stderr, "need x86\n");
      return NULL;
   }

   if (e->e.e_phentsize != sizeof(ESZ(Phdr))) {
      fprintf(stderr, "sizeof Phdr wrong\n");
      return NULL;
   }

   phsz = sizeof(ESZ(Phdr)) * e->e.e_phnum;
   e->p = malloc(phsz);

   if (pread(fd, e->p, phsz, e->e.e_phoff) != phsz) {
      fprintf(stderr, "can't read phdr: %s\n", strerror(errno));
      return NULL;
   }

   return e;
}

#define REMAINS(x, a)   ((x)        & ((a)-1))

/* Map an ELF file.  Returns the brk address. */
ESZ(Addr) mapelf(struct elfinfo *e, ESZ(Addr) base, int setbrk)
{
   int i;
   ESZ(Addr) elfbrk = 0;

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];
      ESZ(Addr) addr, brkaddr;
      ESZ(Word) memsz;

      if (ph->p_type != PT_LOAD)
	 continue;

      addr = ph->p_vaddr+base;
      memsz = ph->p_memsz;
      brkaddr = addr+memsz;

      if (brkaddr > elfbrk)
	 elfbrk = brkaddr;
   }

   if (setbrk) {
      /* sneaking up on the brk limit works better than actually
	 jumping directly there.  Unfortunately, setting the brk is
	 tested against the datasize rlimit, even though we're not
	 actually using any memory. */
      char *b = sbrk(0);
      char *initb = (char *)PGROUNDUP(b);

      while(b < (char *)elfbrk) {
	 unsigned delta = (char *)elfbrk - b;
	 static const unsigned limit = 256*1024*1024;
	 char *bb;

	 if (delta > limit)
	    delta = limit;
	 //printf("elfbrk=%p b=%p delta=%u\n", elfbrk, b, delta);
	 bb = sbrk(delta);
	 if (bb != b) {
	    fprintf(stderr, "sbrk failed while adjusting brk base: "
		    "perhaps we hit the datasize ulimit?\n");
	    return 0;
	 }
	 b += delta;
      }
      munmap(initb, (char *)PGROUNDDN(elfbrk)-initb);
   }

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];
      ESZ(Addr) addr, bss, brkaddr;
      ESZ(Off) off;
      ESZ(Word) filesz;
      ESZ(Word) memsz;
      ESZ(Word) align;
      unsigned prot = 0;

      if (ph->p_type != PT_LOAD)
	 continue;

      if (ph->p_flags & PF_X)
	 prot |= PROT_EXEC;
      if (ph->p_flags & PF_W)
	 prot |= PROT_WRITE;
      if (ph->p_flags & PF_R)
	 prot |= PROT_READ;

      align = ph->p_align;

      addr = ph->p_vaddr+base;
      off =  ph->p_offset;
      filesz = ph->p_filesz;
      bss = addr+filesz;
      memsz = ph->p_memsz;
      brkaddr = addr+memsz;

      mmap((char *)ROUNDDN(addr, align), ROUNDUP(bss, align)-ROUNDDN(addr, align),
	   prot, MAP_FIXED|MAP_PRIVATE, e->fd, ROUNDDN(off, align));

      /* if memsz > filesz, then we need to fill the remainder with zeroed pages */
      if (memsz > filesz) {
	 UInt bytes;

	 bytes = ROUNDUP(brkaddr, align)-ROUNDUP(bss, align);
	 if (bytes > 0)
	    mmap((char *)ROUNDUP(bss, align), bytes,
		 prot, MAP_FIXED|MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

	 bytes = bss & (VKI_BYTES_PER_PAGE - 1);
	 if (bytes > 0) {
	    bytes = VKI_BYTES_PER_PAGE - bytes;
	    memset((char *)bss, 0, bytes);
	 }
      }
   }

   return elfbrk;
}


static int do_exec_inner(const char *exe, struct exeinfo *info);


static int match_ELF(const char *hdr, int len)
{
   ESZ(Ehdr) *e = (ESZ(Ehdr) *)hdr;
   return (len > sizeof(*e)) && memcmp(&e->e_ident[0], ELFMAG, SELFMAG) == 0;
}

static int load_ELF(char *hdr, int len, int fd, const char *name, struct exeinfo *info)
{
   struct elfinfo *e;
   struct elfinfo *interp = NULL;
   ESZ(Addr) minaddr = ~0;
   ESZ(Addr) maxaddr = 0;
   ESZ(Addr) interp_addr = 0;
   ESZ(Word) interp_size = 0;
   int i;
   void *entry;

   e = readelf(fd, name);

   if (e == NULL)
      return ENOEXEC;

   info->phnum = e->e.e_phnum;
   info->entry = e->e.e_entry;

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];

      switch(ph->p_type) {
      case PT_PHDR:
	 info->phdr = ph->p_vaddr;
	 break;

      case PT_LOAD:
	 if (ph->p_vaddr < minaddr)
	    minaddr = ph->p_vaddr;
	 if (ph->p_vaddr+ph->p_memsz > maxaddr)
	    maxaddr = ph->p_vaddr+ph->p_memsz;
	 break;
			
      case PT_INTERP: {
	 char *buf = malloc(ph->p_filesz+1);
	 int j;
	 int intfd;
	 int baseaddr_set;

	 pread(fd, buf, ph->p_filesz, ph->p_offset);
	 buf[ph->p_filesz] = '\0';

	 intfd = open(buf, O_RDONLY);
	 if (intfd == -1) {
	    perror("open interp");
	    exit(1);
	 }

	 interp = readelf(intfd, buf);
	 if (interp == NULL) {
	    fprintf(stderr, "Can't read interpreter\n");
	    return 1;
	 }
	 free(buf);

	 baseaddr_set = 0;
	 for(j = 0; j < interp->e.e_phnum; j++) {
	    ESZ(Phdr) *iph = &interp->p[j];
	    ESZ(Addr) end;

	    if (iph->p_type != PT_LOAD)
	       continue;
	    
	    if (!baseaddr_set) {
	       interp_addr = iph->p_vaddr;
	       baseaddr_set = 1;
	    }

	    /* assumes that all segments in the interp are close */
	    end = (iph->p_vaddr - interp_addr) + iph->p_memsz;

	    if (end > interp_size)
	       interp_size = end;
	 }
	 break;
      }
      }
   }

   if (info->exe_base != info->exe_end) {
      if (minaddr >= maxaddr ||
	  (minaddr < info->exe_base ||
	   maxaddr > info->exe_end)) {
	 fprintf(stderr, "Executable is mapped outside of range %p-%p\n",
		 (void *)info->exe_base, (void *)info->exe_end);
	 return ENOMEM;
      }
   }

   info->brkbase = mapelf(e, 0, info->setbrk);		/* map the executable */

   if (info->brkbase == 0)
      return ENOMEM;

   if (interp != NULL) {
      /* reserve a chunk of address space for interpreter */
      char *base = (char *)info->exe_base;
      char *baseoff;
      int flags = MAP_PRIVATE|MAP_ANONYMOUS;

      if (info->map_base != 0) {
	 base = (char *)info->map_base;
	 flags |= MAP_FIXED;
      }

      base = mmap(base, interp_size, PROT_NONE, flags, -1, 0);

      baseoff = base - interp_addr;

      mapelf(interp, (ESZ(Addr))baseoff, 0);

      close(interp->fd);
      free(interp);

      entry = baseoff + interp->e.e_entry;
      info->interp_base = (ESZ(Addr))base;
   } else
      entry = (void *)e->e.e_entry;

   info->exe_base = minaddr;
   info->exe_end = maxaddr;

   info->init_eip = (addr_t)entry;

   free(e);

   return 0;
}


static int match_script(const char *hdr, Int len)
{
   return (len > 2) && memcmp(hdr, "#!", 2) == 0;
}

static int load_script(char *hdr, int len, int fd, const char *name, struct exeinfo *info)
{
   char *interp;
   char *const end = hdr+len;
   char *cp;
   char *arg = NULL;
   int eol;

   interp = hdr + 2;
   while(interp < end && (*interp == ' ' || *interp == '\t'))
      interp++;

   if (*interp != '/')
      return ENOEXEC;		/* absolute path only for interpreter */

   /* skip over interpreter name */
   for(cp = interp; cp < end && *cp != ' ' && *cp != '\t' && *cp != '\n'; cp++)
      ;

   eol = (*cp == '\n');

   *cp++ = '\0';

   if (!eol && cp < end) {
      /* skip space before arg */
      while (cp < end && (*cp == '\t' || *cp == ' '))
	 cp++;

      /* arg is from here to eol */
      arg = cp;
      while (cp < end && *cp != '\n')
	 cp++;
      *cp = '\0';
   }
   
   info->argv0 = strdup(interp);
   if (arg != NULL && *arg != '\0')
      info->argv1 = strdup(arg);

   if (info->argv && info->argv[0] != NULL)
      info->argv[0] = (char *)name;

   if (0)
      printf("#! script: argv0=\"%s\" argv1=\"%s\"\n",
	     info->argv0, info->argv1);

   return do_exec_inner(interp, info);
}

struct binfmt {
   int	(*match)(const char *hdr, int len);
   int	(*load) (      char *hdr, int len, int fd, const char *name, struct exeinfo *);
};

static const struct binfmt formats[] = {
   { match_ELF,		load_ELF },
   { match_script,	load_script },
};


static int do_exec_inner(const char *exe, struct exeinfo *info)
{
   int fd;
   char buf[VKI_BYTES_PER_PAGE];
   int bufsz;
   int i;
   int ret;
   struct stat st;

   fd = open(exe, O_RDONLY);
   if (fd == -1) {
      if (0)
	 fprintf(stderr, "Can't open executable %s: %s\n",
		 exe, strerror(errno));
      return errno;
   }

   if (fstat(fd, &st) == -1) 
      return errno;
   else {
      uid_t uid = geteuid();
      gid_t gid = getegid();
      gid_t groups[32];
      int ngrp = getgroups(32, groups);

      if (st.st_mode & (S_ISUID | S_ISGID)) {
	 fprintf(stderr, "Can't execute suid/sgid executable %s\n", exe);
	 return EACCES;
      }

      if (uid == st.st_uid) {
	 if (!(st.st_mode & S_IXUSR))
	    return EACCES;
      } else {
	 int grpmatch = 0;

	 if (gid == st.st_gid)
	    grpmatch = 1;
	 else 
	    for(i = 0; i < ngrp; i++)
	       if (groups[i] == st.st_gid) {
		  grpmatch = 1;
		  break;
	       }

	 if (grpmatch) {
	    if (!(st.st_mode & S_IXGRP))
	       return EACCES;
	 } else if (!(st.st_mode & S_IXOTH))
	    return EACCES;
      }
   }

   bufsz = pread(fd, buf, sizeof(buf), 0);
   if (bufsz < 0) {
      fprintf(stderr, "Can't read executable header: %s\n",
	      strerror(errno));
      close(fd);
      return errno;
   }

   ret = ENOEXEC;
   for(i = 0; i < sizeof(formats)/sizeof(*formats); i++) {
      if ((formats[i].match)(buf, bufsz)) {
	 ret = (formats[i].load)(buf, bufsz, fd, exe, info);
	 break;
      }
   }

   close(fd);

   return ret;
}

int do_exec(const char *exe, struct exeinfo *info)
{
   info->argv0 = NULL;
   info->argv1 = NULL;

   return do_exec_inner(exe, info);
}

/*--------------------------------------------------------------------*/
/*--- end                                                    ume.c ---*/
/*--------------------------------------------------------------------*/
