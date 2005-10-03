
/*--------------------------------------------------------------------*/
/*--- User-mode execve(), and other stuff shared between stage1    ---*/
/*--- and stage2.                                          m_ume.c ---*/
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


#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64

// It seems that on SuSE 9.1 (x86) something in <fcntl.h> messes up stuff
// acquired indirectly from vki-x86-linux.h.  Therefore our headers must be
// included ahead of the glibc ones.  This fix is a kludge;  the right
// solution is to entirely remove the glibc dependency.
#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_machine.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcfile.h"    // VG_(close) et al
#include "pub_core_libcproc.h"    // VG_(geteuid), VG_(getegid)
#include "pub_core_libcassert.h"  // VG_(exit), vg_assert
#include "pub_core_syscall.h"     // VG_(strerror)
#include "pub_core_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_core_aspacemgr.h"   // various mapping fns
#include "vki_unistd.h"           // mmap-related constants

#include "pub_core_ume.h"


#if	VG_WORDSIZE == 8
#define ESZ(x)	Elf64_##x
#elif	VG_WORDSIZE == 4
#define ESZ(x)	Elf32_##x
#else
#error VG_WORDSIZE needs to ==4 or ==8
#endif

struct elfinfo
{
   ESZ(Ehdr)	e;
   ESZ(Phdr)	*p;
   int		fd;
};

static void check_mmap(SysRes res, Addr base, SizeT len)
{
   if (res.isError) {
      VG_(printf)("valgrind: mmap(0x%llx, %lld) failed in UME.\n", 
                  (ULong)base, (Long)len);
      VG_(exit)(1);
   }
}

//zz // 'extra' allows the caller to pass in extra args to 'fn', like free
//zz // variables to a closure.
//zz void VG_(foreach_map)(int (*fn)(char *start, char *end,
//zz                                 const char *perm, off_t offset,
//zz                                 int maj, int min, int ino, void* extra),
//zz                       void* extra)
//zz {
//zz    static char buf[10240];
//zz    char *bufptr = buf;
//zz    int ret, fd;
//zz 
//zz    fd = open("/proc/self/maps", O_RDONLY);
//zz 
//zz    if (fd == -1) {
//zz       perror("open /proc/self/maps");
//zz       return;
//zz    }
//zz 
//zz    ret = read(fd, buf, sizeof(buf));
//zz 
//zz    if (ret == -1) {
//zz       perror("read /proc/self/maps");
//zz       close(fd);
//zz       return;
//zz    }
//zz    close(fd);
//zz 
//zz    if (ret == sizeof(buf)) {
//zz       VG_(printf)("coregrind/m_ume.c: buf too small\n");
//zz       return;
//zz    }
//zz 
//zz    while(bufptr && bufptr < buf+ret) {
//zz       char perm[5];
//zz       ULong offset;
//zz       int maj, min;
//zz       int ino;
//zz       void *segstart, *segend;
//zz 
//zz       sscanf(bufptr, "%p-%p %s %llx %x:%x %d",
//zz 	     &segstart, &segend, perm, &offset, &maj, &min, &ino);
//zz       bufptr = strchr(bufptr, '\n');
//zz       if (bufptr != NULL)
//zz 	 bufptr++; /* skip \n */
//zz 
//zz       if (!(*fn)(segstart, segend, perm, offset, maj, min, ino, extra))
//zz 	 break;
//zz    }
//zz }
//zz 
//zz /*------------------------------------------------------------*/
//zz /*--- Stack switching                                      ---*/
//zz /*------------------------------------------------------------*/
//zz 
//zz // __attribute__((noreturn))
//zz // void VG_(jump_and_switch_stacks) ( Addr stack, Addr dst );
//zz #if defined(VGA_x86)
//zz // 4(%esp) == stack
//zz // 8(%esp) == dst
//zz asm(
//zz ".global vgPlain_jump_and_switch_stacks\n"
//zz "vgPlain_jump_and_switch_stacks:\n"
//zz "   movl   %esp, %esi\n"      // remember old stack pointer
//zz "   movl   4(%esi), %esp\n"   // set stack
//zz "   pushl  8(%esi)\n"         // dst to stack
//zz "   movl $0, %eax\n"          // zero all GP regs
//zz "   movl $0, %ebx\n"
//zz "   movl $0, %ecx\n"
//zz "   movl $0, %edx\n"
//zz "   movl $0, %esi\n"
//zz "   movl $0, %edi\n"
//zz "   movl $0, %ebp\n"
//zz "   ret\n"                    // jump to dst
//zz "   ud2\n"                    // should never get here
//zz );
//zz #elif defined(VGA_amd64)
//zz // %rdi == stack
//zz // %rsi == dst
//zz asm(
//zz ".global vgPlain_jump_and_switch_stacks\n"
//zz "vgPlain_jump_and_switch_stacks:\n"
//zz "   movq   %rdi, %rsp\n"   // set stack
//zz "   pushq  %rsi\n"         // dst to stack
//zz "   movq $0, %rax\n"       // zero all GP regs
//zz "   movq $0, %rbx\n"
//zz "   movq $0, %rcx\n"
//zz "   movq $0, %rdx\n"
//zz "   movq $0, %rsi\n"
//zz "   movq $0, %rdi\n"
//zz "   movq $0, %rbp\n"
//zz "   movq $0, %r8\n"
//zz "   movq $0, %r9\n"
//zz "   movq $0, %r10\n"
//zz "   movq $0, %r11\n"
//zz "   movq $0, %r12\n"
//zz "   movq $0, %r13\n"
//zz "   movq $0, %r14\n"
//zz "   movq $0, %r15\n"
//zz "   ret\n"                 // jump to dst
//zz "   ud2\n"                 // should never get here
//zz );
//zz 
//zz #elif defined(VGA_ppc32)
//zz /* Jump to 'dst', but first set the stack pointer to 'stack'.  Also,
//zz    clear all the integer registers before entering 'dst'.  It's
//zz    important that the stack pointer is set to exactly 'stack' and not
//zz    (eg) stack - apparently_harmless_looking_small_offset.  Basically
//zz    because the code at 'dst' might be wanting to scan the area above
//zz    'stack' (viz, the auxv array), and putting spurious words on the
//zz    stack confuses it.
//zz */
//zz // %r3 == stack
//zz // %r4 == dst
//zz asm(
//zz ".global vgPlain_jump_and_switch_stacks\n"
//zz "vgPlain_jump_and_switch_stacks:\n"
//zz "   mtctr %r4\n\t"         // dst to %ctr
//zz "   mr %r1,%r3\n\t"        // stack to %sp
//zz "   li 0,0\n\t"            // zero all GP regs
//zz "   li 3,0\n\t"
//zz "   li 4,0\n\t"
//zz "   li 5,0\n\t"
//zz "   li 6,0\n\t"
//zz "   li 7,0\n\t"
//zz "   li 8,0\n\t"
//zz "   li 9,0\n\t"
//zz "   li 10,0\n\t"
//zz "   li 11,0\n\t"
//zz "   li 12,0\n\t"
//zz "   li 13,0\n\t"           // CAB: This right? r13 = small data area ptr
//zz "   li 14,0\n\t"
//zz "   li 15,0\n\t"
//zz "   li 16,0\n\t"
//zz "   li 17,0\n\t"
//zz "   li 18,0\n\t"
//zz "   li 19,0\n\t"
//zz "   li 20,0\n\t"
//zz "   li 21,0\n\t"
//zz "   li 22,0\n\t"
//zz "   li 23,0\n\t"
//zz "   li 24,0\n\t"
//zz "   li 25,0\n\t"
//zz "   li 26,0\n\t"
//zz "   li 27,0\n\t"
//zz "   li 28,0\n\t"
//zz "   li 29,0\n\t"
//zz "   li 30,0\n\t"
//zz "   li 31,0\n\t"
//zz "   mtxer 0\n\t"
//zz "   mtcr 0\n\t"
//zz "   mtlr %r0\n\t"
//zz "   bctr\n\t"              // jump to dst
//zz "   trap\n"                // should never get here
//zz );
//zz 
//zz #else
//zz #  error Unknown architecture
//zz #endif

/*------------------------------------------------------------*/
/*--- Finding auxv on the stack                            ---*/
/*------------------------------------------------------------*/

struct ume_auxv *VG_(find_auxv)(UWord* sp)
{
   sp++;                // skip argc (Nb: is word-sized, not int-sized!)

   while (*sp != 0)     // skip argv
      sp++;
   sp++;

   while (*sp != 0)     // skip env
      sp++;
   sp++;
   
#if defined(VGA_ppc32)
# if defined AT_IGNOREPPC
   while (*sp == AT_IGNOREPPC)        // skip AT_IGNOREPPC entries
      sp += 2;
# endif
#endif

   return (struct ume_auxv *)sp;
}

/*------------------------------------------------------------*/
/*--- Loading ELF files                                    ---*/
/*------------------------------------------------------------*/

static 
struct elfinfo *readelf(int fd, const char *filename)
{
   SysRes sres;
   struct elfinfo *e = VG_(malloc)(sizeof(*e));
   int phsz;

   vg_assert(e);
   e->fd = fd;

   sres = VG_(pread)(fd, &e->e, sizeof(e->e), 0);
   if (sres.isError || sres.val != sizeof(e->e)) {
      VG_(printf)("valgrind: %s: can't read ELF header: %s\n", 
                  filename, VG_(strerror)(sres.val));
      goto bad;
   }

   if (VG_(memcmp)(&e->e.e_ident[0], ELFMAG, SELFMAG) != 0) {
      VG_(printf)("valgrind: %s: bad ELF magic number\n", filename);
      goto bad;
   }
   if (e->e.e_ident[EI_CLASS] != VG_ELF_CLASS) {
      VG_(printf)("valgrind: wrong ELF executable class "
                  "(eg. 32-bit instead of 64-bit)\n");
      goto bad;
   }
   if (e->e.e_ident[EI_DATA] != VG_ELF_DATA2XXX) {
      VG_(printf)("valgrind: executable has wrong endian-ness\n");
      goto bad;
   }
   if (!(e->e.e_type == ET_EXEC || e->e.e_type == ET_DYN)) {
      VG_(printf)("valgrind: this is not an executable\n");
      goto bad;
   }

   if (e->e.e_machine != VG_ELF_MACHINE) {
      VG_(printf)("valgrind: executable is not for "
                  "this architecture\n");
      goto bad;
   }

   if (e->e.e_phentsize != sizeof(ESZ(Phdr))) {
      VG_(printf)("valgrind: sizeof ELF Phdr wrong\n");
      goto bad;
   }

   phsz = sizeof(ESZ(Phdr)) * e->e.e_phnum;
   e->p = VG_(malloc)(phsz);
   vg_assert(e->p);

   sres = VG_(pread)(fd, e->p, phsz, e->e.e_phoff);
   if (sres.isError || sres.val != phsz) {
      VG_(printf)("valgrind: can't read phdr: %s\n", 
                  VG_(strerror)(sres.val));
      VG_(free)(e->p);
      goto bad;
   }

   return e;

  bad:
   VG_(free)(e);
   return NULL;
}

/* Map an ELF file.  Returns the brk address. */
static
ESZ(Addr) mapelf(struct elfinfo *e, ESZ(Addr) base)
{
   Int    i;
   SysRes res;
   ESZ(Addr) elfbrk = 0;

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];
      ESZ(Addr) addr, brkaddr;
      ESZ(Word) memsz;

      if (ph->p_type != PT_LOAD)
	 continue;

      addr    = ph->p_vaddr+base;
      memsz   = ph->p_memsz;
      brkaddr = addr+memsz;

      if (brkaddr > elfbrk)
	 elfbrk = brkaddr;
   }

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];
      ESZ(Addr) addr, bss, brkaddr;
      ESZ(Off) off;
      ESZ(Word) filesz;
      ESZ(Word) memsz;
      unsigned prot = 0;

      if (ph->p_type != PT_LOAD)
	 continue;

      if (ph->p_flags & PF_X) prot |= VKI_PROT_EXEC;
      if (ph->p_flags & PF_W) prot |= VKI_PROT_WRITE;
      if (ph->p_flags & PF_R) prot |= VKI_PROT_READ;

      addr    = ph->p_vaddr+base;
      off     = ph->p_offset;
      filesz  = ph->p_filesz;
      bss     = addr+filesz;
      memsz   = ph->p_memsz;
      brkaddr = addr+memsz;

      // Tom says: In the following, do what the Linux kernel does and only
      // map the pages that are required instead of rounding everything to
      // the specified alignment (ph->p_align).  (AMD64 doesn't work if you
      // use ph->p_align -- part of stage2's memory gets trashed somehow.)
      //
      // The condition handles the case of a zero-length segment.
      if (VG_PGROUNDUP(bss)-VG_PGROUNDDN(addr) > 0) {
         if (0) VG_(debugLog)(0,"ume","mmap_file_fixed_client #1\n");
         res = VG_(am_mmap_file_fixed_client)(
                  VG_PGROUNDDN(addr),
                  VG_PGROUNDUP(bss)-VG_PGROUNDDN(addr),
                  prot, /*VKI_MAP_FIXED|VKI_MAP_PRIVATE, */
                  e->fd, VG_PGROUNDDN(off)
               );
         if (0) VG_(am_show_nsegments)(0,"after #1");
         check_mmap(res, VG_PGROUNDDN(addr),
                         VG_PGROUNDUP(bss)-VG_PGROUNDDN(addr));
      }

      // if memsz > filesz, fill the remainder with zeroed pages
      if (memsz > filesz) {
	 UInt bytes;

	 bytes = VG_PGROUNDUP(brkaddr)-VG_PGROUNDUP(bss);
	 if (bytes > 0) {
            if (0) VG_(debugLog)(0,"ume","mmap_anon_fixed_client #2\n");
	    res = VG_(am_mmap_anon_fixed_client)(
                     VG_PGROUNDUP(bss), bytes,
		     prot
                  );
            if (0) VG_(am_show_nsegments)(0,"after #2");
            check_mmap(res, VG_PGROUNDUP(bss), bytes);
         }

	 bytes = bss & (VKI_PAGE_SIZE - 1);

         // The 'prot' condition allows for a read-only bss
         if ((prot & VKI_PROT_WRITE) && (bytes > 0)) {
	    bytes = VKI_PAGE_SIZE - bytes;
	    VG_(memset)((char *)bss, 0, bytes);
	 }
      }
   }

   return elfbrk;
}

// Forward declaration.
/* returns: 0 = success, non-0 is failure */
static int do_exec_inner(const char *exe, struct exeinfo *info);

static int match_ELF(const char *hdr, int len)
{
   ESZ(Ehdr) *e = (ESZ(Ehdr) *)hdr;
   return (len > sizeof(*e)) && VG_(memcmp)(&e->e_ident[0], ELFMAG, SELFMAG) == 0;
}


/* load_ELF pulls an ELF executable into the address space, prepares
   it for execution, and writes info about it into INFO.  In
   particular it fills in .init_eip, which is the starting point.

   Returns zero on success, non-zero (a VKI_E.. value) on failure.

   The sequence of activities is roughly as follows:

   - use readelf() to extract program header info from the exe file.

   - scan the program header, collecting info (not sure what all those
     info-> fields are, or whether they are used, but still) and in
     particular looking out fo the PT_INTERP header, which describes
     the interpreter.  If such a field is found, the space needed to
     hold the interpreter is computed into interp_size.

   - map the executable in, by calling mapelf().  This maps in all
     loadable sections, and I _think_ also creates any .bss areas
     required.  mapelf() returns the address just beyond the end of
     the furthest-along mapping it creates.  The executable is mapped
     starting at EBASE, which is usually read from it (eg, 0x8048000
     etc) except if it's a PIE, in which case I'm not sure what
     happens.

     The returned address is recorded in info->brkbase as the start
     point of the brk (data) segment, as it is traditional to place
     the data segment just after the executable.  Neither load_ELF nor
     mapelf creates the brk segment, though: that is for the caller of
     load_ELF to attend to.

   - If the initial phdr scan didn't find any mention of an
     interpreter (interp == NULL), this must be a statically linked
     executable, and we're pretty much done.

   - Otherwise, we need to use mapelf() a second time to load the
     interpreter.  The interpreter can go anywhere, but mapelf() wants
     to be told a specific address to put it at.  So an advisory query
     is passed to aspacem, asking where it would put an anonymous
     client mapping of size INTERP_SIZE.  That address is then used
     as the mapping address for the interpreter.

   - The entry point in INFO is set to the interpreter's entry point,
     and we're done.  */
static int load_ELF(char *hdr, int len, int fd, const char *name,
                    /*MOD*/struct exeinfo *info)
{
   SysRes sres;
   struct elfinfo *e;
   struct elfinfo *interp = NULL;
   ESZ(Addr) minaddr = ~0;	/* lowest mapped address */
   ESZ(Addr) maxaddr = 0;	/* highest mapped address */
   ESZ(Addr) interp_addr = 0;	/* interpreter (ld.so) address */
   ESZ(Word) interp_size = 0;	/* interpreter size */
   ESZ(Word) interp_align = VKI_PAGE_SIZE;
   int i;
   void *entry;
   ESZ(Addr) ebase = 0;

#ifdef HAVE_PIE
   ebase = info->exe_base;
#endif

   e = readelf(fd, name);

   if (e == NULL)
      return VKI_ENOEXEC;

   /* The kernel maps position-independent executables at TASK_SIZE*2/3;
      duplicate this behavior as close as we can. */
   if (e->e.e_type == ET_DYN && ebase == 0) {
      ebase = VG_PGROUNDDN(info->exe_base + (info->exe_end - info->exe_base) * 2 / 3);
   }

   info->phnum = e->e.e_phnum;
   info->entry = e->e.e_entry + ebase;
   info->phdr = 0;

   for(i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];

      switch(ph->p_type) {
      case PT_PHDR:
	 info->phdr = ph->p_vaddr + ebase;
	 break;

      case PT_LOAD:
	 if (ph->p_vaddr < minaddr)
	    minaddr = ph->p_vaddr;
	 if (ph->p_vaddr+ph->p_memsz > maxaddr)
	    maxaddr = ph->p_vaddr+ph->p_memsz;
	 break;
			
      case PT_INTERP: {
	 char *buf = VG_(malloc)(ph->p_filesz+1);
	 int j;
	 int intfd;
	 int baseaddr_set;

         vg_assert(buf);
	 VG_(pread)(fd, buf, ph->p_filesz, ph->p_offset);
	 buf[ph->p_filesz] = '\0';

	 sres = VG_(open)(buf, VKI_O_RDONLY, 0);
         if (sres.isError) {
	    VG_(printf)("valgrind: m_ume.c: can't open interpreter\n");
	    VG_(exit)(1);
	 }
         intfd = sres.val;

	 interp = readelf(intfd, buf);
	 if (interp == NULL) {
	    VG_(printf)("valgrind: m_ume.c: can't read interpreter\n");
	    return 1;
	 }
	 VG_(free)(buf);

	 baseaddr_set = 0;
	 for(j = 0; j < interp->e.e_phnum; j++) {
	    ESZ(Phdr) *iph = &interp->p[j];
	    ESZ(Addr) end;

	    if (iph->p_type != PT_LOAD)
	       continue;
	    
	    if (!baseaddr_set) {
	       interp_addr  = iph->p_vaddr;
	       interp_align = iph->p_align;
	       baseaddr_set = 1;
	    }

	    /* assumes that all segments in the interp are close */
	    end = (iph->p_vaddr - interp_addr) + iph->p_memsz;

	    if (end > interp_size)
	       interp_size = end;
	 }
	 break;

      default:
         // do nothing
         break;
      }
      }
   }

   if (info->phdr == 0)
      info->phdr = minaddr + ebase + e->e.e_phoff;

   if (info->exe_base != info->exe_end) {
      if (minaddr >= maxaddr ||
	  (minaddr + ebase < info->exe_base ||
	   maxaddr + ebase > info->exe_end)) {
	 VG_(printf)("Executable range %p-%p is outside the\n"
                     "acceptable range %p-%p\n",
                     (void *)minaddr + ebase, (void *)maxaddr + ebase,
                     (void *)info->exe_base,  (void *)info->exe_end);
	 return VKI_ENOMEM;
      }
   }

   info->brkbase = mapelf(e, ebase);	/* map the executable */

   if (info->brkbase == 0)
      return VKI_ENOMEM;

   if (interp != NULL) {
      /* reserve a chunk of address space for interpreter */
      Addr       advised;
      Bool       ok;

      /* Don't actually reserve the space.  Just get an advisory
         indicating where it would be allocated, and pass that to
         mapelf(), which in turn asks aspacem to do some fixed maps at
         the specified address.  This is a bit of hack, but it should
         work because there should be no intervening transactions with
         aspacem which could cause those fixed maps to fail. */
      advised = VG_(am_get_advisory_client_simple)( 
                   (Addr)interp_addr, interp_size, &ok 
                );
      if (!ok) {
         /* bomb out */
         SysRes res = VG_(mk_SysRes_Error)(VKI_EINVAL);
         if (0) VG_(printf)("reserve for interp: failed\n");
         check_mmap(res, (Addr)interp_addr, interp_size);
         /*NOTREACHED*/
      }

      (void)mapelf(interp, (ESZ(Addr))advised - interp_addr);

      VG_(close)(interp->fd);

      entry = (void *)(advised - interp_addr + interp->e.e_entry);
      info->interp_base = (ESZ(Addr))advised;

      VG_(free)(interp->p);
      VG_(free)(interp);
   } else
      entry = (void *)(ebase + e->e.e_entry);

   info->exe_base = minaddr + ebase;
   info->exe_end  = maxaddr + ebase;

   info->init_eip = (Addr)entry;

   VG_(free)(e->p);
   VG_(free)(e);

   return 0;
}


static int match_script(const char *hdr, Int len)
{
   return (len > 2) && VG_(memcmp)(hdr, "#!", 2) == 0;
}

/* returns: 0 = success, non-0 is failure */
static int load_script(char *hdr, int len, int fd, const char *name,
                       struct exeinfo *info)
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
      return VKI_ENOEXEC; /* absolute path only for interpreter */

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
   
   info->interp_name = VG_(strdup)(interp);
   vg_assert(NULL != info->interp_name);
   if (arg != NULL && *arg != '\0') {
      info->interp_args = VG_(strdup)(arg);
      vg_assert(NULL != info->interp_args);
   }

   if (info->argv && info->argv[0] != NULL)
      info->argv[0] = (char *)name;

   if (0)
      VG_(printf)("#! script: interp_name=\"%s\" interp_args=\"%s\"\n",
                  info->interp_name, info->interp_args);

   return do_exec_inner(interp, info);
}

/* 
   Emulate the normal Unix permissions checking algorithm.

   If owner matches, then use the owner permissions, else
   if group matches, then use the group permissions, else
   use other permissions.

   Note that we can't deal with SUID/SGID, so we refuse to run them
   (otherwise the executable may misbehave if it doesn't have the
   permissions it thinks it does).
*/
/* returns: 0 = success, non-0 is failure */
static int check_perms(int fd)
{
   struct vki_stat st;

   if (VG_(fstat)(fd, &st) == -1) 
      return VKI_EACCES;

   if (st.st_mode & (VKI_S_ISUID | VKI_S_ISGID)) {
      //VG_(printf)("Can't execute suid/sgid executable %s\n", exe);
      return VKI_EACCES;
   }

   if (VG_(geteuid)() == st.st_uid) {
      if (!(st.st_mode & VKI_S_IXUSR))
	 return VKI_EACCES;
   } else {
      int grpmatch = 0;

      if (VG_(getegid)() == st.st_gid)
	 grpmatch = 1;
      else {
	 UInt groups[32];
	 Int ngrp = VG_(getgroups)(32, groups);
	 Int i;
         /* ngrp will be -1 if VG_(getgroups) failed. */
         for (i = 0; i < ngrp; i++) {
	    if (groups[i] == st.st_gid) {
	       grpmatch = 1;
	       break;
	    }
         }
      }

      if (grpmatch) {
	 if (!(st.st_mode & VKI_S_IXGRP))
	    return VKI_EACCES;
      } else if (!(st.st_mode & VKI_S_IXOTH))
	 return VKI_EACCES;
   }

   return 0;
}

/* returns: 0 = success, non-0 is failure */
static int do_exec_inner(const char *exe, struct exeinfo *info)
{
   SysRes sres;
   int fd;
   int err;
   char buf[VKI_PAGE_SIZE];
   int bufsz;
   int i;
   int ret;
   static const struct {
      int (*match)(const char *hdr, int len);
      int (*load) (      char *hdr, int len, int fd2, const char *name,
                         struct exeinfo *);
   } formats[] = {
      { match_ELF,    load_ELF },
      { match_script, load_script },
   };

   sres = VG_(open)(exe, VKI_O_RDONLY, 0);
   if (sres.isError) {
      if (0)
	 VG_(printf)("Can't open executable %s: %s\n",
                     exe, VG_(strerror)(sres.val));
      return sres.val;
   }
   fd = sres.val;

   err = check_perms(fd);
   if (err != 0) {
      VG_(close)(fd);
      return err;
   }

   bufsz = VG_(fsize)(fd);
   if (bufsz > sizeof(buf))
      bufsz = sizeof(buf);

   sres = VG_(pread)(fd, buf, bufsz, 0);
   if (sres.isError || sres.val != bufsz) {
      VG_(printf)("Can't read executable header: %s\n",
                  VG_(strerror)(sres.val));
      VG_(close)(fd);
      return sres.val;
   }
   bufsz = sres.val;

   ret = VKI_ENOEXEC;
   for(i = 0; i < sizeof(formats)/sizeof(*formats); i++) {
      if ((formats[i].match)(buf, bufsz)) {
	 ret = (formats[i].load)(buf, bufsz, fd, exe, info);
	 break;
      }
   }

   VG_(close)(fd);

   return ret;
}

// See ume.h for an indication of which entries of 'info' are inputs, which
// are outputs, and which are both.
/* returns: 0 = success, non-0 is failure */
int VG_(do_exec)(const char *exe, struct exeinfo *info)
{
   info->interp_name = NULL;
   info->interp_args = NULL;

   return do_exec_inner(exe, info);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
