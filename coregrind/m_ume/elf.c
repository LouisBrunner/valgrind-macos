
/*--------------------------------------------------------------------*/
/*--- User-mode execve() for ELF executables           m_ume_elf.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)

#include "pub_core_basics.h"
#include "pub_core_vki.h"

#include "pub_core_aspacemgr.h"     // various mapping fns
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"    // VG_(exit), vg_assert
#include "pub_core_libcbase.h"      // VG_(memcmp), etc
#include "pub_core_libcprint.h"
#include "pub_core_libcfile.h"      // VG_(open) et al
#include "pub_core_machine.h"       // VG_ELF_CLASS (XXX: which should be moved)
#include "pub_core_mallocfree.h"    // VG_(malloc), VG_(free)
#include "pub_core_vkiscnums.h"
#include "pub_core_syscall.h"       // VG_(strerror)
#include "pub_core_ume.h"           // self

#include "priv_ume.h"

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#if defined(VGO_linux)
#  define _GNU_SOURCE
#  define _FILE_OFFSET_BITS 64
#endif
/* This is for ELF types etc, and also the AT_ constants. */
#include <elf.h>
#if defined(VGO_solaris)
#  include <sys/fasttrap.h> // PT_SUNWDTRACE_SIZE
#  if defined(SOLARIS_PT_SUNDWTRACE_THRP)
#     define PT_SUNWDTRACE_PROTECTION (PF_R)
#  else
#     define PT_SUNWDTRACE_PROTECTION (PF_R | PF_W | PF_X)
#  endif
#endif
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */


#if     VG_WORDSIZE == 8
#define ESZ(x)  Elf64_##x
#elif   VG_WORDSIZE == 4
#define ESZ(x)  Elf32_##x
#else
#error VG_WORDSIZE needs to ==4 or ==8
#endif

struct elfinfo
{
   ESZ(Ehdr)    e;
   ESZ(Phdr)    *p;
   Int          fd;
};

#if defined(VGO_linux)

/*
   arch_elf_pt_proc() - check a PT_LOPROC..PT_HIPROC ELF program header
      @ehdr: The main ELF header
      @phdr: The program header to check
      @fd:  The ELF file filedescriptor
      @is_interpreter:  True if the phdr is from the interpreter of the ELF
      being loaded, else false.
      @state:  Architecture-specific state preserved throughout the process
      of loading the ELF.

   Inspects the program header phdr to validate its correctness and/or
   suitability for the system. Called once per ELF program header in the
   range PT_LOPROC to PT_HIPROC, for both the ELF being loaded and its
   interpreter.

   Return: Zero to proceed with the ELF load, non-zero to fail the ELF load
           with that return code.

   arch_check_elf()
      @ehdr: The main ELF header
      @has_interpreter: True if the ELF has an interpreter, else false.
      @state:  Architecture-specific state preserved throughout the process
      of loading the ELF.

   Provides a final opportunity for architecture code to reject the loading
   of the ELF. This is called after all program headers to be checked by
   arch_elf_pt_proc have been.

   Return: Zero to proceed with the ELF load, non-zero to fail the ELF load
           with that return code.

   Ref: linux/fs/binfmt_elf.c
 */

#   if defined(VGP_mips32_linux)

/* Ref: linux/arch/mips/kernel/elf.c */
static inline Int arch_elf_pt_proc(ESZ(Ehdr) *ehdr,
                                   ESZ(Phdr) *phdr,
                                   Int fd, Bool is_interpreter,
                                   struct vki_arch_elf_state *state)
{
   struct vki_mips_elf_abiflags_v0 abiflags;
   SysRes sres;

   if ( (ehdr->e_ident[EI_CLASS] == ELFCLASS32) &&
        (ehdr->e_flags & VKI_EF_MIPS_FP64) ) {
      /*
       * Set MIPS_ABI_FP_OLD_64 for EF_MIPS_FP64. We will override it
       * later if needed
       */
      if (is_interpreter)
         state->interp_fp_abi = VKI_MIPS_ABI_FP_OLD_64;
      else
         state->fp_abi = VKI_MIPS_ABI_FP_OLD_64;
   }

   if (phdr->p_type != VKI_PT_MIPS_ABIFLAGS)
      return 0;

   if (phdr->p_filesz < sizeof(abiflags))
      return VKI_EINVAL;

   sres = VG_(pread)(fd, &abiflags, sizeof(abiflags), phdr->p_offset);

   if (sr_isError(sres))
      return sr_Err(sres);

   if (sr_Res(sres) != sizeof(abiflags))
      return VKI_EIO;

   /* Record the required FP ABIs for use by arch_check_elf */
   if (is_interpreter)
      state->interp_fp_abi = abiflags.fp_abi;
   else
      state->fp_abi = abiflags.fp_abi;

   return 0;
}

/* Ref: linux/arch/mips/kernel/elf.c */
static inline Int arch_check_elf(ESZ(Ehdr) *ehdr,
                                 Bool has_interpreter,
                                 struct vki_arch_elf_state *state)
{
   struct mode_req {
      Bool single;
      Bool soft;
      Bool fr1;
      Bool frdefault;
      Bool fre;
   };

   struct mode_req fpu_reqs[] = {
      [VKI_MIPS_ABI_FP_ANY]    = { True,  True,  True,  True,  True  },
      [VKI_MIPS_ABI_FP_DOUBLE] = { False, False, False, True,  True  },
      [VKI_MIPS_ABI_FP_SINGLE] = { True,  False, False, False, False },
      [VKI_MIPS_ABI_FP_SOFT]   = { False, True,  False, False, False },
      [VKI_MIPS_ABI_FP_OLD_64] = { False, False, False, False, False },
      [VKI_MIPS_ABI_FP_XX]     = { False, False, True,  True,  True  },
      [VKI_MIPS_ABI_FP_64]     = { False, False, True,  False, False },
      [VKI_MIPS_ABI_FP_64A]    = { False, False, True,  False, True  }
   };

   /* Mode requirements when .MIPS.abiflags is not present in the ELF.
      Not present means that everything is acceptable except FR1. */
   struct mode_req none_req = { True, True, False, True, True };

   struct mode_req prog_req, interp_req;
   Int fp_abi, interp_fp_abi, abi0, abi1, max_abi;
   Bool is_mips64;

   VexArchInfo vai;
   VG_(machine_get_VexArchInfo)(NULL, &vai);

   fp_abi = state->fp_abi;

   if (has_interpreter) {
      interp_fp_abi = state->interp_fp_abi;

      abi0 = VG_MIN(fp_abi, interp_fp_abi);
      abi1 = VG_MAX(fp_abi, interp_fp_abi);
   } else {
      abi0 = abi1 = fp_abi;
   }

   is_mips64 = (ehdr->e_ident[EI_CLASS] == ELFCLASS64) ||
               (ehdr->e_flags & EF_MIPS_ABI2);

   if (is_mips64) {
      /* MIPS64 code always uses FR=1, thus the default is easy */
      state->overall_fp_mode = VKI_FP_FR1;

      /* Disallow access to the various FPXX & FP64 ABIs */
      max_abi = VKI_MIPS_ABI_FP_SOFT;
   } else {
      /* Default to a mode capable of running code expecting FR=0 */

      /* TODO: Should be changed during implementation of MIPS-R6 support.
         state->overall_fp_mode = cpu_has_mips_r6 ? VKI_FP_FRE : VKI_FP_FR0; */
      state->overall_fp_mode = VKI_FP_FR0;

      /* Allow all ABIs we know about */
      max_abi = VKI_MIPS_ABI_FP_64A;
   }

   if ((abi0 > max_abi && abi0 != VKI_MIPS_ABI_FP_UNKNOWN) ||
       (abi1 > max_abi && abi1 != VKI_MIPS_ABI_FP_UNKNOWN))
      return VKI_ELIBBAD;

   /* It's time to determine the FPU mode requirements */
   prog_req = (abi0 == VKI_MIPS_ABI_FP_UNKNOWN) ? none_req : fpu_reqs[abi0];
   interp_req = (abi1 == VKI_MIPS_ABI_FP_UNKNOWN) ? none_req : fpu_reqs[abi1];

   /* Check whether the program's and interp's ABIs have a matching FPU
      mode requirement. */
   prog_req.single = interp_req.single && prog_req.single;
   prog_req.soft = interp_req.soft && prog_req.soft;
   prog_req.fr1 = interp_req.fr1 && prog_req.fr1;
   prog_req.frdefault = interp_req.frdefault && prog_req.frdefault;
   prog_req.fre = interp_req.fre && prog_req.fre;

   /* Determine the desired FPU mode

      Decision making:

      - We want FR_FRE if FRE=1 and both FR=1 and FR=0 are false. This
        means that we have a combination of program and interpreter
        that inherently require the hybrid FP mode.
      - If FR1 and FRDEFAULT is true, that means we hit the any-abi or
        fpxx case. This is because, in any-ABI (or no-ABI) we have no FPU
        instructions so we don't care about the mode. We will simply use
        the one preferred by the hardware. In fpxx case, that ABI can
        handle both FR=1 and FR=0, so, again, we simply choose the one
        preferred by the hardware. Next, if we only use single-precision
        FPU instructions, and the default ABI FPU mode is not good
        (ie single + any ABI combination), we set again the FPU mode to the
        one is preferred by the hardware. Next, if we know that the code
        will only use single-precision instructions, shown by single being
        true but frdefault being false, then we again set the FPU mode to
        the one that is preferred by the hardware.
      - We want FP_FR1 if that's the only matching mode and the default one
        is not good.
      - Return with ELIBADD if we can't find a matching FPU mode. */
   if (prog_req.fre && !prog_req.frdefault && !prog_req.fr1)
      state->overall_fp_mode = VKI_FP_FRE;
   else if ((prog_req.fr1 && prog_req.frdefault) ||
            (prog_req.single && !prog_req.frdefault))
      state->overall_fp_mode = VEX_MIPS_HOST_FP_MODE(vai.hwcaps) ?
                               VKI_FP_FR1 : VKI_FP_FR0;
   else if (prog_req.fr1)
      state->overall_fp_mode = VKI_FP_FR1;
   else if (!prog_req.fre && !prog_req.frdefault &&
            !prog_req.fr1 && !prog_req.single && !prog_req.soft)
      return VKI_ELIBBAD;

  /* TODO: Currently, Valgrind doesn't support FRE and doesn't support FR1
     emulation on FR0 system, so in those cases we are forced to
     reject the ELF. */
     if ((state->overall_fp_mode == VKI_FP_FRE) ||
         ((state->overall_fp_mode == VKI_FP_FR1) &&
          !VEX_MIPS_HOST_FP_MODE(vai.hwcaps)))
        return VKI_ELIBBAD;

  return 0;
}

#   else

static inline Int arch_elf_pt_proc(ESZ(Ehdr) *ehdr,
                                   ESZ(Phdr) *phdr,
                                   Int fd, Bool is_interpreter,
                                   struct vki_arch_elf_state *state)
{
  /* Dummy implementation, always proceed */
  return 0;
}

static inline Int arch_check_elf(ESZ(Ehdr) *ehdr,
                                 Bool has_interpreter,
                                 struct vki_arch_elf_state *state)
{
  /* Dummy implementation, always proceed */
  return 0;
}

#   endif
#endif

static void check_mmap(SysRes res, Addr base, SizeT len)
{
   if (sr_isError(res)) {
      VG_(printf)("valgrind: mmap(0x%llx, %lld) failed in UME "
                  "with error %lu (%s).\n", 
                  (ULong)base, (Long)len, 
                  sr_Err(res), VG_(strerror)(sr_Err(res)) );
      if (sr_Err(res) == VKI_EINVAL) {
         VG_(printf)("valgrind: this can be caused by executables with "
                     "very large text, data or bss segments.\n");
      }
      VG_(exit)(1);
   }
}

/*------------------------------------------------------------*/
/*--- Loading ELF files                                    ---*/
/*------------------------------------------------------------*/

static 
struct elfinfo *readelf(Int fd, const HChar *filename)
{
   SysRes sres;
   struct elfinfo *e = VG_(malloc)("ume.re.1", sizeof(*e));
   Int phsz;

   e->fd = fd;

   sres = VG_(pread)(fd, &e->e, sizeof(e->e), 0);
   if (sr_isError(sres) || sr_Res(sres) != sizeof(e->e)) {
      VG_(printf)("valgrind: %s: can't read ELF header: %s\n", 
                  filename, VG_(strerror)(sr_Err(sres)));
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
   e->p = VG_(malloc)("ume.re.2", phsz);

   sres = VG_(pread)(fd, e->p, phsz, e->e.e_phoff);
   if (sr_isError(sres) || sr_Res(sres) != phsz) {
      VG_(printf)("valgrind: can't read phdr: %s\n", 
                  VG_(strerror)(sr_Err(sres)));
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

   for (i = 0; i < e->e.e_phnum; i++) {
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

   for (i = 0; i < e->e.e_phnum; i++) {
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
            VG_(memset)((void *)bss, 0, bytes);
         }
      }
   }

   return elfbrk;
}

Bool VG_(match_ELF)(const void *hdr, SizeT len)
{
   const ESZ(Ehdr) *e = hdr;
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
Int VG_(load_ELF)(Int fd, const HChar* name, /*MOD*/ExeInfo* info)
{
   SysRes sres;
   struct elfinfo *e;
   struct elfinfo *interp = NULL;
   ESZ(Addr) minaddr = ~0;      /* lowest mapped address */
   ESZ(Addr) maxaddr = 0;       /* highest mapped address */
   ESZ(Addr) interp_addr = 0;   /* interpreter (ld.so) address */
   ESZ(Word) interp_size = 0;   /* interpreter size */
   /* ESZ(Word) interp_align = VKI_PAGE_SIZE; */ /* UNUSED */
   Int i;
   void *entry;
   ESZ(Addr) ebase = 0;
#  if defined(VGO_solaris)
   ESZ(Addr) thrptr_addr = 0;
#  endif

#  if defined(VGO_linux)
   Int retval;
#  endif

#  if defined(HAVE_PIE)
   ebase = info->exe_base;
#  endif

   e = readelf(fd, name);

   if (e == NULL)
      return VKI_ENOEXEC;

   /* The kernel maps position-independent executables at TASK_SIZE*2/3;
      duplicate this behavior as close as we can. */
   if (e->e.e_type == ET_DYN && ebase == 0) {
      ebase = VG_PGROUNDDN(info->exe_base 
                           + (info->exe_end - info->exe_base) * 2 / 3);
      /* We really don't want to load PIEs at zero or too close.  It
         works, but it's unrobust (NULL pointer reads and writes
         become legit, which is really bad) and causes problems for
         exp-ptrcheck, which assumes all numbers below 1MB are
         nonpointers.  So, hackily, move it above 1MB. */
      /* Later .. it appears ppc32-linux tries to put [vdso] at 1MB,
         which totally screws things up, because nothing else can go
         there.  The size of [vdso] is around 2 or 3 pages, so bump
         the hacky load address along by 8 * VKI_PAGE_SIZE to be safe. */
      /* Later .. on mips64 we can't use 0x108000, because mapelf will
         fail. */
#     if defined(VGP_mips64_linux)
      if (ebase < 0x100000)
         ebase = 0x100000;
#     else
      vg_assert(VKI_PAGE_SIZE >= 4096); /* stay sane */
      ESZ(Addr) hacky_load_address = 0x100000 + 8 * VKI_PAGE_SIZE;
      if (ebase < hacky_load_address)
         ebase = hacky_load_address;
#     endif

#     if defined(VGO_solaris)
      /* Record for later use in AT_BASE. */
      info->interp_offset = ebase;
#     endif
   }

   info->phnum = e->e.e_phnum;
   info->entry = e->e.e_entry + ebase;
   info->phdr = 0;
   info->stack_prot = VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC;

   for (i = 0; i < e->e.e_phnum; i++) {
      ESZ(Phdr) *ph = &e->p[i];

      switch(ph->p_type) {
      case PT_PHDR:
         info->phdr = ph->p_vaddr + ebase;
#        if defined(VGO_solaris)
         info->real_phdr_present = True;
#        endif
         break;

      case PT_LOAD:
         if (ph->p_vaddr < minaddr)
            minaddr = ph->p_vaddr;
         if (ph->p_vaddr+ph->p_memsz > maxaddr)
            maxaddr = ph->p_vaddr+ph->p_memsz;
         break;

#     if defined(VGO_solaris)
      case PT_SUNWDTRACE:
         if (ph->p_memsz < PT_SUNWDTRACE_SIZE) {
            VG_(printf)("valgrind: m_ume.c: too small SUNWDTRACE size\n");
            return VKI_ENOEXEC;
         }
         if ((ph->p_flags & (PF_R | PF_W | PF_X)) != PT_SUNWDTRACE_PROTECTION) {
            VG_(printf)("valgrind: m_ume.c: SUNWDTRACE protection mismatch\n");
            return VKI_ENOEXEC;
         }

         info->init_thrptr = ph->p_vaddr + ebase;
         break;
#     endif
                        
      case PT_INTERP: {
         HChar *buf = VG_(malloc)("ume.LE.1", ph->p_filesz+1);
         Int j;
         Int intfd;
         Int baseaddr_set;

         VG_(pread)(fd, buf, ph->p_filesz, ph->p_offset);
         buf[ph->p_filesz] = '\0';

#if defined(VGP_x86_freebsd)
         sres._isError = True;
         /* Hack.  FreeBSD's kernel overloads the interpreter name. */
         if (VG_(strcmp)(buf, "/libexec/ld-elf.so.1") == 0 ||
             VG_(strcmp)(buf, "/usr/libexec/ld-elf.so.1") == 0) {
            sres = VG_(open)("/libexec/ld-elf32.so.1", VKI_O_RDONLY, 0);
         }
         if (sr_isError(sres))
#endif
         sres = VG_(open)(buf, VKI_O_RDONLY, 0);
         //sres = VG_(open)("/usr/home/paulf/build/src/obj/usr/home/paulf/build/src/amd64.amd64/libexec/rtld-elf/ld-elf.so.1.full", VKI_O_RDONLY, 0);
         if (sr_isError(sres)) {
            VG_(printf)("valgrind: m_ume.c: can't open interpreter\n");
            VG_(exit)(1);
         }
         intfd = sr_Res(sres);

         interp = readelf(intfd, buf);
         if (interp == NULL) {
            VG_(printf)("valgrind: m_ume.c: can't read interpreter\n");
            return 1;
         }
         VG_(free)(buf);

         baseaddr_set = 0;
         for (j = 0; j < interp->e.e_phnum; j++) {
            ESZ(Phdr) *iph = &interp->p[j];
            ESZ(Addr) end;

#           if defined(VGO_solaris)
            if (iph->p_type == PT_SUNWDTRACE) {
               if (iph->p_memsz < PT_SUNWDTRACE_SIZE) {
                  VG_(printf)("valgrind: m_ume.c: too small SUNWDTRACE size\n");
                  return VKI_ENOEXEC;
               }
               if ((iph->p_flags & (PF_R | PF_W | PF_X))
                      != PT_SUNWDTRACE_PROTECTION) {
                  VG_(printf)("valgrind: m_ume.c: SUNWDTRACE protection "
                              "mismatch\n");
                  return VKI_ENOEXEC;
               }

               /* Store the thrptr value into a temporary because we do not
                  know yet where the interpreter is mapped. */
               thrptr_addr = iph->p_vaddr;
            }
#           endif

#           if defined(VGO_linux)
            if ((iph->p_type >= PT_LOPROC) && (iph->p_type <= PT_HIPROC)) {
               retval = arch_elf_pt_proc(&interp->e, iph, intfd, True,
                                         info->arch_elf_state);
               if (retval)
                 return retval;
            }
#           endif

            if (iph->p_type != PT_LOAD || iph->p_memsz == 0)
               continue;
            
            if (!baseaddr_set) {
               interp_addr  = iph->p_vaddr;
               /* interp_align = iph->p_align; */ /* UNUSED */
               baseaddr_set = 1;
            }

            /* assumes that all segments in the interp are close */
            end = (iph->p_vaddr - interp_addr) + iph->p_memsz;

            if (end > interp_size)
               interp_size = end;
         }
         break;
         }

#     if defined(PT_GNU_STACK) || defined(PT_SUNWSTACK)
#     if defined(PT_GNU_STACK)
      /* Android's elf.h doesn't appear to have PT_GNU_STACK. */
      case PT_GNU_STACK:
#     endif
#     if defined(PT_SUNWSTACK)
      /* Solaris-specific program header. */
      case PT_SUNWSTACK:
#     endif
         if ((ph->p_flags & PF_X) == 0) info->stack_prot &= ~VKI_PROT_EXEC;
         if ((ph->p_flags & PF_W) == 0) info->stack_prot &= ~VKI_PROT_WRITE;
         if ((ph->p_flags & PF_R) == 0) info->stack_prot &= ~VKI_PROT_READ;
         break;
#     endif

#     if defined(PT_SUNW_SYSSTAT)
      /* Solaris-specific program header which requires link-time support. */
      case PT_SUNW_SYSSTAT:
         VG_(unimplemented)("Support for program header PT_SUNW_SYSSTAT.");
         break;
#     endif
#     if defined(PT_SUNW_SYSSTAT_ZONE)
      /* Solaris-specific program header which requires link-time support. */
      case PT_SUNW_SYSSTAT_ZONE:
         VG_(unimplemented)("Support for program header PT_SUNW_SYSSTAT_ZONE.");
         break;
#     endif

#     if defined(VGO_linux)
      case PT_LOPROC ... PT_HIPROC:
         retval = arch_elf_pt_proc(&e->e, ph, fd, False, info->arch_elf_state);
         if (retval)
            return retval;
         break;
#     endif

      default:
         // do nothing
         break;
      }
   }

#  if defined(VGO_linux)
   retval = arch_check_elf(&e->e,
                           interp != NULL,
                           info->arch_elf_state);
   if (retval)
      return retval;
#  endif

   if (info->phdr == 0)
      info->phdr = minaddr + ebase + e->e.e_phoff;

   if (info->exe_base != info->exe_end) {
      if (minaddr >= maxaddr ||
          (minaddr + ebase < info->exe_base ||
           maxaddr + ebase > info->exe_end)) {
         VG_(printf)("Executable range %p-%p is outside the\n"
                     "acceptable range %p-%p\n",
                     (char *)minaddr + ebase, (char *)maxaddr + ebase,
                     (char *)info->exe_base,  (char *)info->exe_end);
         return VKI_ENOMEM;
      }
   }

   info->brkbase = mapelf(e, ebase);    /* map the executable */

   if (info->brkbase == 0)
      return VKI_ENOMEM;

   if (interp != NULL) {
      /* reserve a chunk of address space for interpreter */
      MapRequest mreq;
      Addr       advised;
      Bool       ok;

      /* Don't actually reserve the space.  Just get an advisory
         indicating where it would be allocated, and pass that to
         mapelf(), which in turn asks aspacem to do some fixed maps at
         the specified address.  This is a bit of hack, but it should
         work because there should be no intervening transactions with
         aspacem which could cause those fixed maps to fail.

         Placement policy is:

         if the interpreter asks to be loaded at zero
            ignore that and put it wherever we like (mappings at zero 
            are bad news)
         else
            try and put it where it asks for, but if that doesn't work,
            just put it anywhere.
      */
      if (interp_addr == 0) {
         mreq.rkind = MAny;
         mreq.start = 0;
         mreq.len   = interp_size;
      } else {
         mreq.rkind = MHint;
         mreq.start = interp_addr;
         mreq.len   = interp_size;
      }

      advised = VG_(am_get_advisory)( &mreq, True/*client*/, &ok );

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

      info->interp_offset = advised - interp_addr;
#     if defined(VGO_solaris)
      if (thrptr_addr)
         info->init_thrptr = thrptr_addr + info->interp_offset;
#     endif

      VG_(free)(interp->p);
      VG_(free)(interp);
   } else {
      entry = (void *)(ebase + e->e.e_entry);

#     if defined(VGO_solaris)
      if (e->e.e_type == ET_DYN)
         info->ldsoexec = True;
#     endif
   }

   info->exe_base = minaddr + ebase;
   info->exe_end  = maxaddr + ebase;

#if defined(VGP_ppc64be_linux)
   /* On PPC64BE, ELF ver 1, a func ptr is represented by a TOC entry ptr.
      This TOC entry contains three words; the first word is the function
      address, the second word is the TOC ptr (r2), and the third word
      is the static chain value. */
   info->init_ip  = ((ULong*)entry)[0];
   info->init_toc = ((ULong*)entry)[1];
   info->init_ip  += info->interp_offset;
   info->init_toc += info->interp_offset;
#elif defined(VGP_ppc64le_linux)
   /* On PPC64LE, ELF ver 2. API doesn't use a func ptr */
   info->init_ip  = (Addr)entry;
   info->init_toc = 0; /* meaningless on this platform */
#else
   info->init_ip  = (Addr)entry;
   info->init_toc = 0; /* meaningless on this platform */
#endif
   VG_(free)(e->p);
   VG_(free)(e);

   return 0;
}

#endif // defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
