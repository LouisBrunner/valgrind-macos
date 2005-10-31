
/*--------------------------------------------------------------------*/
/*--- Dumping core.                                 coredump-elf.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_machine.h"
#include "pub_core_coredump.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcfile.h"    // VG_(close) et al
#include "pub_core_libcproc.h"    // VG_(geteuid), VG_(getegid)
#include "pub_core_libcassert.h"  // VG_(exit), vg_assert
#include "pub_core_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_core_threadstate.h"
#include "pub_core_clientstate.h"
#include "pub_core_options.h"

#include "priv_elf.h"

/*
  Dump core
   
  Generate a standard ELF core file corresponding to the client state
  at the time of a crash.
 */
#include <elf.h>
#ifndef NT_PRXFPREG
#define NT_PRXFPREG     0x46e62b7f      /* copied from gdb5.1/include/elf/common.h */
#endif /* NT_PRXFPREG */

#if	VG_WORDSIZE == 8
#define ESZ(x)	Elf64_##x
#elif	VG_WORDSIZE == 4
#define ESZ(x)	Elf32_##x
#else
#error VG_WORDSIZE needs to ==4 or ==8
#endif

/* TODO: GIVE THIS A PROPER HOME
   TODO: MERGE THIS WITH DUPLICATES IN m_main.c and mac_leakcheck.c
   Extract from aspacem a vector of the current segment start
   addresses.  The vector is dynamically allocated and should be freed
   by the caller when done.  REQUIRES m_mallocfree to be running.
   Writes the number of addresses required into *n_acquired. */

static Addr* get_seg_starts ( /*OUT*/Int* n_acquired )
{
   Addr* starts;
   Int   n_starts, r = 0;

   n_starts = 1;
   while (True) {
      starts = VG_(malloc)( n_starts * sizeof(Addr) );
      if (starts == NULL)
         break;
      r = VG_(am_get_segment_starts)( starts, n_starts );
      if (r >= 0)
         break;
      VG_(free)(starts);
      n_starts *= 2;
   }

   if (starts == NULL) {
     *n_acquired = 0;
     return NULL;
   }

   *n_acquired = r;
   return starts;
}

/* If true, then this Segment may be mentioned in the core */
static Bool may_dump(const NSegment *seg)
{
   if (seg->kind == SkAnonC ||
       seg->kind == SkShmC ||
       (seg->kind == SkFileC &&
        !VKI_S_ISCHR(seg->mode) && !VKI_S_ISBLK(seg->mode)))
      return True;

   return False;
}

/* If true, then this Segment's contents will be in the core */
static Bool should_dump(const NSegment *seg)
{
   return may_dump(seg); // && seg->hasW;
}

static void fill_ehdr(ESZ(Ehdr) *ehdr, Int num_phdrs)
{
   VG_(memset)(ehdr, 0, sizeof(*ehdr));

   VG_(memcpy)(ehdr->e_ident, ELFMAG, SELFMAG);
   ehdr->e_ident[EI_CLASS]   = VG_ELF_CLASS;
   ehdr->e_ident[EI_DATA]    = VG_ELF_DATA2XXX;
   ehdr->e_ident[EI_VERSION] = EV_CURRENT;

   ehdr->e_type = ET_CORE;
   ehdr->e_machine = VG_ELF_MACHINE;
   ehdr->e_version = EV_CURRENT;
   ehdr->e_entry = 0;
   ehdr->e_phoff = sizeof(ESZ(Ehdr));
   ehdr->e_shoff = 0;
   ehdr->e_flags = 0;
   ehdr->e_ehsize = sizeof(ESZ(Ehdr));
   ehdr->e_phentsize = sizeof(ESZ(Phdr));
   ehdr->e_phnum = num_phdrs;
   ehdr->e_shentsize = 0;
   ehdr->e_shnum = 0;
   ehdr->e_shstrndx = 0;

}

static void fill_phdr(ESZ(Phdr) *phdr, const NSegment *seg, UInt off, Bool write)
{
   SizeT len = seg->end - seg->start;

   write = write && should_dump(seg);

   VG_(memset)(phdr, 0, sizeof(*phdr));

   phdr->p_type = PT_LOAD;
   phdr->p_offset = off;
   phdr->p_vaddr = seg->start;
   phdr->p_paddr = 0;
   phdr->p_filesz = write ? len : 0;
   phdr->p_memsz = len;
   phdr->p_flags = 0;

   if (seg->hasR)
      phdr->p_flags |= PF_R;
   if (seg->hasW)
      phdr->p_flags |= PF_W;
   if (seg->hasX)
      phdr->p_flags |= PF_X;

   phdr->p_align = VKI_PAGE_SIZE;
}

struct note {
   struct note *next;
   ESZ(Nhdr) note;
   Char name[0];
};

static UInt note_size(const struct note *n)
{
   return sizeof(ESZ(Nhdr)) + VG_ROUNDUP(VG_(strlen)(n->name)+1, 4) + VG_ROUNDUP(n->note.n_descsz, 4);
}

static void add_note(struct note **list, const Char *name, UInt type, const void *data, UInt datasz)
{
   Int namelen = VG_(strlen)(name)+1;
   Int notelen = sizeof(struct note) + 
      VG_ROUNDUP(namelen, 4) + 
      VG_ROUNDUP(datasz, 4);
   struct note *n = VG_(arena_malloc)(VG_AR_CORE, notelen);

   VG_(memset)(n, 0, notelen);

   n->next = *list;
   *list = n;

   n->note.n_type = type;
   n->note.n_namesz = namelen;
   n->note.n_descsz = datasz;

   VG_(memcpy)(n->name, name, namelen);
   VG_(memcpy)(n->name+VG_ROUNDUP(namelen,4), data, datasz);
}

static void write_note(Int fd, const struct note *n)
{
   VG_(write)(fd, &n->note, note_size(n));
}

static void fill_prpsinfo(const ThreadState *tst, struct vki_elf_prpsinfo *prpsinfo)
{
   static Char name[VKI_PATH_MAX];

   VG_(memset)(prpsinfo, 0, sizeof(*prpsinfo));

   switch(tst->status) {
   case VgTs_Runnable:
   case VgTs_Yielding:
      prpsinfo->pr_sname = 'R';
      break;

   case VgTs_WaitSys:
      prpsinfo->pr_sname = 'S';
      break;

   case VgTs_Zombie:
      prpsinfo->pr_sname = 'Z';
      break;

   case VgTs_Empty:
   case VgTs_Init:
      prpsinfo->pr_sname = '?';
      break;
   }

   prpsinfo->pr_uid = 0;
   prpsinfo->pr_gid = 0;
   
   if (VG_(resolve_filename)(VG_(cl_exec_fd), name, VKI_PATH_MAX)) {
      Char *n = name+VG_(strlen)(name)-1;

      while (n > name && *n != '/')
	 n--;
      if (n != name)
	 n++;

      VG_(strncpy)(prpsinfo->pr_fname, n, sizeof(prpsinfo->pr_fname));
   }
}

static void fill_prstatus(const ThreadState *tst, 
			  struct vki_elf_prstatus *prs, 
			  const vki_siginfo_t *si)
{
   struct vki_user_regs_struct *regs;

   VG_(memset)(prs, 0, sizeof(*prs));

   prs->pr_info.si_signo = si->si_signo;
   prs->pr_info.si_code = si->si_code;
   prs->pr_info.si_errno = 0;

   prs->pr_cursig = si->si_signo;

   prs->pr_pid = tst->os_state.lwpid;
   prs->pr_ppid = 0;
   prs->pr_pgrp = VG_(getpgrp)();
   prs->pr_sid = VG_(getpgrp)();
   
   regs = (struct vki_user_regs_struct *)prs->pr_reg;

   vg_assert(sizeof(*regs) == sizeof(prs->pr_reg));

   ML_(fill_elfregs_from_tst)(regs, &tst->arch);
}

static void fill_fpu(const ThreadState *tst, vki_elf_fpregset_t *fpu)
{
   ML_(fill_elffpregs_from_tst)(fpu, &tst->arch);
}

#if defined(VGP_x86_linux)
static void fill_xfpu(const ThreadState *tst, vki_elf_fpxregset_t *xfpu)
{
   ML_(fill_elffpxregs_from_tst)(xfpu, &tst->arch);
}
#endif

void ML_(make_elf_coredump)(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   Char buf[1000];
   Char *basename = "vgcore";
   Char *coreext = "";
   Int seq = 0;
   Int core_fd;
   NSegment *seg;
   ESZ(Ehdr) ehdr;
   ESZ(Phdr) *phdrs;
   Int num_phdrs;
   Int i, idx;
   UInt off;
   struct note *notelist, *note;
   UInt notesz;
   struct vki_elf_prpsinfo prpsinfo;
   struct vki_elf_prstatus prstatus;
   Addr *seg_starts;
   Int n_seg_starts;

   if (VG_(clo_log_name) != NULL) {
      coreext = ".core";
      basename = VG_(clo_log_name);
   }

   for(;;) {
      SysRes sres;

      if (seq == 0)
	 VG_(sprintf)(buf, "%s%s.pid%d",
		      basename, coreext, VG_(getpid)());
      else
	 VG_(sprintf)(buf, "%s%s.pid%d.%d",
		      basename, coreext, VG_(getpid)(), seq);
      seq++;

      sres = VG_(open)(buf, 			   
                       VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
                       VKI_S_IRUSR|VKI_S_IWUSR);
      if (!sres.isError) {
         core_fd = sres.val;
	 break;
      }

      if (sres.isError && sres.val != VKI_EEXIST)
	 return;		/* can't create file */
   }

   /* Get the segments */
   seg_starts = get_seg_starts(&n_seg_starts);

   /* First, count how many memory segments to dump */
   num_phdrs = 1;		/* start with notes */
   for(i = 0; i < n_seg_starts; i++) {
      if (!may_dump(VG_(am_find_nsegment(seg_starts[i]))))
	 continue;

      num_phdrs++;
   }

   fill_ehdr(&ehdr, num_phdrs);

   notelist = NULL;

   /* Second, work out their layout */
   phdrs = VG_(arena_malloc)(VG_AR_CORE, sizeof(*phdrs) * num_phdrs);

   for(i = 1; i < VG_N_THREADS; i++) {
      vki_elf_fpregset_t  fpu;
#if defined(VGP_x86_linux)
      vki_elf_fpxregset_t xfpu;
#endif

      if (VG_(threads)[i].status == VgTs_Empty)
	 continue;

#if defined(VGP_x86_linux)
      fill_xfpu(&VG_(threads)[i], &xfpu);
      add_note(&notelist, "LINUX", NT_PRXFPREG, &xfpu, sizeof(xfpu));
#endif

      fill_fpu(&VG_(threads)[i], &fpu);
      add_note(&notelist, "CORE", NT_FPREGSET, &fpu, sizeof(fpu));

      fill_prstatus(&VG_(threads)[i], &prstatus, si);
      add_note(&notelist, "CORE", NT_PRSTATUS, &prstatus, sizeof(prstatus));
   }

   fill_prpsinfo(&VG_(threads)[tid], &prpsinfo);
   add_note(&notelist, "CORE", NT_PRPSINFO, &prpsinfo, sizeof(prpsinfo));

   for(note = notelist, notesz = 0; note != NULL; note = note->next)
      notesz += note_size(note);

   off = sizeof(ehdr) + sizeof(*phdrs) * num_phdrs;

   phdrs[0].p_type = PT_NOTE;
   phdrs[0].p_offset = off;
   phdrs[0].p_vaddr = 0;
   phdrs[0].p_paddr = 0;
   phdrs[0].p_filesz = notesz;
   phdrs[0].p_memsz = 0;
   phdrs[0].p_flags = 0;
   phdrs[0].p_align = 0;

   off += notesz;

   off = VG_PGROUNDUP(off);

   for(i = 0, idx = 1; i < n_seg_starts; i++) {
      seg = VG_(am_find_nsegment(seg_starts[i]));

      if (!may_dump(seg))
	 continue;

      fill_phdr(&phdrs[idx], seg, off, (seg->end - seg->start + off) < max_size);
      
      off += phdrs[idx].p_filesz;

      idx++;
   }

   /* write everything out */
   VG_(write)(core_fd, &ehdr, sizeof(ehdr));
   VG_(write)(core_fd, phdrs, sizeof(*phdrs) * num_phdrs);

   for(note = notelist; note != NULL; note = note->next)
      write_note(core_fd, note);
   
   VG_(lseek)(core_fd, phdrs[1].p_offset, VKI_SEEK_SET);

   for(i = 0, idx = 1; i < n_seg_starts; i++) {
      seg = VG_(am_find_nsegment(seg_starts[i]));

      if (!should_dump(seg))
	 continue;

      if (phdrs[idx].p_filesz > 0) {
	 Int ret;

	 vg_assert(VG_(lseek)(core_fd, phdrs[idx].p_offset, VKI_SEEK_SET) == phdrs[idx].p_offset);
	 vg_assert(seg->end - seg->start >= phdrs[idx].p_filesz);

	 ret = VG_(write)(core_fd, (void *)seg->start, phdrs[idx].p_filesz);
      }
      idx++;
   }

   VG_(free)(seg_starts);

   VG_(close)(core_fd);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
