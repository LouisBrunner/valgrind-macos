
/*--------------------------------------------------------------------*/
/*--- Dumping core.                                   m_coredump.c ---*/
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
#include "pub_core_coredump.h"
#include "pub_core_libcassert.h"

// Core dumping is disabled until someone can work out how to abstract out
// the arch-specific and word-size-specific parts neatly.
//
// Note that the code below is not 64-bit clean!
//
#if 0
/*
  Dump core
   
  Generate a standard ELF core file corresponding to the client state
  at the time of a crash.
 */
#include <elf.h>
#ifndef NT_PRXFPREG
#define NT_PRXFPREG     0x46e62b7f      /* copied from gdb5.1/include/elf/common.h */
#endif /* NT_PRXFPREG */

/* If true, then this Segment may be mentioned in the core */
static Bool may_dump(const Segment *seg)
{
   return (seg->flags & (SF_DEVICE|SF_VALGRIND)) == 0 && VG_(is_client_addr)(seg->addr);
}

/* If true, then this Segment's contents will be in the core */
static Bool should_dump(const Segment *seg)
{
   return may_dump(seg); // && (seg->prot & VKI_PROT_WRITE);
}

static void fill_ehdr(Elf32_Ehdr *ehdr, Int num_phdrs)
{
   VG_(memset)(ehdr, 0, sizeof(*ehdr));

   VG_(memcpy)(ehdr->e_ident, ELFMAG, SELFMAG);
   ehdr->e_ident[EI_CLASS]   = VG_ELF_CLASS;
   ehdr->e_ident[EI_DATA]    = VG_ELF_ENDIANNESS;
   ehdr->e_ident[EI_VERSION] = EV_CURRENT;

   ehdr->e_type = ET_CORE;
   ehdr->e_machine = VG_ELF_MACHINE;
   ehdr->e_version = EV_CURRENT;
   ehdr->e_entry = 0;
   ehdr->e_phoff = sizeof(Elf32_Ehdr);
   ehdr->e_shoff = 0;
   ehdr->e_flags = 0;
   ehdr->e_ehsize = sizeof(Elf32_Ehdr);
   ehdr->e_phentsize = sizeof(Elf32_Phdr);
   ehdr->e_phnum = num_phdrs;
   ehdr->e_shentsize = 0;
   ehdr->e_shnum = 0;
   ehdr->e_shstrndx = 0;

}

static void fill_phdr(Elf32_Phdr *phdr, const Segment *seg, UInt off, Bool write)
{
   write = write && should_dump(seg);

   VG_(memset)(phdr, 0, sizeof(*phdr));

   phdr->p_type = PT_LOAD;
   phdr->p_offset = off;
   phdr->p_vaddr = seg->addr;
   phdr->p_paddr = 0;
   phdr->p_filesz = write ? seg->len : 0;
   phdr->p_memsz = seg->len;
   phdr->p_flags = 0;

   if (seg->prot & VKI_PROT_READ)
      phdr->p_flags |= PF_R;
   if (seg->prot & VKI_PROT_WRITE)
      phdr->p_flags |= PF_W;
   if (seg->prot & VKI_PROT_EXEC)
      phdr->p_flags |= PF_X;

   phdr->p_align = VKI_PAGE_SIZE;
}

struct note {
   struct note *next;
   Elf32_Nhdr note;
   Char name[0];
};

static UInt note_size(const struct note *n)
{
   return sizeof(Elf32_Nhdr) + VG_ROUNDUP(VG_(strlen)(n->name)+1, 4) + VG_ROUNDUP(n->note.n_descsz, 4);
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
   Bool res;

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
   
   if (VG_(resolve_filename)(VG_(clexecfd), name, VKI_PATH_MAX)) {
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

   VG_(fill_elfregs_from_tst)(regs, &tst->arch);
}

static void fill_fpu(const ThreadState *tst, vki_elf_fpregset_t *fpu)
{
   VG_(fill_elffpregs_from_tst)(fpu, &tst->arch);
}

static void fill_xfpu(const ThreadState *tst, vki_elf_fpxregset_t *xfpu)
{
   VG_(fill_elffpxregs_from_tst)(xfpu, &tst->arch);
}

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   Char buf[1000];
   Char *basename = "vgcore";
   Char *coreext = "";
   Int seq = 0;
   Int core_fd;
   Segment *seg;
   Elf32_Ehdr ehdr;
   Elf32_Phdr *phdrs;
   Int num_phdrs;
   Int i, idx;
   UInt off;
   struct note *notelist, *note;
   UInt notesz;
   struct vki_elf_prpsinfo prpsinfo;
   struct vki_elf_prstatus prstatus;

   if (VG_(clo_log_name) != NULL) {
      coreext = ".core";
      basename = VG_(clo_log_name);
   }

   for(;;) {
      if (seq == 0)
	 VG_(sprintf)(buf, "%s%s.pid%d",
		      basename, coreext, VG_(getpid)());
      else
	 VG_(sprintf)(buf, "%s%s.pid%d.%d",
		      basename, coreext, VG_(getpid)(), seq);
      seq++;

      core_fd = VG_(open)(buf, 			   
			  VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
			  VKI_S_IRUSR|VKI_S_IWUSR);
      if (core_fd >= 0)
	 break;

      if (core_fd != -VKI_EEXIST)
	 return;		/* can't create file */
   }

   /* First, count how many memory segments to dump */
   num_phdrs = 1;		/* start with notes */
   for(seg = VG_(first_segment)();
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!may_dump(seg))
	 continue;

      num_phdrs++;
   }

   fill_ehdr(&ehdr, num_phdrs);

   notelist = NULL;

   /* Second, work out their layout */
   phdrs = VG_(arena_malloc)(VG_AR_CORE, sizeof(*phdrs) * num_phdrs);

   for(i = 1; i < VG_N_THREADS; i++) {
      vki_elf_fpregset_t  fpu;
      vki_elf_fpxregset_t xfpu;

      if (VG_(threads)[i].status == VgTs_Empty)
	 continue;

      fill_xfpu(&VG_(threads)[i], &xfpu);
      add_note(&notelist, "LINUX", NT_PRXFPREG, &xfpu, sizeof(xfpu));

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

   for(seg = VG_(first_segment)(), idx = 1;
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!may_dump(seg))
	 continue;

      fill_phdr(&phdrs[idx], seg, off, (seg->len + off) < max_size);
      
      off += phdrs[idx].p_filesz;

      idx++;
   }

   /* write everything out */
   VG_(write)(core_fd, &ehdr, sizeof(ehdr));
   VG_(write)(core_fd, phdrs, sizeof(*phdrs) * num_phdrs);

   for(note = notelist; note != NULL; note = note->next)
      write_note(core_fd, note);
   
   VG_(lseek)(core_fd, phdrs[1].p_offset, VKI_SEEK_SET);

   for(seg = VG_(first_segment)(), idx = 1;
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!should_dump(seg))
	 continue;

      if (phdrs[idx].p_filesz > 0) {
	 Int ret;

	 vg_assert(VG_(lseek)(core_fd, phdrs[idx].p_offset, VKI_SEEK_SET) == phdrs[idx].p_offset);
	 vg_assert(seg->len >= phdrs[idx].p_filesz);

	 ret = VG_(write)(core_fd, (void *)seg->addr, phdrs[idx].p_filesz);
      }
      idx++;
   }

   VG_(close)(core_fd);
}
#endif

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   I_die_here;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
