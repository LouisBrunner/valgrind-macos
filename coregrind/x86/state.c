
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                    x86/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "core.h"
#include "x86_private.h"
#include <sys/ptrace.h>

#include "libvex_guest_x86.h"

/* This is set early to indicate whether this CPU has the
   SSE/fxsave/fxrestor features.  */
Bool VG_(have_ssestate);


/* Global and Local descriptor tables for threads.

   See comments in libvex_guest_x86.h for LibVEX's model of x86
   segment descriptors.

   Mostly, threads never generate LDT (or GDT?) entries.  Therefore,
   we will initially start off with LDTs and GDTs being (HWord)NULL
   and allocate them on demand.
*/


/*------------------------------------------------------------*/
/*--- Initialising the first thread                        ---*/
/*------------------------------------------------------------*/

/* Given a pointer to the ThreadArchState for thread 1 (the root
   thread), initialise the VEX guest state, and copy in essential
   starting values.
*/
void VGA_(init_thread1state) ( Addr client_eip, 
                               Addr esp_at_startup,
			       /*MOD*/ ThreadArchState* arch )
{
   vg_assert(0 == sizeof(VexGuestX86State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestX86_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */

   arch->vex.guest_ESP = esp_at_startup;
   arch->vex.guest_EIP = client_eip;

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0"
                :
                : "m" (arch->vex.guest_CS));
   asm volatile("movw %%ds, %0"
                :
                : "m" (arch->vex.guest_DS));
   asm volatile("movw %%ss, %0"
                :
                : "m" (arch->vex.guest_SS));

   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));

   /* I assume that if we have SSE2 we also have SSE */
   VG_(have_ssestate) = False;
   //      VG_(cpu_has_feature)(VG_X86_FEAT_FXSR) &&
   //   VG_(cpu_has_feature)(VG_X86_FEAT_SSE);

   if (0) {
      if (VG_(have_ssestate))
         VG_(printf)("Looks like a SSE-capable CPU\n");
      else
         VG_(printf)("Looks like a MMX-only CPU\n");
   }
}


/*------------------------------------------------------------*/
/*--- LDT/GDT stuff                                        ---*/
/*------------------------------------------------------------*/

/* Create a zeroed-out GDT. */

static VexGuestX86SegDescr* alloc_zeroed_GDT ( void )
{
   Int nbytes 
      = VEX_GUEST_X86_GDT_NENT * sizeof(VexGuestX86SegDescr);
   return
      VG_(arena_calloc)(VG_AR_CORE, VG_MIN_MALLOC_SZB, nbytes, 1);
}

/* Create a zeroed-out LDT. */

static VexGuestX86SegDescr* alloc_zeroed_LDT ( void )
{
   Int nbytes 
      = VEX_GUEST_X86_LDT_NENT * sizeof(VexGuestX86SegDescr);
   return
      VG_(arena_calloc)(VG_AR_CORE, VG_MIN_MALLOC_SZB, nbytes, 1);
}

/* Free up an LDT or GDT allocated by the above fns. */

static void free_LDT_or_GDT ( VexGuestX86SegDescr* dt )
{
   vg_assert(dt);
   VG_(arena_free)(VG_AR_CORE, (void*)dt);
}

/* Copy contents between two existing LDTs. */

static void copy_LDT_from_to ( VexGuestX86SegDescr* src,
                               VexGuestX86SegDescr* dst )
{
  Int i;
  vg_assert(src);
  vg_assert(dst);
  for (i = 0; i < VEX_GUEST_X86_LDT_NENT; i++)
     dst[i] = src[i];
}

/* Free this thread's DTs, if it has any. */

static void deallocate_LGDTs_for_thread ( VexGuestX86State* vex )
{
   vg_assert(sizeof(HWord) == sizeof(void*));

   if (0)
      VG_(printf)("deallocate_LGDTs_for_thread: "
                  "ldt = 0x%x, gdt = 0x%x\n", 
                  vex->guest_LDT, vex->guest_GDT );

   if (vex->guest_LDT != (HWord)NULL) {
      free_LDT_or_GDT( (VexGuestX86SegDescr*)vex->guest_LDT );
      vex->guest_LDT = (HWord)NULL;
   }

   if (vex->guest_GDT != (HWord)NULL) {
      free_LDT_or_GDT( (VexGuestX86SegDescr*)vex->guest_GDT );
      vex->guest_GDT = (HWord)NULL;
   }
}


/* Translate a struct modify_ldt_ldt_s to a VexGuestX86SegDescr, using
   the Linux kernel's logic (cut-n-paste of code in
   linux/kernel/ldt.c).  */

static
void translate_to_hw_format ( /* IN  */ vki_modify_ldt_t* inn,
			      /* OUT */ VexGuestX86SegDescr* out,
                                        Int oldmode )
{
   UInt entry_1, entry_2;
   vg_assert(8 == sizeof(VexGuestX86SegDescr));

   if (0)
      VG_(printf)("translate_to_hw_format: base %p, limit %d\n", 
                  inn->base_addr, inn->limit );

   /* Allow LDTs to be cleared by the user. */
   if (inn->base_addr == 0 && inn->limit == 0) {
      if (oldmode ||
          (inn->contents == 0      &&
           inn->read_exec_only == 1   &&
           inn->seg_32bit == 0      &&
           inn->limit_in_pages == 0   &&
           inn->seg_not_present == 1   &&
           inn->useable == 0 )) {
         entry_1 = 0;
         entry_2 = 0;
         goto install;
      }
   }

   entry_1 = ((inn->base_addr & 0x0000ffff) << 16) |
             (inn->limit & 0x0ffff);
   entry_2 = (inn->base_addr & 0xff000000) |
             ((inn->base_addr & 0x00ff0000) >> 16) |
             (inn->limit & 0xf0000) |
             ((inn->read_exec_only ^ 1) << 9) |
             (inn->contents << 10) |
             ((inn->seg_not_present ^ 1) << 15) |
             (inn->seg_32bit << 22) |
             (inn->limit_in_pages << 23) |
             0x7000;
   if (!oldmode)
      entry_2 |= (inn->useable << 20);

   /* Install the new entry ...  */
  install:
   out->LdtEnt.Words.word1 = entry_1;
   out->LdtEnt.Words.word2 = entry_2;
}


/*
 * linux/kernel/ldt.c
 *
 * Copyright (C) 1992 Krishna Balasubramanian and Linus Torvalds
 * Copyright (C) 1999 Ingo Molnar <mingo@redhat.com>
 */

/*
 * read_ldt() is not really atomic - this is not a problem since
 * synchronization of reads and writes done to the LDT has to be
 * assured by user-space anyway. Writes are atomic, to protect
 * the security checks done on new descriptors.
 */
static
Int read_ldt ( ThreadId tid, UChar* ptr, UInt bytecount )
{
   Int    err;
   UInt   i, size;
   UChar* ldt;

   if (0)
      VG_(printf)("read_ldt: tid = %d, ptr = %p, bytecount = %d\n",
                  tid, ptr, bytecount );

   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));
   vg_assert(8 == sizeof(VexGuestX86SegDescr));

   ldt = (Char*)(VG_(threads)[tid].arch.vex.guest_LDT);
   err = 0;
   if (ldt == NULL)
      /* LDT not allocated, meaning all entries are null */
      goto out;

   size = VEX_GUEST_X86_LDT_NENT * sizeof(VexGuestX86SegDescr);
   if (size > bytecount)
      size = bytecount;

   err = size;
   for (i = 0; i < size; i++)
      ptr[i] = ldt[i];

  out:
   return err;
}


static
Int write_ldt ( ThreadId tid, void* ptr, UInt bytecount, Int oldmode )
{
   Int error;
   VexGuestX86SegDescr* ldt;
   vki_modify_ldt_t* ldt_info; 

   if (0)
      VG_(printf)("write_ldt: tid = %d, ptr = %p, "
                  "bytecount = %d, oldmode = %d\n",
                  tid, ptr, bytecount, oldmode );

   vg_assert(8 == sizeof(VexGuestX86SegDescr));
   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));

   ldt      = (VexGuestX86SegDescr*)VG_(threads)[tid].arch.vex.guest_LDT;
   ldt_info = (vki_modify_ldt_t*)ptr;

   error = -VKI_EINVAL;
   if (bytecount != sizeof(vki_modify_ldt_t))
      goto out;

   error = -VKI_EINVAL;
   if (ldt_info->entry_number >= VEX_GUEST_X86_LDT_NENT)
      goto out;
   if (ldt_info->contents == 3) {
      if (oldmode)
         goto out;
      if (ldt_info->seg_not_present == 0)
         goto out;
   }

   /* If this thread doesn't have an LDT, we'd better allocate it
      now. */
   if (ldt == (HWord)NULL) {
      ldt = alloc_zeroed_LDT();
      VG_(threads)[tid].arch.vex.guest_LDT = (HWord)ldt;
   }

   /* Install the new entry ...  */
   translate_to_hw_format ( ldt_info, &ldt[ldt_info->entry_number], oldmode );
   error = 0;

  out:
   return error;
}


Int VG_(sys_modify_ldt) ( ThreadId tid,
                          Int func, void* ptr, UInt bytecount )
{
   Int ret = -VKI_ENOSYS;

   switch (func) {
   case 0:
      ret = read_ldt(tid, ptr, bytecount);
      break;
   case 1:
      ret = write_ldt(tid, ptr, bytecount, 1);
      break;
   case 2:
      VG_(unimplemented)("sys_modify_ldt: func == 2");
      /* god knows what this is about */
      /* ret = read_default_ldt(ptr, bytecount); */
      /*UNREACHED*/
      break;
   case 0x11:
      ret = write_ldt(tid, ptr, bytecount, 0);
      break;
   }
   return ret;
}


Int VG_(sys_set_thread_area) ( ThreadId tid,
                               vki_modify_ldt_t* info )
{
   Int idx;
   VexGuestX86SegDescr* gdt;

   vg_assert(8 == sizeof(VexGuestX86SegDescr));
   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));

   if (info == NULL)
      return -VKI_EFAULT;

   gdt = (VexGuestX86SegDescr*)VG_(threads)[tid].arch.vex.guest_GDT;

   /* If the thread doesn't have a GDT, allocate it now. */
   if (!gdt) {
      gdt = alloc_zeroed_GDT();
      VG_(threads)[tid].arch.vex.guest_GDT = (HWord)gdt;
   }

   idx = info->entry_number;

   if (idx == -1) {
      /* Find and use the first free entry. */
      for (idx = 0; idx < VEX_GUEST_X86_GDT_NENT; idx++) {
         if (gdt[idx].LdtEnt.Words.word1 == 0 
             && gdt[idx].LdtEnt.Words.word2 == 0)
            break;
      }

      if (idx == VEX_GUEST_X86_GDT_NENT)
         return -VKI_ESRCH;
   } else if (idx < 0 || idx >= VEX_GUEST_X86_GDT_NENT) {
      return -VKI_EINVAL;
   }

   translate_to_hw_format(info, &gdt[idx], 0);

   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid,
             "set_thread_area(info->entry)",
             (Addr) & info->entry_number, sizeof(unsigned int) );
   info->entry_number = idx;
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid,
             (Addr) & info->entry_number, sizeof(unsigned int) );

   return 0;
}


Int VG_(sys_get_thread_area) ( ThreadId tid,
                               vki_modify_ldt_t* info )
{
   Int idx;
   VexGuestX86SegDescr* gdt;

   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));
   vg_assert(8 == sizeof(VexGuestX86SegDescr));

   if (info == NULL)
      return -VKI_EFAULT;

   idx = info->entry_number;

   if (idx < 0 || idx >= VEX_GUEST_X86_GDT_NENT)
      return -VKI_EINVAL;

   gdt = (VexGuestX86SegDescr*)VG_(threads)[tid].arch.vex.guest_GDT;

   /* If the thread doesn't have a GDT, allocate it now. */
   if (!gdt) {
      gdt = alloc_zeroed_GDT();
      VG_(threads)[tid].arch.vex.guest_GDT = (HWord)gdt;
   }

   info->base_addr = ( gdt[idx].LdtEnt.Bits.BaseHi << 24 ) |
                     ( gdt[idx].LdtEnt.Bits.BaseMid << 16 ) |
                     gdt[idx].LdtEnt.Bits.BaseLow;
   info->limit = ( gdt[idx].LdtEnt.Bits.LimitHi << 16 ) |
                   gdt[idx].LdtEnt.Bits.LimitLow;
   info->seg_32bit = gdt[idx].LdtEnt.Bits.Default_Big;
   info->contents = ( gdt[idx].LdtEnt.Bits.Type >> 2 ) & 0x3;
   info->read_exec_only = ( gdt[idx].LdtEnt.Bits.Type & 0x1 ) ^ 0x1;
   info->limit_in_pages = gdt[idx].LdtEnt.Bits.Granularity;
   info->seg_not_present = gdt[idx].LdtEnt.Bits.Pres ^ 0x1;
   info->useable = gdt[idx].LdtEnt.Bits.Sys;
   info->reserved = 0;

   return 0;
}


/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(cleanup_thread) ( ThreadArchState* arch )
{
   /* Release arch-specific resources held by this thread. */
   /* On x86, we have to dump the LDT and GDT. */
   deallocate_LGDTs_for_thread( &arch->vex );
}  


void VGA_(setup_child) ( /*OUT*/ ThreadArchState *child, 
                         /*IN*/  ThreadArchState *parent )
{
   /* We inherit our parent's LDT. */
   if (parent->vex.guest_LDT == (HWord)NULL) {
      /* We hope this is the common case. */
      child->vex.guest_LDT = (HWord)NULL;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      child->vex.guest_LDT = (HWord)alloc_zeroed_LDT();
      copy_LDT_from_to( (VexGuestX86SegDescr*)parent->vex.guest_LDT, 
                        (VexGuestX86SegDescr*)child->vex.guest_LDT );
   }

   /* We need an empty GDT. */
   child->vex.guest_GDT = (HWord)NULL;
}  


void VGA_(set_arg_and_bogus_ret)( ThreadId tid, UWord arg, Addr ret )
{
   /* Push the arg, and mark it as readable. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = arg;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
             VG_(threads)[tid].arch.vex.guest_ESP, sizeof(void*) );

   /* Don't mark the pushed return address as readable; any attempt to read
      this is an internal valgrind bug since thread_exit_wrapper() should not
      return. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = ret;
}


void VGA_(thread_initial_stack)(ThreadId tid, UWord arg, Addr ret)
{
   Addr esp = (Addr)STACK_PTR(VG_(threads)[tid].arch);

   /* push two args */
   esp -= 2 * sizeof(UWord);
   SET_PTHREQ_ESP(tid, esp);
   
   VG_TRACK ( new_mem_stack, esp, 2 * sizeof(UWord) );
   VG_TRACK ( pre_mem_write, Vg_CorePThread, tid, "new thread: stack",
                             esp, 2 * sizeof(UWord) );

   /* push arg and (bogus) return address */
   *(UWord*)(esp+sizeof(UWord)) = arg;
   *(UWord*)(esp)               = ret;

   VG_TRACK ( post_mem_write, Vg_CoreSignal, tid, esp, 2 * sizeof(UWord) );
}

/*------------------------------------------------------------*/
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

/* This is the Intel register encoding -- integer regs. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7

UInt *VGA_(reg_addr_from_tst)(Int regno, ThreadArchState *arch)
{
   switch (regno) {
   case R_EAX: return &arch->vex.guest_EAX;
   case R_ECX: return &arch->vex.guest_ECX;
   case R_EDX: return &arch->vex.guest_EDX;
   case R_EBX: return &arch->vex.guest_EBX;
   case R_ESP: return &arch->vex.guest_ESP;
   case R_EBP: return &arch->vex.guest_EBP;
   case R_ESI: return &arch->vex.guest_ESI;
   case R_EDI: return &arch->vex.guest_EDI;
   default:    return NULL;
   }
}

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(void)
{
   vki_modify_ldt_t ldt = { 
      VG_POINTERCHECK_SEGIDX,    // entry_number
      VG_(client_base),          // base_addr
      (VG_(client_end)-VG_(client_base)) / VKI_PAGE_SIZE, // limit
      1,                         // seg_32bit
      0,                         // contents: data, RW, non-expanding
      0,                         // ! read_exec_only
      1,                         // limit_in_pages
      0,                         // ! seg not present
      1,                         // useable
   };
   int ret = VG_(do_syscall3)(__NR_modify_ldt, 1, (UWord)&ldt, sizeof(ldt));
   if (ret < 0) {
      VG_(message)(Vg_UserMsg,
                   "Warning: ignoring --pointercheck=yes, "
                   "because modify_ldt failed (errno=%d)", -ret);
      return False;
   } else {
      return True;
   }
}

/*------------------------------------------------------------*/
/*--- Debugger-related operations                          ---*/
/*------------------------------------------------------------*/

Int VGA_(ptrace_setregs_from_tst)(Int pid, ThreadArchState* arch)
{
   struct vki_user_regs_struct regs;

   regs.cs     = arch->vex.guest_CS;
   regs.ss     = arch->vex.guest_SS;
   regs.ds     = arch->vex.guest_DS;
   regs.es     = arch->vex.guest_ES;
   regs.fs     = arch->vex.guest_FS;
   regs.gs     = arch->vex.guest_GS;
   regs.eax    = arch->vex.guest_EAX;
   regs.ebx    = arch->vex.guest_EBX;
   regs.ecx    = arch->vex.guest_ECX;
   regs.edx    = arch->vex.guest_EDX;
   regs.esi    = arch->vex.guest_ESI;
   regs.edi    = arch->vex.guest_EDI;
   regs.ebp    = arch->vex.guest_EBP;
   regs.esp    = arch->vex.guest_ESP;
   regs.eflags = LibVEX_GuestX86_get_eflags(&arch->vex);
   regs.eip    = arch->vex.guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

