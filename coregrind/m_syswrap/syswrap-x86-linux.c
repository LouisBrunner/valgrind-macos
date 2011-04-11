
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.        syswrap-x86-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Nicholas Nethercote
      njn@valgrind.org

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

#if defined(VGP_x86_linux)

/* TODO/FIXME jrs 20050207: assignments to the syscall return result
   in interrupted_syscall() need to be reviewed.  They don't seem
   to assign the shadow state.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"    /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"      /* for decls of linux-ish wrappers */
#include "priv_syswrap-linux-variants.h" /* decls of linux variant wrappers */
#include "priv_syswrap-main.h"


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
			          Addr retaddr,
			          void (*f)(Word),
                                  Word arg1 );
//  4(%esp) == stack
//  8(%esp) == retaddr
// 12(%esp) == f
// 16(%esp) == arg1
asm(
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   movl %esp, %esi\n"     // remember old stack pointer
"   movl 4(%esi), %esp\n"  // set stack
"   pushl 16(%esi)\n"      // arg1 to stack
"   pushl  8(%esi)\n"      // retaddr to stack
"   pushl 12(%esi)\n"      // f to stack
"   movl $0, %eax\n"       // zero all GP regs
"   movl $0, %ebx\n"
"   movl $0, %ecx\n"
"   movl $0, %edx\n"
"   movl $0, %esi\n"
"   movl $0, %edi\n"
"   movl $0, %ebp\n"
"   ret\n"                 // jump to f
"   ud2\n"                 // should never get here
".previous\n"
);


/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

        Upon entry, we have:

            int (fn)(void*)     in  0+FSZ(%esp)
            void* child_stack   in  4+FSZ(%esp)
            int flags           in  8+FSZ(%esp)
            void* arg           in 12+FSZ(%esp)
            pid_t* child_tid    in 16+FSZ(%esp)
            pid_t* parent_tid   in 20+FSZ(%esp)
            void* tls_ptr       in 24+FSZ(%esp)

        System call requires:

            int    $__NR_clone  in %eax
            int    flags        in %ebx
            void*  child_stack  in %ecx
            pid_t* parent_tid   in %edx
            pid_t* child_tid    in %edi
            void*  tls_ptr      in %esi

	Returns an Int encoded in the linux-x86 way, not a SysRes.
 */
#define FSZ               "4+4+4+4" /* frame size = retaddr+ebx+edi+esi */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
Int do_syscall_clone_x86_linux ( Word (*fn)(void *), 
                                 void* stack, 
                                 Int   flags, 
                                 void* arg,
                                 Int*  child_tid, 
                                 Int*  parent_tid, 
                                 vki_modify_ldt_t * );
asm(
".text\n"
"do_syscall_clone_x86_linux:\n"
"        push    %ebx\n"
"        push    %edi\n"
"        push    %esi\n"

         /* set up child stack with function and arg */
"        movl     4+"FSZ"(%esp), %ecx\n"    /* syscall arg2: child stack */
"        movl    12+"FSZ"(%esp), %ebx\n"    /* fn arg */
"        movl     0+"FSZ"(%esp), %eax\n"    /* fn */
"        lea     -8(%ecx), %ecx\n"          /* make space on stack */
"        movl    %ebx, 4(%ecx)\n"           /*   fn arg */
"        movl    %eax, 0(%ecx)\n"           /*   fn */

         /* get other args to clone */
"        movl     8+"FSZ"(%esp), %ebx\n"    /* syscall arg1: flags */
"        movl    20+"FSZ"(%esp), %edx\n"    /* syscall arg3: parent tid * */
"        movl    16+"FSZ"(%esp), %edi\n"    /* syscall arg5: child tid * */
"        movl    24+"FSZ"(%esp), %esi\n"    /* syscall arg4: tls_ptr * */
"        movl    $"__NR_CLONE", %eax\n"
"        int     $0x80\n"                   /* clone() */
"        testl   %eax, %eax\n"              /* child if retval == 0 */
"        jnz     1f\n"

         /* CHILD - call thread function */
"        popl    %eax\n"
"        call    *%eax\n"                   /* call fn */

         /* exit with result */
"        movl    %eax, %ebx\n"              /* arg1: return value from fn */
"        movl    $"__NR_EXIT", %eax\n"
"        int     $0x80\n"

         /* Hm, exit returned */
"        ud2\n"

"1:\n"   /* PARENT or ERROR */
"        pop     %esi\n"
"        pop     %edi\n"
"        pop     %ebx\n"
"        ret\n"
".previous\n"
);

#undef FSZ
#undef __NR_CLONE
#undef __NR_EXIT


// forward declarations
static void setup_child ( ThreadArchState*, ThreadArchState*, Bool );
static SysRes sys_set_thread_area ( ThreadId, vki_modify_ldt_t* );

/* 
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for EIP, and a separate stack
   for ESP.
 */
static SysRes do_clone ( ThreadId ptid, 
                         UInt flags, Addr esp, 
                         Int* parent_tidptr, 
                         Int* child_tidptr, 
                         vki_modify_ldt_t *tlsinfo)
{
   static const Bool debug = False;

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   UWord*       stack;
   NSegment const* seg;
   SysRes       res;
   Int          eax;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = (UWord*)ML_(allocstack)(ctid);
   if (stack == NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto out;
   }

   /* Copy register state

      Both parent and child return to the same place, and the code
      following the clone syscall works out which is which, so we
      don't need to worry about it.

      The parent gets the child's new tid returned from clone, but the
      child gets 0.

      If the clone call specifies a NULL esp for the new thread, then
      it actually gets a copy of the parent's esp.
   */
   /* Note: the clone call done by the Quadrics Elan3 driver specifies
      clone flags of 0xF00, and it seems to rely on the assumption
      that the child inherits a copy of the parent's GDT.  
      setup_child takes care of setting that up. */
   setup_child( &ctst->arch, &ptst->arch, True );

   /* Make sys_clone appear to have returned Success(0) in the
      child. */
   ctst->arch.vex.guest_EAX = 0;

   if (esp != 0)
      ctst->arch.vex.guest_ESP = esp;

   ctst->os_state.parent = ptid;

   /* inherit signal mask */
   ctst->sig_mask     = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* Start the child with its threadgroup being the same as the
      parent's.  This is so that any exit_group calls that happen
      after the child is created but before it sets its
      os_state.threadgroup field for real (in thread_wrapper in
      syswrap-linux.c), really kill the new thread.  a.k.a this avoids
      a race condition in which the thread is unkillable (via
      exit_group) because its threadgroup is not set.  The race window
      is probably only a few hundred or a few thousand cycles long.
      See #226116. */
   ctst->os_state.threadgroup = ptst->os_state.threadgroup;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)((Addr)esp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(esp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;

      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);

      if (debug)
	 VG_(printf)("tid %d: guessed client stack range %#lx-%#lx\n",
		     ctid, seg->start, VG_PGROUNDUP(esp));
   } else {
      VG_(message)(Vg_UserMsg,
                   "!? New thread %d starts with ESP(%#lx) unmapped\n",
		   ctid, esp);
      ctst->client_stack_szB  = 0;
   }

   /* Assume the clone will succeed, and tell any tool that wants to
      know that this thread has come into existence.  We cannot defer
      it beyond this point because sys_set_thread_area, just below,
      causes tCheck to assert by making references to the new ThreadId
      if we don't state the new thread exists prior to that point.
      If the clone fails, we'll send out a ll_exit notification for it
      at the out: label below, to clean up. */
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
	 VG_(printf)("clone child has SETTLS: tls info at %p: idx=%d "
                     "base=%#lx limit=%x; esp=%#x fs=%x gs=%x\n",
		     tlsinfo, tlsinfo->entry_number, 
                     tlsinfo->base_addr, tlsinfo->limit,
		     ptst->arch.vex.guest_ESP,
		     ctst->arch.vex.guest_FS, ctst->arch.vex.guest_GS);
      res = sys_set_thread_area(ctid, tlsinfo);
      if (sr_isError(res))
	 goto out;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   eax = do_syscall_clone_x86_linux(
            ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
            child_tidptr, parent_tidptr, NULL
         );
   res = VG_(mk_SysRes_x86_linux)( eax );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

  out:
   if (sr_isError(res)) {
      /* clone failed */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
      /* oops.  Better tell the tool the thread exited in a hurry :-) */
      VG_TRACK( pre_thread_ll_exit, ctid );
   }

   return res;
}


/* ---------------------------------------------------------------------
   LDT/GDT simulation
   ------------------------------------------------------------------ */

/* Details of the LDT simulation
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   When a program runs natively, the linux kernel allows each *thread*
   in it to have its own LDT.  Almost all programs never do this --
   it's wildly unportable, after all -- and so the kernel never
   allocates the structure, which is just as well as an LDT occupies
   64k of memory (8192 entries of size 8 bytes).

   A thread may choose to modify its LDT entries, by doing the
   __NR_modify_ldt syscall.  In such a situation the kernel will then
   allocate an LDT structure for it.  Each LDT entry is basically a
   (base, limit) pair.  A virtual address in a specific segment is
   translated to a linear address by adding the segment's base value.
   In addition, the virtual address must not exceed the limit value.

   To use an LDT entry, a thread loads one of the segment registers
   (%cs, %ss, %ds, %es, %fs, %gs) with the index of the LDT entry (0
   .. 8191) it wants to use.  In fact, the required value is (index <<
   3) + 7, but that's not important right now.  Any normal instruction
   which includes an addressing mode can then be made relative to that
   LDT entry by prefixing the insn with a so-called segment-override
   prefix, a byte which indicates which of the 6 segment registers
   holds the LDT index.

   Now, a key constraint is that valgrind's address checks operate in
   terms of linear addresses.  So we have to explicitly translate
   virtual addrs into linear addrs, and that means doing a complete
   LDT simulation.

   Calls to modify_ldt are intercepted.  For each thread, we maintain
   an LDT (with the same normally-never-allocated optimisation that
   the kernel does).  This is updated as expected via calls to
   modify_ldt.

   When a thread does an amode calculation involving a segment
   override prefix, the relevant LDT entry for the thread is
   consulted.  It all works.

   There is a conceptual problem, which appears when switching back to
   native execution, either temporarily to pass syscalls to the
   kernel, or permanently, when debugging V.  Problem at such points
   is that it's pretty pointless to copy the simulated machine's
   segment registers to the real machine, because we'd also need to
   copy the simulated LDT into the real one, and that's prohibitively
   expensive.

   Fortunately it looks like no syscalls rely on the segment regs or
   LDT being correct, so we can get away with it.  Apart from that the
   simulation is pretty straightforward.  All 6 segment registers are
   tracked, although only %ds, %es, %fs and %gs are allowed as
   prefixes.  Perhaps it could be restricted even more than that -- I
   am not sure what is and isn't allowed in user-mode.
*/

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
      VG_(printf)("translate_to_hw_format: base %#lx, limit %d\n",
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

/* Create a zeroed-out GDT. */
static VexGuestX86SegDescr* alloc_zeroed_x86_GDT ( void )
{
   Int nbytes = VEX_GUEST_X86_GDT_NENT * sizeof(VexGuestX86SegDescr);
   return VG_(arena_calloc)(VG_AR_CORE, "di.syswrap-x86.azxG.1", nbytes, 1);
}

/* Create a zeroed-out LDT. */
static VexGuestX86SegDescr* alloc_zeroed_x86_LDT ( void )
{
   Int nbytes = VEX_GUEST_X86_LDT_NENT * sizeof(VexGuestX86SegDescr);
   return VG_(arena_calloc)(VG_AR_CORE, "di.syswrap-x86.azxL.1", nbytes, 1);
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

/* Copy contents between two existing GDTs. */
static void copy_GDT_from_to ( VexGuestX86SegDescr* src,
                               VexGuestX86SegDescr* dst )
{
   Int i;
   vg_assert(src);
   vg_assert(dst);
   for (i = 0; i < VEX_GUEST_X86_GDT_NENT; i++)
      dst[i] = src[i];
}

/* Free this thread's DTs, if it has any. */
static void deallocate_LGDTs_for_thread ( VexGuestX86State* vex )
{
   vg_assert(sizeof(HWord) == sizeof(void*));

   if (0)
      VG_(printf)("deallocate_LGDTs_for_thread: "
                  "ldt = 0x%lx, gdt = 0x%lx\n",
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
SysRes read_ldt ( ThreadId tid, UChar* ptr, UInt bytecount )
{
   SysRes res;
   UInt   i, size;
   UChar* ldt;

   if (0)
      VG_(printf)("read_ldt: tid = %d, ptr = %p, bytecount = %d\n",
                  tid, ptr, bytecount );

   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));
   vg_assert(8 == sizeof(VexGuestX86SegDescr));

   ldt = (Char*)(VG_(threads)[tid].arch.vex.guest_LDT);
   res = VG_(mk_SysRes_Success)( 0 );
   if (ldt == NULL)
      /* LDT not allocated, meaning all entries are null */
      goto out;

   size = VEX_GUEST_X86_LDT_NENT * sizeof(VexGuestX86SegDescr);
   if (size > bytecount)
      size = bytecount;

   res = VG_(mk_SysRes_Success)( size );
   for (i = 0; i < size; i++)
      ptr[i] = ldt[i];

  out:
   return res;
}


static
SysRes write_ldt ( ThreadId tid, void* ptr, UInt bytecount, Int oldmode )
{
   SysRes res;
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

   res = VG_(mk_SysRes_Error)( VKI_EINVAL );
   if (bytecount != sizeof(vki_modify_ldt_t))
      goto out;

   res = VG_(mk_SysRes_Error)( VKI_EINVAL );
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
   if (ldt == NULL) {
      ldt = alloc_zeroed_x86_LDT();
      VG_(threads)[tid].arch.vex.guest_LDT = (HWord)ldt;
   }

   /* Install the new entry ...  */
   translate_to_hw_format ( ldt_info, &ldt[ldt_info->entry_number], oldmode );
   res = VG_(mk_SysRes_Success)( 0 );

  out:
   return res;
}


static SysRes sys_modify_ldt ( ThreadId tid,
                               Int func, void* ptr, UInt bytecount )
{
   SysRes ret = VG_(mk_SysRes_Error)( VKI_ENOSYS );

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


static SysRes sys_set_thread_area ( ThreadId tid, vki_modify_ldt_t* info )
{
   Int                  idx;
   VexGuestX86SegDescr* gdt;

   vg_assert(8 == sizeof(VexGuestX86SegDescr));
   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));

   if (info == NULL)
      return VG_(mk_SysRes_Error)( VKI_EFAULT );

   gdt = (VexGuestX86SegDescr*)VG_(threads)[tid].arch.vex.guest_GDT;

   /* If the thread doesn't have a GDT, allocate it now. */
   if (!gdt) {
      gdt = alloc_zeroed_x86_GDT();
      VG_(threads)[tid].arch.vex.guest_GDT = (HWord)gdt;
   }

   idx = info->entry_number;

   if (idx == -1) {
      /* Find and use the first free entry.  Don't allocate entry
         zero, because the hardware will never do that, and apparently
         doing so confuses some code (perhaps stuff running on
         Wine). */
      for (idx = 1; idx < VEX_GUEST_X86_GDT_NENT; idx++) {
         if (gdt[idx].LdtEnt.Words.word1 == 0 
             && gdt[idx].LdtEnt.Words.word2 == 0)
            break;
      }

      if (idx == VEX_GUEST_X86_GDT_NENT)
         return VG_(mk_SysRes_Error)( VKI_ESRCH );
   } else if (idx < 0 || idx == 0 || idx >= VEX_GUEST_X86_GDT_NENT) {
      /* Similarly, reject attempts to use GDT[0]. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   translate_to_hw_format(info, &gdt[idx], 0);

   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid,
             "set_thread_area(info->entry)",
             (Addr) & info->entry_number, sizeof(unsigned int) );
   info->entry_number = idx;
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid,
             (Addr) & info->entry_number, sizeof(unsigned int) );

   return VG_(mk_SysRes_Success)( 0 );
}


static SysRes sys_get_thread_area ( ThreadId tid, vki_modify_ldt_t* info )
{
   Int idx;
   VexGuestX86SegDescr* gdt;

   vg_assert(sizeof(HWord) == sizeof(VexGuestX86SegDescr*));
   vg_assert(8 == sizeof(VexGuestX86SegDescr));

   if (info == NULL)
      return VG_(mk_SysRes_Error)( VKI_EFAULT );

   idx = info->entry_number;

   if (idx < 0 || idx >= VEX_GUEST_X86_GDT_NENT)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   gdt = (VexGuestX86SegDescr*)VG_(threads)[tid].arch.vex.guest_GDT;

   /* If the thread doesn't have a GDT, allocate it now. */
   if (!gdt) {
      gdt = alloc_zeroed_x86_GDT();
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

   return VG_(mk_SysRes_Success)( 0 );
}

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VG_(cleanup_thread) ( ThreadArchState* arch )
{
   /* Release arch-specific resources held by this thread. */
   /* On x86, we have to dump the LDT and GDT. */
   deallocate_LGDTs_for_thread( &arch->vex );
}  


static void setup_child ( /*OUT*/ ThreadArchState *child, 
                          /*IN*/  ThreadArchState *parent,
                          Bool inherit_parents_GDT )
{
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;

   /* We inherit our parent's LDT. */
   if (parent->vex.guest_LDT == (HWord)NULL) {
      /* We hope this is the common case. */
      child->vex.guest_LDT = (HWord)NULL;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      child->vex.guest_LDT = (HWord)alloc_zeroed_x86_LDT();
      copy_LDT_from_to( (VexGuestX86SegDescr*)parent->vex.guest_LDT, 
                        (VexGuestX86SegDescr*)child->vex.guest_LDT );
   }

   /* Either we start with an empty GDT (the usual case) or inherit a
      copy of our parents' one (Quadrics Elan3 driver -style clone
      only). */
   child->vex.guest_GDT = (HWord)NULL;

   if (inherit_parents_GDT && parent->vex.guest_GDT != (HWord)NULL) {
      child->vex.guest_GDT = (HWord)alloc_zeroed_x86_GDT();
      copy_GDT_from_to( (VexGuestX86SegDescr*)parent->vex.guest_GDT,
                        (VexGuestX86SegDescr*)child->vex.guest_GDT );
   }
}  


/* ---------------------------------------------------------------------
   PRE/POST wrappers for x86/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(x86_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(x86_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */
DECL_TEMPLATE(x86_linux, sys_socketcall);
DECL_TEMPLATE(x86_linux, sys_stat64);
DECL_TEMPLATE(x86_linux, sys_fstatat64);
DECL_TEMPLATE(x86_linux, sys_fstat64);
DECL_TEMPLATE(x86_linux, sys_lstat64);
DECL_TEMPLATE(x86_linux, sys_clone);
DECL_TEMPLATE(x86_linux, old_mmap);
DECL_TEMPLATE(x86_linux, sys_mmap2);
DECL_TEMPLATE(x86_linux, sys_sigreturn);
DECL_TEMPLATE(x86_linux, sys_ipc);
DECL_TEMPLATE(x86_linux, sys_rt_sigreturn);
DECL_TEMPLATE(x86_linux, sys_modify_ldt);
DECL_TEMPLATE(x86_linux, sys_set_thread_area);
DECL_TEMPLATE(x86_linux, sys_get_thread_area);
DECL_TEMPLATE(x86_linux, sys_ptrace);
DECL_TEMPLATE(x86_linux, sys_sigaction);
DECL_TEMPLATE(x86_linux, sys_sigsuspend);
DECL_TEMPLATE(x86_linux, old_select);
DECL_TEMPLATE(x86_linux, sys_vm86old);
DECL_TEMPLATE(x86_linux, sys_vm86);
DECL_TEMPLATE(x86_linux, sys_syscall223);

PRE(old_select)
{
   /* struct sel_arg_struct {
      unsigned long n;
      fd_set *inp, *outp, *exp;
      struct timeval *tvp;
      };
   */
   PRE_REG_READ1(long, "old_select", struct sel_arg_struct *, args);
   PRE_MEM_READ( "old_select(args)", ARG1, 5*sizeof(UWord) );
   *flags |= SfMayBlock;
   {
      UInt* arg_struct = (UInt*)ARG1;
      UInt a1, a2, a3, a4, a5;

      a1 = arg_struct[0];
      a2 = arg_struct[1];
      a3 = arg_struct[2];
      a4 = arg_struct[3];
      a5 = arg_struct[4];

      PRINT("old_select ( %d, %#x, %#x, %#x, %#x )", a1,a2,a3,a4,a5);
      if (a2 != (Addr)NULL)
         PRE_MEM_READ( "old_select(readfds)",   a2, a1/8 /* __FD_SETSIZE/8 */ );
      if (a3 != (Addr)NULL)
         PRE_MEM_READ( "old_select(writefds)",  a3, a1/8 /* __FD_SETSIZE/8 */ );
      if (a4 != (Addr)NULL)
         PRE_MEM_READ( "old_select(exceptfds)", a4, a1/8 /* __FD_SETSIZE/8 */ );
      if (a5 != (Addr)NULL)
         PRE_MEM_READ( "old_select(timeout)", a5, sizeof(struct vki_timeval) );
   }
}

PRE(sys_clone)
{
   UInt cloneflags;
   Bool badarg = False;

   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ2(int, "clone",
                 unsigned long, flags,
                 void *, child_stack);

   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA3("clone", int *, parent_tidptr);
      }
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         badarg = True;
      }
   }
   if (ARG1 & VKI_CLONE_SETTLS) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA4("clone", vki_modify_ldt_t *, tlsinfo);
      }
      PRE_MEM_READ("clone(tlsinfo)", ARG4, sizeof(vki_modify_ldt_t));
      if (!VG_(am_is_valid_for_client)(ARG4, sizeof(vki_modify_ldt_t), 
                                             VKI_PROT_READ)) {
         badarg = True;
      }
   }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA5("clone", int *, child_tidptr);
      }
      PRE_MEM_WRITE("clone(child_tidptr)", ARG5, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG5, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         badarg = True;
      }
   }

   if (badarg) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   cloneflags = ARG1;

   if (!ML_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Be ultra-paranoid and filter out any clone-variants we don't understand:
      - ??? specifies clone flags of 0x100011
      - ??? specifies clone flags of 0x1200011.
      - NPTL specifies clone flags of 0x7D0F00.
      - The Quadrics Elan3 driver specifies clone flags of 0xF00.
      - Newer Quadrics Elan3 drivers with NTPL support specify 0x410F00.
      Everything else is rejected. 
   */
   if (
        1 ||
        /* 11 Nov 05: for the time being, disable this ultra-paranoia.
           The switch below probably does a good enough job. */
          (cloneflags == 0x100011 || cloneflags == 0x1200011
                                  || cloneflags == 0x7D0F00
                                  || cloneflags == 0x790F00
                                  || cloneflags == 0x3D0F00
                                  || cloneflags == 0x410F00
                                  || cloneflags == 0xF00
                                  || cloneflags == 0xF21)) {
     /* OK */
   }
   else {
      /* Nah.  We don't like it.  Go away. */
      goto reject;
   }

   /* Only look at the flags we really care about */
   switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS 
                         | VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
   case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
      /* thread creation */
      SET_STATUS_from_SysRes(
         do_clone(tid,
                  ARG1,         /* flags */
                  (Addr)ARG2,   /* child ESP */
                  (Int *)ARG3,  /* parent_tidptr */
                  (Int *)ARG5,  /* child_tidptr */
                  (vki_modify_ldt_t *)ARG4)); /* set_tls */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_STATUS_from_SysRes(
         ML_(do_fork_clone)(tid,
                       cloneflags,      /* flags */
                       (Int *)ARG3,     /* parent_tidptr */
                       (Int *)ARG5));   /* child_tidptr */
      break;

   default:
   reject:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "\n");
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx\n", ARG1);
      VG_(message)(Vg_UserMsg, "\n");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:\n");
      VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)\n");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork\n");
      VG_(message)(Vg_UserMsg, " - for the Quadrics Elan3 user-space driver\n");
      VG_(unimplemented)
         ("Valgrind does not support general clone().");
   }

   if (SUCCESS) {
      if (ARG1 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG5, sizeof(Int));

      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

PRE(sys_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   ThreadState* tst;
   PRINT("sys_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Adjust esp to point to start of frame; skip back up over
      sigreturn sequence's "popl %eax" and handler ret addr */
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_ESP -= sizeof(Addr)+sizeof(Word);
   /* XXX why does ESP change differ from rt_sigreturn case below? */

   /* This is only so that the EIP is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, False);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   ThreadState* tst;
   PRINT("sys_rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Adjust esp to point to start of frame; skip back up over handler
      ret addr */
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_ESP -= sizeof(Addr);
   /* XXX why does ESP change differ from sigreturn case above? */

   /* This is only so that the EIP is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_modify_ldt)
{
   PRINT("sys_modify_ldt ( %ld, %#lx, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "modify_ldt", int, func, void *, ptr,
                 unsigned long, bytecount);
   
   if (ARG1 == 0) {
      /* read the LDT into ptr */
      PRE_MEM_WRITE( "modify_ldt(ptr)", ARG2, ARG3 );
   }
   if (ARG1 == 1 || ARG1 == 0x11) {
      /* write the LDT with the entry pointed at by ptr */
      PRE_MEM_READ( "modify_ldt(ptr)", ARG2, sizeof(vki_modify_ldt_t) );
   }
   /* "do" the syscall ourselves; the kernel never sees it */
   SET_STATUS_from_SysRes( sys_modify_ldt( tid, ARG1, (void*)ARG2, ARG3 ) );

   if (ARG1 == 0 && SUCCESS && RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
   }
}

PRE(sys_set_thread_area)
{
   PRINT("sys_set_thread_area ( %#lx )", ARG1);
   PRE_REG_READ1(int, "set_thread_area", struct user_desc *, u_info)
   PRE_MEM_READ( "set_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   SET_STATUS_from_SysRes( sys_set_thread_area( tid, (void *)ARG1 ) );
}

PRE(sys_get_thread_area)
{
   PRINT("sys_get_thread_area ( %#lx )", ARG1);
   PRE_REG_READ1(int, "get_thread_area", struct user_desc *, u_info)
   PRE_MEM_WRITE( "get_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   SET_STATUS_from_SysRes( sys_get_thread_area( tid, (void *)ARG1 ) );

   if (SUCCESS) {
      POST_MEM_WRITE( ARG1, sizeof(vki_modify_ldt_t) );
   }
}

// Parts of this are x86-specific, but the *PEEK* cases are generic.
//
// ARG3 is only used for pointers into the traced process's address
// space and for offsets into the traced process's struct
// user_regs_struct. It is never a pointer into this process's memory
// space, and we should therefore not check anything it points to.
PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "ptrace", 
                 long, request, long, pid, long, addr, long, data);
   switch (ARG1) {
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      PRE_MEM_WRITE( "ptrace(peek)", ARG4, 
		     sizeof (long));
      break;
   case VKI_PTRACE_GETREGS:
      PRE_MEM_WRITE( "ptrace(getregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETFPXREGS:
      PRE_MEM_WRITE( "ptrace(getfpxregs)", ARG4, 
                     sizeof(struct vki_user_fxsr_struct) );
      break;
   case VKI_PTRACE_SETREGS:
      PRE_MEM_READ( "ptrace(setregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_SETFPREGS:
      PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_SETFPXREGS:
      PRE_MEM_READ( "ptrace(setfpxregs)", ARG4, 
                     sizeof(struct vki_user_fxsr_struct) );
      break;
   case VKI_PTRACE_GETEVENTMSG:
      PRE_MEM_WRITE( "ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      PRE_MEM_WRITE( "ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_SETSIGINFO:
      PRE_MEM_READ( "ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   default:
      break;
   }
}

POST(sys_ptrace)
{
   switch (ARG1) {
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      POST_MEM_WRITE( ARG4, sizeof (long));
      break;
   case VKI_PTRACE_GETREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETFPXREGS:
      POST_MEM_WRITE( ARG4, sizeof(struct vki_user_fxsr_struct) );
      break;
   case VKI_PTRACE_GETEVENTMSG:
      POST_MEM_WRITE( ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      /* XXX: This is a simplification. Different parts of the
       * siginfo_t are valid depending on the type of signal.
       */
      POST_MEM_WRITE( ARG4, sizeof(vki_siginfo_t));
      break;
   default:
      break;
   }
}

static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}
 
PRE(sys_ipc)
{
   PRINT("sys_ipc ( %ld, %ld, %ld, %ld, %#lx, %ld )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   // XXX: this is simplistic -- some args are not used in all circumstances.
   PRE_REG_READ6(int, "ipc",
                 vki_uint, call, int, first, int, second, int, third,
                 void *, ptr, long, fifth)

   switch (ARG1 /* call */) {
   case VKI_SEMOP:
      ML_(generic_PRE_sys_semop)( tid, ARG2, ARG5, ARG3 );
      *flags |= SfMayBlock;
      break;
   case VKI_SEMGET:
      break;
   case VKI_SEMCTL:
   {
      UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
      ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
      break;
   }
   case VKI_SEMTIMEDOP:
      ML_(generic_PRE_sys_semtimedop)( tid, ARG2, ARG5, ARG3, ARG6 );
      *flags |= SfMayBlock;
      break;
   case VKI_MSGSND:
      ML_(linux_PRE_sys_msgsnd)( tid, ARG2, ARG5, ARG3, ARG4 );
      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         *flags |= SfMayBlock;
      break;
   case VKI_MSGRCV:
   {
      Addr msgp;
      Word msgtyp;
 
      msgp = deref_Addr( tid,
			 (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
			 "msgrcv(msgp)" );
      msgtyp = deref_Addr( tid,
			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
			   "msgrcv(msgp)" );

      ML_(linux_PRE_sys_msgrcv)( tid, ARG2, msgp, ARG3, msgtyp, ARG4 );

      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         *flags |= SfMayBlock;
      break;
   }
   case VKI_MSGGET:
      break;
   case VKI_MSGCTL:
      ML_(linux_PRE_sys_msgctl)( tid, ARG2, ARG3, ARG5 );
      break;
   case VKI_SHMAT:
   {
      UWord w;
      PRE_MEM_WRITE( "shmat(raddr)", ARG4, sizeof(Addr) );
      w = ML_(generic_PRE_sys_shmat)( tid, ARG2, ARG5, ARG3 );
      if (w == 0)
         SET_STATUS_Failure( VKI_EINVAL );
      else
         ARG5 = w;
      break;
   }
   case VKI_SHMDT:
      if (!ML_(generic_PRE_sys_shmdt)(tid, ARG5))
	 SET_STATUS_Failure( VKI_EINVAL );
      break;
   case VKI_SHMGET:
      break;
   case VKI_SHMCTL: /* IPCOP_shmctl */
      ML_(generic_PRE_sys_shmctl)( tid, ARG2, ARG3, ARG5 );
      break;
   default:
      VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %ld\n", ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }   
}

POST(sys_ipc)
{
   vg_assert(SUCCESS);
   switch (ARG1 /* call */) {
   case VKI_SEMOP:
   case VKI_SEMGET:
      break;
   case VKI_SEMCTL:
   {
      UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
      ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
      break;
   }
   case VKI_SEMTIMEDOP:
   case VKI_MSGSND:
      break;
   case VKI_MSGRCV:
   {
      Addr msgp;
      Word msgtyp;

      msgp = deref_Addr( tid,
			 (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
			 "msgrcv(msgp)" );
      msgtyp = deref_Addr( tid,
			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
			   "msgrcv(msgp)" );

      ML_(linux_POST_sys_msgrcv)( tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4 );
      break;
   }
   case VKI_MSGGET:
      break;
   case VKI_MSGCTL:
      ML_(linux_POST_sys_msgctl)( tid, RES, ARG2, ARG3, ARG5 );
      break;
   case VKI_SHMAT:
   {
      Addr addr;

      /* force readability. before the syscall it is
       * indeed uninitialized, as can be seen in
       * glibc/sysdeps/unix/sysv/linux/shmat.c */
      POST_MEM_WRITE( ARG4, sizeof( Addr ) );

      addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
      ML_(generic_POST_sys_shmat)( tid, addr, ARG2, ARG5, ARG3 );
      break;
   }
   case VKI_SHMDT:
      ML_(generic_POST_sys_shmdt)( tid, RES, ARG5 );
      break;
   case VKI_SHMGET:
      break;
   case VKI_SHMCTL:
      ML_(generic_POST_sys_shmctl)( tid, RES, ARG2, ARG3, ARG5 );
      break;
   default:
      VG_(message)(Vg_DebugMsg,
		   "FATAL: unhandled syscall(ipc) %ld\n",
		   ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}

PRE(old_mmap)
{
   /* struct mmap_arg_struct {           
         unsigned long addr;
         unsigned long len;
         unsigned long prot;
         unsigned long flags;
         unsigned long fd;
         unsigned long offset;
   }; */
   UWord a1, a2, a3, a4, a5, a6;
   SysRes r;

   UWord* args = (UWord*)ARG1;
   PRE_REG_READ1(long, "old_mmap", struct mmap_arg_struct *, args);
   PRE_MEM_READ( "old_mmap(args)", (Addr)args, 6*sizeof(UWord) );

   a1 = args[1-1];
   a2 = args[2-1];
   a3 = args[3-1];
   a4 = args[4-1];
   a5 = args[5-1];
   a6 = args[6-1];

   PRINT("old_mmap ( %#lx, %llu, %ld, %ld, %ld, %ld )",
         a1, (ULong)a2, a3, a4, a5, a6 );

   r = ML_(generic_PRE_sys_mmap)( tid, a1, a2, a3, a4, a5, (Off64T)a6 );
   SET_STATUS_from_SysRes(r);
}

PRE(sys_mmap2)
{
   SysRes r;

   // Exactly like old_mmap() except:
   //  - all 6 args are passed in regs, rather than in a memory-block.
   //  - the file offset is specified in pagesize units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   // pagesize or 4K-size units in offset?  For ppc32/64-linux, this is
   // 4K-sized.  Assert that the page size is 4K here for safety.
   vg_assert(VKI_PAGE_SIZE == 4096);
   PRINT("sys_mmap2 ( %#lx, %llu, %ld, %ld, %ld, %ld )",
         ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap2",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
                                       4096 * (Off64T)ARG6 );
   SET_STATUS_from_SysRes(r);
}

// XXX: lstat64/fstat64/stat64 are generic, but not necessarily
// applicable to every architecture -- I think only to 32-bit archs.
// We're going to need something like linux/core_os32.h for such
// things, eventually, I think.  --njn
PRE(sys_lstat64)
{
   PRINT("sys_lstat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "lstat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "lstat64(file_name)", ARG1 );
   PRE_MEM_WRITE( "lstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_lstat64)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
   }
}

PRE(sys_stat64)
{
   PRINT("sys_stat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "stat64(file_name)", ARG1 );
   PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_stat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(sys_fstatat64)
{
   PRINT("sys_fstatat64 ( %ld, %#lx(%s), %#lx )",ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "fstatat64",
                 int, dfd, char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "fstatat64(file_name)", ARG2 );
   PRE_MEM_WRITE( "fstatat64(buf)", ARG3, sizeof(struct vki_stat64) );
}

POST(sys_fstatat64)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_stat64) );
}

PRE(sys_fstat64)
{
   PRINT("sys_fstat64 ( %ld, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(long, "fstat64", unsigned long, fd, struct stat64 *, buf);
   PRE_MEM_WRITE( "fstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_fstat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

   *flags |= SfMayBlock;
   PRINT("sys_socketcall ( %ld, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(long, "socketcall", int, call, unsigned long *, args);

   switch (ARG1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
      /* int socketpair(int d, int type, int protocol, int sv[2]); */
      PRE_MEM_READ( "socketcall.socketpair(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_socketpair)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   case VKI_SYS_SOCKET:
      /* int socket(int domain, int type, int protocol); */
      PRE_MEM_READ( "socketcall.socket(args)", ARG2, 3*sizeof(Addr) );
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
                  int addrlen); */
      PRE_MEM_READ( "socketcall.bind(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_bind)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      PRE_MEM_READ( "socketcall.listen(args)", ARG2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_ACCEPT: {
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      PRE_MEM_READ( "socketcall.accept(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
   }

   case VKI_SYS_ACCEPT4: {
      /*int accept(int s, struct sockaddr *add, int *addrlen, int flags)*/
      PRE_MEM_READ( "socketcall.accept4(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
   }

   case VKI_SYS_SENDTO:
      /* int sendto(int s, const void *msg, int len, 
                    unsigned int flags, 
                    const struct sockaddr *to, int tolen); */
      PRE_MEM_READ( "socketcall.sendto(args)", ARG2, 6*sizeof(Addr) );
      ML_(generic_PRE_sys_sendto)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                   ARG2_3, ARG2_4, ARG2_5 );
      break;

   case VKI_SYS_SEND:
      /* int send(int s, const void *msg, size_t len, int flags); */
      PRE_MEM_READ( "socketcall.send(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_send)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_RECVFROM:
      /* int recvfrom(int s, void *buf, int len, unsigned int flags,
         struct sockaddr *from, int *fromlen); */
      PRE_MEM_READ( "socketcall.recvfrom(args)", ARG2, 6*sizeof(Addr) );
      ML_(generic_PRE_sys_recvfrom)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                     ARG2_3, ARG2_4, ARG2_5 );
      break;
   
   case VKI_SYS_RECV:
      /* int recv(int s, void *buf, int len, unsigned int flags); */
      /* man 2 recv says:
         The  recv call is normally used only on a connected socket
         (see connect(2)) and is identical to recvfrom with a  NULL
         from parameter.
      */
      PRE_MEM_READ( "socketcall.recv(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_recv)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_CONNECT:
      /* int connect(int sockfd, 
                     struct sockaddr *serv_addr, int addrlen ); */
      PRE_MEM_READ( "socketcall.connect(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_connect)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
                        const void *optval, int optlen); */
      PRE_MEM_READ( "socketcall.setsockopt(args)", ARG2, 5*sizeof(Addr) );
      ML_(generic_PRE_sys_setsockopt)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                       ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKOPT:
      /* int getsockopt(int s, int level, int optname, 
                        void *optval, socklen_t *optlen); */
      PRE_MEM_READ( "socketcall.getsockopt(args)", ARG2, 5*sizeof(Addr) );
      ML_(linux_PRE_sys_getsockopt)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                     ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKNAME:
      /* int getsockname(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ( "socketcall.getsockname(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_getsockname)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_GETPEERNAME:
      /* int getpeername(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ( "socketcall.getpeername(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_getpeername)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SHUTDOWN:
      /* int shutdown(int s, int how); */
      PRE_MEM_READ( "socketcall.shutdown(args)", ARG2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_SENDMSG: {
      /* int sendmsg(int s, const struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       PRE_MEM_READ( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
      */
      ML_(generic_PRE_sys_sendmsg)( tid, ARG2_0, ARG2_1 );
      break;
   }
      
   case VKI_SYS_RECVMSG: {
      /* int recvmsg(int s, struct msghdr *msg, int flags); */

      /* this causes warnings, and I don't get why. glibc bug?
       * (after all it's glibc providing the arguments array)
       PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
      */
      ML_(generic_PRE_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
      break;
   }

   default:
      VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%lx\n",ARG1);
      SET_STATUS_Failure( VKI_EINVAL );
      break;
   }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

POST(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

   SysRes r;
   vg_assert(SUCCESS);
   switch (ARG1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
      r = ML_(generic_POST_sys_socketpair)( 
             tid, VG_(mk_SysRes_Success)(RES), 
             ARG2_0, ARG2_1, ARG2_2, ARG2_3 
          );
      SET_STATUS_from_SysRes(r);
      break;

   case VKI_SYS_SOCKET:
      r = ML_(generic_POST_sys_socket)( tid, VG_(mk_SysRes_Success)(RES) );
      SET_STATUS_from_SysRes(r);
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
			int addrlen); */
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      break;

   case VKI_SYS_ACCEPT:
   case VKI_SYS_ACCEPT4:
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      /* int accept4(int s, struct sockaddr *addr, int *addrlen, int flags); */
     r = ML_(generic_POST_sys_accept)( tid, VG_(mk_SysRes_Success)(RES), 
                                            ARG2_0, ARG2_1, ARG2_2 );
     SET_STATUS_from_SysRes(r);
     break;

   case VKI_SYS_SENDTO:
      break;

   case VKI_SYS_SEND:
      break;

   case VKI_SYS_RECVFROM:
      ML_(generic_POST_sys_recvfrom)( tid, VG_(mk_SysRes_Success)(RES),
                                           ARG2_0, ARG2_1, ARG2_2,
                                           ARG2_3, ARG2_4, ARG2_5 );
      break;

   case VKI_SYS_RECV:
      ML_(generic_POST_sys_recv)( tid, RES, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_CONNECT:
      break;

   case VKI_SYS_SETSOCKOPT:
      break;

   case VKI_SYS_GETSOCKOPT:
      ML_(linux_POST_sys_getsockopt)( tid, VG_(mk_SysRes_Success)(RES),
                                      ARG2_0, ARG2_1, 
                                      ARG2_2, ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKNAME:
      ML_(generic_POST_sys_getsockname)( tid, VG_(mk_SysRes_Success)(RES),
                                              ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_GETPEERNAME:
      ML_(generic_POST_sys_getpeername)( tid, VG_(mk_SysRes_Success)(RES), 
                                              ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SHUTDOWN:
      break;

   case VKI_SYS_SENDMSG:
      break;

   case VKI_SYS_RECVMSG:
     ML_(generic_POST_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
     break;

   default:
      VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%lx\n",ARG1);
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

/* Convert from non-RT to RT sigset_t's */
static 
void convert_sigset_to_rt(const vki_old_sigset_t *oldset, vki_sigset_t *set)
{
   VG_(sigemptyset)(set);
   set->sig[0] = *oldset;
}
PRE(sys_sigaction)
{
   vki_sigaction_toK_t   new, *newp;
   vki_sigaction_fromK_t old, *oldp;

   PRINT("sys_sigaction ( %ld, %#lx, %#lx )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "sigaction",
                 int, signum, const struct old_sigaction *, act,
                 struct old_sigaction *, oldact);

   newp = oldp = NULL;

   if (ARG2 != 0) {
      struct vki_old_sigaction *sa = (struct vki_old_sigaction *)ARG2;
      PRE_MEM_READ( "sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
      if (ML_(safe_to_deref)(sa,sizeof(sa)) 
          && (sa->sa_flags & VKI_SA_RESTORER))
         PRE_MEM_READ( "sigaction(act->sa_restorer)", (Addr)&sa->sa_restorer, sizeof(sa->sa_restorer));
   }

   if (ARG3 != 0) {
      PRE_MEM_WRITE( "sigaction(oldact)", ARG3, sizeof(struct vki_old_sigaction));
      oldp = &old;
   }

   //jrs 20050207: what?!  how can this make any sense?
   //if (VG_(is_kerror)(SYSRES))
   //   return;

   if (ARG2 != 0) {
      struct vki_old_sigaction *oldnew = (struct vki_old_sigaction *)ARG2;
      new.ksa_handler = oldnew->ksa_handler;
      new.sa_flags    = oldnew->sa_flags;
      new.sa_restorer = oldnew->sa_restorer;
      convert_sigset_to_rt(&oldnew->sa_mask, &new.sa_mask);
      newp = &new;
   }

   SET_STATUS_from_SysRes( VG_(do_sys_sigaction)(ARG1, newp, oldp) );

   if (ARG3 != 0 && SUCCESS && RES == 0) {
      struct vki_old_sigaction *oldold = (struct vki_old_sigaction *)ARG3;
      oldold->ksa_handler = oldp->ksa_handler;
      oldold->sa_flags    = oldp->sa_flags;
      oldold->sa_restorer = oldp->sa_restorer;
      oldold->sa_mask = oldp->sa_mask.sig[0];
   }
}

POST(sys_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_old_sigaction));
}

PRE(sys_sigsuspend)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has three arguments - the first
      two don't appear to be used by the kernel and are always passed as
      zero by glibc and the third is the first word of the signal mask
      so only 32 signals are supported.
     
      In fact glibc normally uses rt_sigsuspend if it is available as
      that takes a pointer to the signal mask so supports more signals.
    */
   *flags |= SfMayBlock;
   PRINT("sys_sigsuspend ( %ld, %ld, %ld )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(int, "sigsuspend",
                 int, history0, int, history1,
                 vki_old_sigset_t, mask);
}

PRE(sys_vm86old)
{
   PRINT("sys_vm86old ( %#lx )", ARG1);
   PRE_REG_READ1(int, "vm86old", struct vm86_struct *, info);
   PRE_MEM_WRITE( "vm86old(info)", ARG1, sizeof(struct vki_vm86_struct));
}

POST(sys_vm86old)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_vm86_struct));
}

PRE(sys_vm86)
{
   PRINT("sys_vm86 ( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(int, "vm86", unsigned long, fn, struct vm86plus_struct *, v86);
   if (ARG1 == VKI_VM86_ENTER || ARG1 == VKI_VM86_ENTER_NO_BYPASS)
      PRE_MEM_WRITE( "vm86(v86)", ARG2, sizeof(struct vki_vm86plus_struct));
}

POST(sys_vm86)
{
   if (ARG1 == VKI_VM86_ENTER || ARG1 == VKI_VM86_ENTER_NO_BYPASS)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_vm86plus_struct));
}


/* ---------------------------------------------------------------
   PRE/POST wrappers for x86/Linux-variant specific syscalls
   ------------------------------------------------------------ */

PRE(sys_syscall223)
{
   Int err;

   /* 223 is used by sys_bproc.  If we're not on a declared bproc
      variant, fail in the usual way. */

   if (!VG_(strstr)(VG_(clo_kernel_variant), "bproc")) {
      PRINT("non-existent syscall! (syscall 223)");
      PRE_REG_READ0(long, "ni_syscall(223)");
      SET_STATUS_Failure( VKI_ENOSYS );
      return;
   }

   err = ML_(linux_variant_PRE_sys_bproc)( ARG1, ARG2, ARG3, 
                                           ARG4, ARG5, ARG6 );
   if (err) {
      SET_STATUS_Failure( err );
      return;
   }
   /* Let it go through. */
   *flags |= SfMayBlock; /* who knows?  play safe. */
}

POST(sys_syscall223)
{
   ML_(linux_variant_POST_sys_bproc)( ARG1, ARG2, ARG3, 
                                      ARG4, ARG5, ARG6 );
}

#undef PRE
#undef POST


/* ---------------------------------------------------------------------
   The x86/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an x86-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(x86_linux, sysno, name) 
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(x86_linux, sysno, name)


// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-i386/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on x86 (as per sys_call_table in linux/arch/i386/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_table[] = {
//zz    //   (restart_syscall)                             // 0
   GENX_(__NR_exit,              sys_exit),           // 1
   GENX_(__NR_fork,              sys_fork),           // 2
   GENXY(__NR_read,              sys_read),           // 3
   GENX_(__NR_write,             sys_write),          // 4

   GENXY(__NR_open,              sys_open),           // 5
   GENXY(__NR_close,             sys_close),          // 6
   GENXY(__NR_waitpid,           sys_waitpid),        // 7
   GENXY(__NR_creat,             sys_creat),          // 8
   GENX_(__NR_link,              sys_link),           // 9

   GENX_(__NR_unlink,            sys_unlink),         // 10
   GENX_(__NR_execve,            sys_execve),         // 11
   GENX_(__NR_chdir,             sys_chdir),          // 12
   GENXY(__NR_time,              sys_time),           // 13
   GENX_(__NR_mknod,             sys_mknod),          // 14

   GENX_(__NR_chmod,             sys_chmod),          // 15
//zz    LINX_(__NR_lchown,            sys_lchown16),       // 16
   GENX_(__NR_break,             sys_ni_syscall),     // 17
//zz    //   (__NR_oldstat,           sys_stat),           // 18 (obsolete)
   LINX_(__NR_lseek,             sys_lseek),          // 19

   GENX_(__NR_getpid,            sys_getpid),         // 20
   LINX_(__NR_mount,             sys_mount),          // 21
   LINX_(__NR_umount,            sys_oldumount),      // 22
   LINX_(__NR_setuid,            sys_setuid16),       // 23 ## P
   LINX_(__NR_getuid,            sys_getuid16),       // 24 ## P

   LINX_(__NR_stime,             sys_stime),          // 25 * (SVr4,SVID,X/OPEN)
   PLAXY(__NR_ptrace,            sys_ptrace),         // 26
   GENX_(__NR_alarm,             sys_alarm),          // 27
//zz    //   (__NR_oldfstat,          sys_fstat),          // 28 * L -- obsolete
   GENX_(__NR_pause,             sys_pause),          // 29

   LINX_(__NR_utime,             sys_utime),          // 30
   GENX_(__NR_stty,              sys_ni_syscall),     // 31
   GENX_(__NR_gtty,              sys_ni_syscall),     // 32
   GENX_(__NR_access,            sys_access),         // 33
   GENX_(__NR_nice,              sys_nice),           // 34

   GENX_(__NR_ftime,             sys_ni_syscall),     // 35
   GENX_(__NR_sync,              sys_sync),           // 36
   GENX_(__NR_kill,              sys_kill),           // 37
   GENX_(__NR_rename,            sys_rename),         // 38
   GENX_(__NR_mkdir,             sys_mkdir),          // 39

   GENX_(__NR_rmdir,             sys_rmdir),          // 40
   GENXY(__NR_dup,               sys_dup),            // 41
   LINXY(__NR_pipe,              sys_pipe),           // 42
   GENXY(__NR_times,             sys_times),          // 43
   GENX_(__NR_prof,              sys_ni_syscall),     // 44
//zz 
   GENX_(__NR_brk,               sys_brk),            // 45
   LINX_(__NR_setgid,            sys_setgid16),       // 46
   LINX_(__NR_getgid,            sys_getgid16),       // 47
//zz    //   (__NR_signal,            sys_signal),         // 48 */* (ANSI C)
   LINX_(__NR_geteuid,           sys_geteuid16),      // 49

   LINX_(__NR_getegid,           sys_getegid16),      // 50
   GENX_(__NR_acct,              sys_acct),           // 51
   LINX_(__NR_umount2,           sys_umount),         // 52
   GENX_(__NR_lock,              sys_ni_syscall),     // 53
   LINXY(__NR_ioctl,             sys_ioctl),          // 54

   LINXY(__NR_fcntl,             sys_fcntl),          // 55
   GENX_(__NR_mpx,               sys_ni_syscall),     // 56
   GENX_(__NR_setpgid,           sys_setpgid),        // 57
   GENX_(__NR_ulimit,            sys_ni_syscall),     // 58
//zz    //   (__NR_oldolduname,       sys_olduname),       // 59 Linux -- obsolete
//zz 
   GENX_(__NR_umask,             sys_umask),          // 60
   GENX_(__NR_chroot,            sys_chroot),         // 61
//zz    //   (__NR_ustat,             sys_ustat)           // 62 SVr4 -- deprecated
   GENXY(__NR_dup2,              sys_dup2),           // 63
   GENX_(__NR_getppid,           sys_getppid),        // 64

   GENX_(__NR_getpgrp,           sys_getpgrp),        // 65
   GENX_(__NR_setsid,            sys_setsid),         // 66
   PLAXY(__NR_sigaction,         sys_sigaction),      // 67
//zz    //   (__NR_sgetmask,          sys_sgetmask),       // 68 */* (ANSI C)
//zz    //   (__NR_ssetmask,          sys_ssetmask),       // 69 */* (ANSI C)
//zz 
   LINX_(__NR_setreuid,          sys_setreuid16),     // 70
   LINX_(__NR_setregid,          sys_setregid16),     // 71
   PLAX_(__NR_sigsuspend,        sys_sigsuspend),     // 72
   LINXY(__NR_sigpending,        sys_sigpending),     // 73
//zz    //   (__NR_sethostname,       sys_sethostname),    // 74 */*
//zz 
   GENX_(__NR_setrlimit,         sys_setrlimit),      // 75
   GENXY(__NR_getrlimit,         sys_old_getrlimit),  // 76
   GENXY(__NR_getrusage,         sys_getrusage),      // 77
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 78
   GENX_(__NR_settimeofday,      sys_settimeofday),   // 79

   LINXY(__NR_getgroups,         sys_getgroups16),    // 80
   LINX_(__NR_setgroups,         sys_setgroups16),    // 81
   PLAX_(__NR_select,            old_select),         // 82
   GENX_(__NR_symlink,           sys_symlink),        // 83
//zz    //   (__NR_oldlstat,          sys_lstat),          // 84 -- obsolete
//zz 
   GENX_(__NR_readlink,          sys_readlink),       // 85
//zz    //   (__NR_uselib,            sys_uselib),         // 86 */Linux
//zz    //   (__NR_swapon,            sys_swapon),         // 87 */Linux
//zz    //   (__NR_reboot,            sys_reboot),         // 88 */Linux
//zz    //   (__NR_readdir,           old_readdir),        // 89 -- superseded
//zz 
   PLAX_(__NR_mmap,              old_mmap),           // 90
   GENXY(__NR_munmap,            sys_munmap),         // 91
   GENX_(__NR_truncate,          sys_truncate),       // 92
   GENX_(__NR_ftruncate,         sys_ftruncate),      // 93
   GENX_(__NR_fchmod,            sys_fchmod),         // 94

   LINX_(__NR_fchown,            sys_fchown16),       // 95
   GENX_(__NR_getpriority,       sys_getpriority),    // 96
   GENX_(__NR_setpriority,       sys_setpriority),    // 97
   GENX_(__NR_profil,            sys_ni_syscall),     // 98
   GENXY(__NR_statfs,            sys_statfs),         // 99

   GENXY(__NR_fstatfs,           sys_fstatfs),        // 100
   LINX_(__NR_ioperm,            sys_ioperm),         // 101
   PLAXY(__NR_socketcall,        sys_socketcall),     // 102 x86/Linux-only
   LINXY(__NR_syslog,            sys_syslog),         // 103
   GENXY(__NR_setitimer,         sys_setitimer),      // 104

   GENXY(__NR_getitimer,         sys_getitimer),      // 105
   GENXY(__NR_stat,              sys_newstat),        // 106
   GENXY(__NR_lstat,             sys_newlstat),       // 107
   GENXY(__NR_fstat,             sys_newfstat),       // 108
//zz    //   (__NR_olduname,          sys_uname),          // 109 -- obsolete
//zz 
   GENX_(__NR_iopl,              sys_iopl),           // 110
   LINX_(__NR_vhangup,           sys_vhangup),        // 111
   GENX_(__NR_idle,              sys_ni_syscall),     // 112
   PLAXY(__NR_vm86old,           sys_vm86old),        // 113 x86/Linux-only
   GENXY(__NR_wait4,             sys_wait4),          // 114
//zz 
//zz    //   (__NR_swapoff,           sys_swapoff),        // 115 */Linux 
   LINXY(__NR_sysinfo,           sys_sysinfo),        // 116
   PLAXY(__NR_ipc,               sys_ipc),            // 117
   GENX_(__NR_fsync,             sys_fsync),          // 118
   PLAX_(__NR_sigreturn,         sys_sigreturn),      // 119 ?/Linux

   PLAX_(__NR_clone,             sys_clone),          // 120
//zz    //   (__NR_setdomainname,     sys_setdomainname),  // 121 */*(?)
   GENXY(__NR_uname,             sys_newuname),       // 122
   PLAX_(__NR_modify_ldt,        sys_modify_ldt),     // 123
   LINXY(__NR_adjtimex,          sys_adjtimex),       // 124

   GENXY(__NR_mprotect,          sys_mprotect),       // 125
   LINXY(__NR_sigprocmask,       sys_sigprocmask),    // 126
//zz    // Nb: create_module() was removed 2.4-->2.6
   GENX_(__NR_create_module,     sys_ni_syscall),     // 127
   LINX_(__NR_init_module,       sys_init_module),    // 128
   LINX_(__NR_delete_module,     sys_delete_module),  // 129
//zz 
//zz    // Nb: get_kernel_syms() was removed 2.4-->2.6
   GENX_(__NR_get_kernel_syms,   sys_ni_syscall),     // 130
   LINX_(__NR_quotactl,          sys_quotactl),       // 131
   GENX_(__NR_getpgid,           sys_getpgid),        // 132
   GENX_(__NR_fchdir,            sys_fchdir),         // 133
//zz    //   (__NR_bdflush,           sys_bdflush),        // 134 */Linux
//zz 
//zz    //   (__NR_sysfs,             sys_sysfs),          // 135 SVr4
   LINX_(__NR_personality,       sys_personality),    // 136
   GENX_(__NR_afs_syscall,       sys_ni_syscall),     // 137
   LINX_(__NR_setfsuid,          sys_setfsuid16),     // 138
   LINX_(__NR_setfsgid,          sys_setfsgid16),     // 139
 
   LINXY(__NR__llseek,           sys_llseek),         // 140
   GENXY(__NR_getdents,          sys_getdents),       // 141
   GENX_(__NR__newselect,        sys_select),         // 142
   GENX_(__NR_flock,             sys_flock),          // 143
   GENX_(__NR_msync,             sys_msync),          // 144

   GENXY(__NR_readv,             sys_readv),          // 145
   GENX_(__NR_writev,            sys_writev),         // 146
   GENX_(__NR_getsid,            sys_getsid),         // 147
   GENX_(__NR_fdatasync,         sys_fdatasync),      // 148
   LINXY(__NR__sysctl,           sys_sysctl),         // 149

   GENX_(__NR_mlock,             sys_mlock),          // 150
   GENX_(__NR_munlock,           sys_munlock),        // 151
   GENX_(__NR_mlockall,          sys_mlockall),       // 152
   LINX_(__NR_munlockall,        sys_munlockall),     // 153
   LINXY(__NR_sched_setparam,    sys_sched_setparam), // 154

   LINXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   LINX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   LINX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   LINX_(__NR_sched_yield,            sys_sched_yield),           // 158
   LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
   LINXY(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161
   GENXY(__NR_nanosleep,         sys_nanosleep),      // 162
   GENX_(__NR_mremap,            sys_mremap),         // 163
   LINX_(__NR_setresuid,         sys_setresuid16),    // 164

   LINXY(__NR_getresuid,         sys_getresuid16),    // 165
   PLAXY(__NR_vm86,              sys_vm86),           // 166 x86/Linux-only
   GENX_(__NR_query_module,      sys_ni_syscall),     // 167
   GENXY(__NR_poll,              sys_poll),           // 168
//zz    //   (__NR_nfsservctl,        sys_nfsservctl),     // 169 */Linux
//zz 
   LINX_(__NR_setresgid,         sys_setresgid16),    // 170
   LINXY(__NR_getresgid,         sys_getresgid16),    // 171
   LINXY(__NR_prctl,             sys_prctl),          // 172
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),   // 173 x86/Linux only?
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 174

   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 175
   LINXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 176
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 177
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 178
   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 179

   GENXY(__NR_pread64,           sys_pread64),        // 180
   GENX_(__NR_pwrite64,          sys_pwrite64),       // 181
   LINX_(__NR_chown,             sys_chown16),        // 182
   GENXY(__NR_getcwd,            sys_getcwd),         // 183
   LINXY(__NR_capget,            sys_capget),         // 184

   LINX_(__NR_capset,            sys_capset),         // 185
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 186
   LINXY(__NR_sendfile,          sys_sendfile),       // 187
   GENXY(__NR_getpmsg,           sys_getpmsg),        // 188
   GENX_(__NR_putpmsg,           sys_putpmsg),        // 189

   // Nb: we treat vfork as fork
   GENX_(__NR_vfork,             sys_fork),           // 190
   GENXY(__NR_ugetrlimit,        sys_getrlimit),      // 191
   PLAX_(__NR_mmap2,             sys_mmap2),          // 192
   GENX_(__NR_truncate64,        sys_truncate64),     // 193
   GENX_(__NR_ftruncate64,       sys_ftruncate64),    // 194
   
   PLAXY(__NR_stat64,            sys_stat64),         // 195
   PLAXY(__NR_lstat64,           sys_lstat64),        // 196
   PLAXY(__NR_fstat64,           sys_fstat64),        // 197
   GENX_(__NR_lchown32,          sys_lchown),         // 198
   GENX_(__NR_getuid32,          sys_getuid),         // 199

   GENX_(__NR_getgid32,          sys_getgid),         // 200
   GENX_(__NR_geteuid32,         sys_geteuid),        // 201
   GENX_(__NR_getegid32,         sys_getegid),        // 202
   GENX_(__NR_setreuid32,        sys_setreuid),       // 203
   GENX_(__NR_setregid32,        sys_setregid),       // 204

   GENXY(__NR_getgroups32,       sys_getgroups),      // 205
   GENX_(__NR_setgroups32,       sys_setgroups),      // 206
   GENX_(__NR_fchown32,          sys_fchown),         // 207
   LINX_(__NR_setresuid32,       sys_setresuid),      // 208
   LINXY(__NR_getresuid32,       sys_getresuid),      // 209

   LINX_(__NR_setresgid32,       sys_setresgid),      // 210
   LINXY(__NR_getresgid32,       sys_getresgid),      // 211
   GENX_(__NR_chown32,           sys_chown),          // 212
   GENX_(__NR_setuid32,          sys_setuid),         // 213
   GENX_(__NR_setgid32,          sys_setgid),         // 214

   LINX_(__NR_setfsuid32,        sys_setfsuid),       // 215
   LINX_(__NR_setfsgid32,        sys_setfsgid),       // 216
//zz    //   (__NR_pivot_root,        sys_pivot_root),     // 217 */Linux
   GENXY(__NR_mincore,           sys_mincore),        // 218
   GENX_(__NR_madvise,           sys_madvise),        // 219

   GENXY(__NR_getdents64,        sys_getdents64),     // 220
   LINXY(__NR_fcntl64,           sys_fcntl64),        // 221
   GENX_(222,                    sys_ni_syscall),     // 222
   PLAXY(223,                    sys_syscall223),     // 223 // sys_bproc?
   LINX_(__NR_gettid,            sys_gettid),         // 224

   LINX_(__NR_readahead,         sys_readahead),      // 225 */Linux
   LINX_(__NR_setxattr,          sys_setxattr),       // 226
   LINX_(__NR_lsetxattr,         sys_lsetxattr),      // 227
   LINX_(__NR_fsetxattr,         sys_fsetxattr),      // 228
   LINXY(__NR_getxattr,          sys_getxattr),       // 229

   LINXY(__NR_lgetxattr,         sys_lgetxattr),      // 230
   LINXY(__NR_fgetxattr,         sys_fgetxattr),      // 231
   LINXY(__NR_listxattr,         sys_listxattr),      // 232
   LINXY(__NR_llistxattr,        sys_llistxattr),     // 233
   LINXY(__NR_flistxattr,        sys_flistxattr),     // 234

   LINX_(__NR_removexattr,       sys_removexattr),    // 235
   LINX_(__NR_lremovexattr,      sys_lremovexattr),   // 236
   LINX_(__NR_fremovexattr,      sys_fremovexattr),   // 237
   LINXY(__NR_tkill,             sys_tkill),          // 238 */Linux
   LINXY(__NR_sendfile64,        sys_sendfile64),     // 239

   LINXY(__NR_futex,             sys_futex),             // 240
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 241
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 242
   PLAX_(__NR_set_thread_area,   sys_set_thread_area),   // 243
   PLAX_(__NR_get_thread_area,   sys_get_thread_area),   // 244

   LINXY(__NR_io_setup,          sys_io_setup),       // 245
   LINX_(__NR_io_destroy,        sys_io_destroy),     // 246
   LINXY(__NR_io_getevents,      sys_io_getevents),   // 247
   LINX_(__NR_io_submit,         sys_io_submit),      // 248
   LINXY(__NR_io_cancel,         sys_io_cancel),      // 249

   LINX_(__NR_fadvise64,         sys_fadvise64),      // 250 */(Linux?)
   GENX_(251,                    sys_ni_syscall),     // 251
   LINX_(__NR_exit_group,        sys_exit_group),     // 252
   LINXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 253
   LINXY(__NR_epoll_create,      sys_epoll_create),   // 254

   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 255
   LINXY(__NR_epoll_wait,        sys_epoll_wait),        // 256
//zz    //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 257 */Linux
   LINX_(__NR_set_tid_address,   sys_set_tid_address),   // 258
   LINXY(__NR_timer_create,      sys_timer_create),      // 259

   LINXY(__NR_timer_settime,     sys_timer_settime),  // (timer_create+1)
   LINXY(__NR_timer_gettime,     sys_timer_gettime),  // (timer_create+2)
   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),//(timer_create+3)
   LINX_(__NR_timer_delete,      sys_timer_delete),   // (timer_create+4)
   LINX_(__NR_clock_settime,     sys_clock_settime),  // (timer_create+5)

   LINXY(__NR_clock_gettime,     sys_clock_gettime),  // (timer_create+6)
   LINXY(__NR_clock_getres,      sys_clock_getres),   // (timer_create+7)
   LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),// (timer_create+8) */*
   GENXY(__NR_statfs64,          sys_statfs64),       // 268
   GENXY(__NR_fstatfs64,         sys_fstatfs64),      // 269

   LINX_(__NR_tgkill,            sys_tgkill),         // 270 */Linux
   GENX_(__NR_utimes,            sys_utimes),         // 271
   LINX_(__NR_fadvise64_64,      sys_fadvise64_64),   // 272 */(Linux?)
   GENX_(__NR_vserver,           sys_ni_syscall),     // 273
   LINX_(__NR_mbind,             sys_mbind),          // 274 ?/?

   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),  // 275 ?/?
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),  // 276 ?/?
   LINXY(__NR_mq_open,           sys_mq_open),        // 277
   LINX_(__NR_mq_unlink,         sys_mq_unlink),      // (mq_open+1)
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),   // (mq_open+2)

   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),// (mq_open+3)
   LINX_(__NR_mq_notify,         sys_mq_notify),      // (mq_open+4)
   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // (mq_open+5)
   GENX_(__NR_sys_kexec_load,    sys_ni_syscall),     // 283
   LINXY(__NR_waitid,            sys_waitid),         // 284

   GENX_(285,                    sys_ni_syscall),     // 285
   LINX_(__NR_add_key,           sys_add_key),        // 286
   LINX_(__NR_request_key,       sys_request_key),    // 287
   LINXY(__NR_keyctl,            sys_keyctl),         // 288
   LINX_(__NR_ioprio_set,        sys_ioprio_set),     // 289

   LINX_(__NR_ioprio_get,        sys_ioprio_get),     // 290
   LINX_(__NR_inotify_init,	 sys_inotify_init),   // 291
   LINX_(__NR_inotify_add_watch, sys_inotify_add_watch), // 292
   LINX_(__NR_inotify_rm_watch,	 sys_inotify_rm_watch), // 293
//   LINX_(__NR_migrate_pages,	 sys_migrate_pages),    // 294

   LINXY(__NR_openat,		 sys_openat),           // 295
   LINX_(__NR_mkdirat,		 sys_mkdirat),          // 296
   LINX_(__NR_mknodat,		 sys_mknodat),          // 297
   LINX_(__NR_fchownat,		 sys_fchownat),         // 298
   LINX_(__NR_futimesat,	 sys_futimesat),        // 299

   PLAXY(__NR_fstatat64,	 sys_fstatat64),        // 300
   LINX_(__NR_unlinkat,		 sys_unlinkat),         // 301
   LINX_(__NR_renameat,		 sys_renameat),         // 302
   LINX_(__NR_linkat,		 sys_linkat),           // 303
   LINX_(__NR_symlinkat,	 sys_symlinkat),        // 304

   LINX_(__NR_readlinkat,	 sys_readlinkat),       // 305
   LINX_(__NR_fchmodat,		 sys_fchmodat),         // 306
   LINX_(__NR_faccessat,	 sys_faccessat),        // 307
   LINX_(__NR_pselect6,		 sys_pselect6),         // 308
   LINXY(__NR_ppoll,		 sys_ppoll),            // 309

//   LINX_(__NR_unshare,		 sys_unshare),          // 310
   LINX_(__NR_set_robust_list,	 sys_set_robust_list),  // 311
   LINXY(__NR_get_robust_list,	 sys_get_robust_list),  // 312
   LINX_(__NR_splice,            sys_splice),           // 313
   LINX_(__NR_sync_file_range,   sys_sync_file_range),  // 314

//   LINX_(__NR_tee,               sys_ni_syscall),       // 315
//   LINX_(__NR_vmsplice,          sys_ni_syscall),       // 316
//   LINX_(__NR_move_pages,        sys_ni_syscall),       // 317
   LINXY(__NR_getcpu,            sys_getcpu),           // 318
   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),      // 319

   LINX_(__NR_utimensat,         sys_utimensat),        // 320
   LINXY(__NR_signalfd,          sys_signalfd),         // 321
   LINXY(__NR_timerfd_create,    sys_timerfd_create),   // 322
   LINX_(__NR_eventfd,           sys_eventfd),          // 323
   LINX_(__NR_fallocate,         sys_fallocate),        // 324

   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),  // 325
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),  // 326
   LINXY(__NR_signalfd4,         sys_signalfd4),        // 327
   LINX_(__NR_eventfd2,          sys_eventfd2),         // 328
   LINXY(__NR_epoll_create1,     sys_epoll_create1),     // 329

   LINXY(__NR_dup3,              sys_dup3),             // 330
   LINXY(__NR_pipe2,             sys_pipe2),            // 331
   LINXY(__NR_inotify_init1,     sys_inotify_init1),    // 332
   LINXY(__NR_preadv,            sys_preadv),           // 333
   LINX_(__NR_pwritev,           sys_pwritev),          // 334

   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 335
   LINXY(__NR_perf_counter_open, sys_perf_counter_open) // 336
};

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_table_size
      = sizeof(syscall_table) / sizeof(syscall_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_table_size) {
      SyscallTableEntry* sys = &syscall_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

   /* Can't find a wrapper */
   return NULL;
}

#endif // defined(VGP_x86_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
