
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

/* Global and Local descriptor tables for threads.

   See comments in libvex_guest_x86.h for LibVEX's model of x86
   segment descriptors.

   Mostly, threads never generate LDT (or GDT?) entries.  Therefore,
   we will initially start off with LDTs and GDTs being (HWord)NULL
   and allocate them on demand.
*/


/*------------------------------------------------------------*/
/*--- Determining arch/subarch.                            ---*/
/*------------------------------------------------------------*/

/* Standard macro to see if a specific flag is changeable */

static inline Bool flag_is_changeable(UInt flag)
{
   UInt f1, f2;

   asm("pushfl\n\t"
       "pushfl\n\t"
       "popl %0\n\t"
       "movl %0,%1\n\t"
       "xorl %2,%0\n\t"
       "pushl %0\n\t"
       "popfl\n\t"
       "pushfl\n\t"
       "popl %0\n\t"
       "popfl\n\t"
       : "=&r" (f1), "=&r" (f2)
       : "ir" (flag));

   return ((f1^f2) & flag) != 0;
}

/* Does this CPU support the CPUID instruction? */

static Bool has_cpuid ( void )
{
   /* 21 is the ID flag */
   return flag_is_changeable(1<<21);
}

/* Actually do a CPUID on the host. */

static void do_cpuid ( UInt n,
                       UInt* a, UInt* b,
                       UInt* c, UInt* d )
{
   __asm__ __volatile__ (
      "cpuid"
      : "=a" (*a), "=b" (*b), "=c" (*c), "=d" (*d)      /* output */
      : "0" (n)         /* input */
   );
}


// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*    vex_arch, 
                              /*OUT*/VexSubArch* vex_subarch )
{
   Bool have_sse0, have_sse1, have_sse2;
   UInt eax, ebx, ecx, edx;

   if (!has_cpuid())
      /* we can't do cpuid at all.  Give up. */
      return False;

   do_cpuid(0, &eax, &ebx, &ecx, &edx);
   if (eax < 1)
     /* we can't ask for cpuid(x) for x > 0.  Give up. */
     return False;

   /* get capabilities bits into edx */
   do_cpuid(1, &eax, &ebx, &ecx, &edx);

   have_sse0 = (edx & (1<<24)) != 0; /* True => have fxsave/fxrstor */
   have_sse1 = (edx & (1<<25)) != 0; /* True => have sse insns */
   have_sse2 = (edx & (1<<26)) != 0; /* True => have sse2 insns */

   if (have_sse2 && have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse2;
      return True;
   }

   if (have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse1;
      return True;
   }

   if (have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse0;
      return True;
   }

   /* we need at least SSE state to operate. */
   return False;
}


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
}


/*------------------------------------------------------------*/
/*--- LDT/GDT stuff                                        ---*/
/*------------------------------------------------------------*/

/* Create a zeroed-out GDT. */

VexGuestX86SegDescr* VG_(alloc_zeroed_x86_GDT) ( void )
{
   Int nbytes 
      = VEX_GUEST_X86_GDT_NENT * sizeof(VexGuestX86SegDescr);
   return
      VG_(arena_calloc)(VG_AR_CORE, VG_MIN_MALLOC_SZB, nbytes, 1);
}

/* Create a zeroed-out LDT. */

VexGuestX86SegDescr* VG_(alloc_zeroed_x86_LDT) ( void )
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
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow = parent->vex_shadow;
   /* We inherit our parent's LDT. */
   if (parent->vex.guest_LDT == (HWord)NULL) {
      /* We hope this is the common case. */
      child->vex.guest_LDT = (HWord)NULL;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      child->vex.guest_LDT = (HWord)VG_(alloc_zeroed_x86_LDT)();
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


void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr))
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ThreadArchState *arch = &tst->arch;

   /* XXX ask tool about validity? */
   (*marker)(arch->vex.guest_EAX);
   (*marker)(arch->vex.guest_ECX);
   (*marker)(arch->vex.guest_EDX);
   (*marker)(arch->vex.guest_EBX);
   (*marker)(arch->vex.guest_ESI);
   (*marker)(arch->vex.guest_EDI);
   (*marker)(arch->vex.guest_ESP);
   (*marker)(arch->vex.guest_EBP);
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

