
/*--------------------------------------------------------------------*/
/*--- Take snapshots of client stacks.              m_stacktrace.c ---*/
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
#include "pub_core_threadstate.h"
#include "pub_core_debuginfo.h"     // Needed for pub_core_aspacemgr :(
#include "pub_core_aspacemgr.h"     // For VG_(is_addressable)()
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_stacktrace.h"
#include "pub_core_trampoline.h"

/*------------------------------------------------------------*/
/*--- Exported functions.                                  ---*/
/*------------------------------------------------------------*/

// Stack frame layout and linkage
#if defined(VGA_x86)
#  define FIRST_STACK_FRAME(ebp)    (ebp)
#  define STACK_FRAME_RET(ebp)      (((UWord*)ebp)[1])
#  define STACK_FRAME_NEXT(ebp)     (((UWord*)ebp)[0])
#elif defined(VGA_amd64)
#  define FIRST_STACK_FRAME(rbp)    (rbp)
#  define STACK_FRAME_RET(rbp)      (((UWord*)rbp)[1])
#  define STACK_FRAME_NEXT(rbp)     (((UWord*)rbp)[0])
#elif defined(VGA_ppc32)
#  define FIRST_STACK_FRAME(sp)     (sp)
#  define STACK_FRAME_RET(sp)       (((UWord*)sp)[1])
#  define STACK_FRAME_NEXT(sp)      (((UWord*)sp)[0])
#else
#  error Unknown platform
#endif

/* Take a snapshot of the client's stack, putting the up to 'n_ips' IPs 
   into 'ips'.  In order to be thread-safe, we pass in the thread's IP
   and FP.  Returns number of IPs put in 'ips'.  */
UInt VG_(get_StackTrace2) ( Addr* ips, UInt n_ips, 
                            Addr ip, Addr sp, Addr fp,
                            Addr fp_min, Addr fp_max_orig )
{
   static const Bool debug = False;
   Int         i;
   Addr        fp_max;
   UInt        n_found = 0;

   VGP_PUSHCC(VgpExeContext);

   /* First snaffle IPs from the client's stack into ips[0 .. n_ips-1], 
      putting zeroes in when the trail goes cold, which we guess to be when
      FP is not a reasonable stack location.  We also assert that FP
      increases down the chain. */

   // Gives shorter stack trace for tests/badjump.c
   // JRS 2002-aug-16: I don't think this is a big deal; looks ok for
   // most "normal" backtraces.
   // NJN 2002-sep-05: traces for pthreaded programs are particularly bad.

   // JRS 2002-sep-17: hack, to round up fp_max to the end of the
   // current page, at least.  Dunno if it helps.
   // NJN 2002-sep-17: seems to -- stack traces look like 1.0.X again
   fp_max = (fp_max_orig + VKI_PAGE_SIZE - 1) & ~(VKI_PAGE_SIZE - 1);
   fp_max -= sizeof(Addr);

   if (debug)
      VG_(printf)("n_ips=%d fp_min=%p fp_max_orig=%p, fp_max=%p ip=%p fp=%p\n",
		  n_ips, fp_min, fp_max_orig, fp_max, ip, fp);

   /* Assertion broken before main() is reached in pthreaded programs;  the
    * offending stack traces only have one item.  --njn, 2002-aug-16 */
   /* vg_assert(fp_min <= fp_max);*/

   ips[0] = ip;
   i = 1;

   if (fp_min + VG_(clo_max_stackframe) <= fp_max) {
      /* If the stack is ridiculously big, don't poke around ... but
         don't bomb out either.  Needed to make John Regehr's
         user-space threads package work. JRS 20021001 */
   } else {

      // XXX: I think this line is needed for PPC to work, but I'm not
      // certain, so I'm commenting it out for the moment.
      //fp = FIRST_STACK_FRAME(fp)

      while (True) {

         if (i >= n_ips)
            break;

         /* Try to derive a new (ip,sp,fp) triple from the current
            set. */

         /* First off, see if there is any CFI info to hand which can
            be used. */
         if ( VG_(use_CFI_info)( &ip, &sp, &fp, fp_min, fp_max ) ) {
            ips[i++] = ip;
            if (debug)
               VG_(printf)("     ipsC[%d]=%08p\n", i-1, ips[i-1]);
            continue;
	 }

 	 /* If VG_(use_CFI_info) fails, it won't modify ip/sp/fp, so
            we can safely try the old-fashioned method. */
	 /* This bit is supposed to deal with frames resulting from
            functions which begin "pushl% ebp ; movl %esp, %ebp" (x86)
            or "pushq %rbp ; movq %rsp, %rbp" (amd64).  Unfortunately,
            since we can't (easily) look at the insns at the start of
            the fn, like GDB does, there's no reliable way to tell.
            Hence the hack of first trying out CFI, and if that fails,
            then use this as a fallback. */
         if (fp_min <= fp && fp <= fp_max) {
            /* fp looks sane, so use it. */
            ip = STACK_FRAME_RET(fp);
            sp = fp + sizeof(Addr) /*saved %ebp/%rbp*/ 
                    + sizeof(Addr) /*ra*/;
            fp = STACK_FRAME_NEXT(fp);
            ips[i++] = ip;
            if (debug)
               VG_(printf)("     ipsF[%d]=%08p\n", i-1, ips[i-1]);
            continue;
	 }

         /* No luck there.  We have to give up. */
         break;
      }

   }
   n_found = i;

   /* Put zeroes in the rest. */
   for (;  i < n_ips; i++) {
      ips[i] = 0;
   }
   VGP_POPCC(VgpExeContext);

   return n_found;
}

UInt VG_(get_StackTrace) ( ThreadId tid, StackTrace ips, UInt n_ips )
{
   /* thread in thread table */
   Addr ip                 = VG_(get_IP)(tid);
   Addr fp                 = VG_(get_FP)(tid);
   Addr sp                 = VG_(get_SP)(tid);
   Addr stack_highest_word = VG_(threads)[tid].client_stack_highest_word;

#  if defined(VGP_x86_linux)
   /* Nasty little hack to deal with sysinfo syscalls - if libc is
      using the sysinfo page for syscalls (the TLS version does), then
      ip will always appear to be in that page when doing a syscall,
      not the actual libc function doing the syscall.  This check sees
      if IP is within the syscall code, and pops the return address
      off the stack so that ip is placed within the library function
      calling the syscall.  This makes stack backtraces much more
      useful.  */
   if (ip >= (Addr)&VG_(trampoline_stuff_start) 
       && ip < (Addr)&VG_(trampoline_stuff_end)
       &&  VG_(is_addressable)(sp, sizeof(Addr), VKI_PROT_READ)) {
      ip = *(Addr *)sp;
      sp += sizeof(Addr);
   }
#  endif

   if (0)
      VG_(printf)("tid %d: stack_highest=%p ip=%p sp=%p fp=%p\n",
		  tid, stack_highest_word, ip, sp, fp);

   return VG_(get_StackTrace2)(ips, n_ips, ip, sp, fp, sp, stack_highest_word);
}

static void printIpDesc(UInt n, Addr ip)
{
   #define BUF_LEN   4096
   
   static UChar buf[BUF_LEN];

   VG_(describe_IP)(ip, buf, BUF_LEN);

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "    %s", buf);
   } else {
      VG_(message)(Vg_UserMsg, "   %s %s", ( n == 0 ? "at" : "by" ), buf);
   }
}

/* Print a StackTrace. */
void VG_(pp_StackTrace) ( StackTrace ips, UInt n_ips )
{
   vg_assert( n_ips > 0 );

   if (VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "  <stack>");

   VG_(apply_StackTrace)( printIpDesc, ips, n_ips );

   if (VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "  </stack>");
}

/* Get and immediately print a StackTrace. */
void VG_(get_and_pp_StackTrace) ( ThreadId tid, UInt n_ips )
{
   Addr ips[n_ips];
   VG_(get_StackTrace)(tid, ips, n_ips);
   VG_(pp_StackTrace) (     ips, n_ips);
}


void VG_(apply_StackTrace)( void(*action)(UInt n, Addr ip),
                            StackTrace ips, UInt n_ips )
{
   #define MYBUF_LEN 10    // only needs to be long enough for "main"

   Bool main_done = False;
   Char mybuf[MYBUF_LEN];     // ok to stack allocate mybuf[] -- it's tiny
   Int i = 0;

   vg_assert(n_ips > 0);
   do {
      Addr ip = ips[i];
      if (i > 0) 
         ip -= VGA_MIN_INSTR_SZB;   // point to calling line

      // Stop after "main";  if main() is recursive, stop after last main().
      if ( ! VG_(clo_show_below_main)) {
         VG_(get_fnname_nodemangle)( ip, mybuf, MYBUF_LEN );
         if ( VG_STREQ("main", mybuf) )
            main_done = True;
         else if (main_done)
            break;
      }

      // Act on the ip
      action(i, ip);

      i++;
   } while (i < n_ips && ips[i] != 0);

   #undef MYBUF_LEN
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
