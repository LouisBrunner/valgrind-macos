/*--------------------------------------------------------------------*/
/*---                                                 stacktrace.c ---*/
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

#include "core.h"
#include "pub_core_stacktrace.h"

/*------------------------------------------------------------*/
/*--- Exported functions.                                  ---*/
/*------------------------------------------------------------*/

/* Take a snapshot of the client's stack, putting the up to 'n_ips' IPs 
   into 'ips'.  In order to be thread-safe, we pass in the thread's IP
   and FP.  Returns number of IPs put in 'ips'.  */
UInt VG_(get_StackTrace2) ( Addr* ips, UInt n_ips, Addr ip, Addr fp,
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

   if (fp_min + 4000000 <= fp_max) {
      /* If the stack is ridiculously big, don't poke around ... but
         don't bomb out either.  Needed to make John Regehr's
         user-space threads package work. JRS 20021001 */
      ips[0] = ip;
      i = 1;
   } else {
      /* Get whatever we safely can ... */
      ips[0] = ip;
      fp = VGA_FIRST_STACK_FRAME(fp);
      for (i = 1; i < n_ips; i++) {
         if (!(fp_min <= fp && fp <= fp_max)) {
	    if (debug)
	       VG_(printf)("... out of range %p\n", fp);
            break; /* fp gone baaaad */
         }
         // NJN 2002-sep-17: monotonicity doesn't work -- gives wrong traces...
         //     if (fp >= ((UInt*)fp)[0]) {
         //   VG_(printf)("nonmonotonic\n");
         //    break; /* fp gone nonmonotonic */
         // }
         ips[i] = VGA_STACK_FRAME_RET(fp);  /* ret addr */
         fp     = VGA_STACK_FRAME_NEXT(fp);  /* old fp */
	 if (debug)
	    VG_(printf)("     ips[%d]=%08p\n", i, ips[i]);
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
   ThreadState* tst        = & VG_(threads)[ tid ];
   Addr ip                 = INSTR_PTR(tst->arch);
   Addr fp                 = FRAME_PTR(tst->arch);
   Addr sp                 = STACK_PTR(tst->arch);
   Addr stack_highest_word = tst->client_stack_highest_word;

#ifdef __x86__
   /* Nasty little hack to deal with sysinfo syscalls - if libc is
      using the sysinfo page for syscalls (the TLS version does), then
      ip will always appear to be in that page when doing a syscall,
      not the actual libc function doing the syscall.  This check sees
      if IP is within the syscall code, and pops the return address
      off the stack so that ip is placed within the library function
      calling the syscall.  This makes stack backtraces much more
      useful.  */
   if (ip >= VG_(client_trampoline_code)+VG_(tramp_syscall_offset) &&
       ip < VG_(client_trampoline_code)+VG_(trampoline_code_length) &&
       VG_(is_addressable)(sp, sizeof(Addr), VKI_PROT_READ)) {
      ip = *(Addr *)sp;
      sp += sizeof(Addr);
   }
#endif
   if (0)
      VG_(printf)("tid %d: stack_highest=%p ip=%p sp=%p fp=%p\n",
		  tid, stack_highest_word, ip, sp, fp);

   return VG_(get_StackTrace2)(ips, n_ips, ip, fp, sp, stack_highest_word);
}

static void printIpDesc(UInt n, Addr ip)
{
   static UChar buf[VG_ERRTXT_LEN];

   VG_(describe_IP)(ip, buf, VG_ERRTXT_LEN);
   VG_(message)(Vg_UserMsg, "   %s %s", ( n == 0 ? "at" : "by" ), buf);
}

/* Print a StackTrace. */
void VG_(pp_StackTrace) ( StackTrace ips, UInt n_ips )
{
   vg_assert( n_ips > 0 );
   VG_(apply_StackTrace)( printIpDesc, ips, n_ips );
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
         ip -= VGA_MIN_INSTR_SIZE;  // point to calling line

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
