
/*--------------------------------------------------------------------*/
/*--- Assertions and panics.                        m_libcassert.c ---*/
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
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"      // For VG_(gettid)()
#include "pub_core_stacktrace.h"
#include "pub_core_syscall.h"
#include "pub_core_tooliface.h"     // For VG_(details).{name,bug_reports_to}
#include "vki_unistd.h"

/* ---------------------------------------------------------------------
   Assertery.
   ------------------------------------------------------------------ */

#if defined(VGP_x86_linux)
#  define GET_REAL_SP_AND_FP(sp, fp) \
      asm("movl %%esp, %0;" \
          "movl %%ebp, %1;" \
          : "=r" (sp),\
            "=r" (fp));
#elif defined(VGP_amd64_linux)
#  define GET_REAL_SP_AND_FP(sp, fp) \
      asm("movq %%rsp, %0;" \
          "movq %%rbp, %1;" \
          : "=r" (sp),\
            "=r" (fp));
#elif defined(VGP_ppc32_linux)
#  define GET_REAL_SP_AND_FP(sp, fp) \
      asm("mr %0,1;" \
          "mr %1,1;" \
          : "=r" (sp),\
            "=r" (fp));
#else
#  error Unknown platform
#endif

#define BACKTRACE_DEPTH    100         // nice and deep!

/* Pull down the entire world */
void VG_(exit)( Int status )
{
   (void)VG_(do_syscall1)(__NR_exit_group, status );
   (void)VG_(do_syscall1)(__NR_exit, status );
   /* Why are we still alive here? */
   /*NOTREACHED*/
   *(volatile Int *)0 = 'x';
   vg_assert(2+2 == 5);
}

// Print the scheduler status.
static void pp_sched_status ( void )
{
   Int i; 
   VG_(printf)("\nsched status:\n"); 
   VG_(printf)("  running_tid=%d\n", VG_(get_running_tid)());
   for (i = 1; i < VG_N_THREADS; i++) {
      if (VG_(threads)[i].status == VgTs_Empty) continue;
      VG_(printf)( "\nThread %d: status = %s\n", i, 
                   VG_(name_of_ThreadStatus)(VG_(threads)[i].status) );
      VG_(get_and_pp_StackTrace)( i, BACKTRACE_DEPTH );
   }
   VG_(printf)("\n");
}

__attribute__ ((noreturn))
static void report_and_quit ( const Char* report, 
                              Addr ip, Addr sp, Addr fp, Addr lr )
{
   Addr stacktop;
   Addr ips[BACKTRACE_DEPTH];
   ThreadState *tst = VG_(get_ThreadState)( VG_(get_lwp_tid)(VG_(gettid)()) );
 
   // If necessary, fake up an ExeContext which is of our actual real CPU
   // state.  Could cause problems if we got the panic/exception within the
   // execontext/stack dump/symtab code.  But it's better than nothing.
   if (0 == ip && 0 == sp && 0 == fp) {
       ip = (Addr)__builtin_return_address(0);
       GET_REAL_SP_AND_FP(sp, fp);
   }
 
   stacktop = tst->os_state.valgrind_stack_init_SP;
 
   VG_(get_StackTrace2)(ips, BACKTRACE_DEPTH, ip, sp, fp, lr, sp, stacktop);
   VG_(pp_StackTrace)  (ips, BACKTRACE_DEPTH);
 
   // Don't print this, as it's not terribly interesting and avoids a
   // dependence on m_scheduler/, which would be crazy.
   //VG_(printf)("\nBasic block ctr is approximately %llu\n", VG_(bbs_done) );
 
   pp_sched_status();
   VG_(printf)("\n");
   VG_(printf)("Note: see also the FAQ.txt in the source distribution.\n");
   VG_(printf)("It contains workarounds to several common problems.\n");
   VG_(printf)("\n");
   VG_(printf)("If that doesn't help, please report this bug to: %s\n\n", 
               report);
   VG_(printf)("In the bug report, send all the above text, the valgrind\n");
   VG_(printf)("version, and what Linux distro you are using.  Thanks.\n\n");
   VG_(exit)(1);
}

void VG_(assert_fail) ( Bool isCore, const Char* expr, const Char* file, 
                        Int line, const Char* fn, const HChar* format, ... )
{
   va_list vargs;
   Char buf[256];
   Char* component;
   Char* bugs_to;

   static Bool entered = False;
   if (entered) 
     VG_(exit)(2);
   entered = True;

   va_start(vargs, format);
   VG_(vsprintf) ( buf, format, vargs );
   va_end(vargs);

   if (isCore) {
      component = "valgrind";
      bugs_to   = VG_BUGS_TO;
   } else { 
      component = VG_(details).name;
      bugs_to   = VG_(details).bug_reports_to;
   }

   // Treat vg_assert2(0, "foo") specially, as a panicky abort
   if (VG_STREQ(expr, "0")) {
      VG_(printf)("\n%s: %s:%d (%s): the 'impossible' happened.\n",
                  component, file, line, fn, expr );
   } else {
      VG_(printf)("\n%s: %s:%d (%s): Assertion '%s' failed.\n",
                  component, file, line, fn, expr );
   }
   if (!VG_STREQ(buf, ""))
      VG_(printf)("%s: %s\n", component, buf );

   report_and_quit(bugs_to, 0,0,0,0);
}

__attribute__ ((noreturn))
static void panic ( Char* name, Char* report, Char* str,
                    Addr ip, Addr sp, Addr fp, Addr lr )
{
   VG_(printf)("\n%s: the 'impossible' happened:\n   %s\n", name, str);
   report_and_quit(report, ip, sp, fp, lr);
}

void VG_(core_panic_at) ( Char* str, Addr ip, Addr sp, Addr fp, Addr lr )
{
   panic("valgrind", VG_BUGS_TO, str, ip, sp, fp, lr);
}

void VG_(core_panic) ( Char* str )
{
   VG_(core_panic_at)(str, 0,0,0,0);
}

void VG_(tool_panic) ( Char* str )
{
   panic(VG_(details).name, VG_(details).bug_reports_to, str, 0,0,0,0);
}

/* Print some helpful-ish text about unimplemented things, and give up. */
void VG_(unimplemented) ( Char* msg )
{
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, 
      "Valgrind detected that your program requires");
   VG_(message)(Vg_UserMsg, 
      "the following unimplemented functionality:");
   VG_(message)(Vg_UserMsg, "   %s", msg);
   VG_(message)(Vg_UserMsg,
      "This may be because the functionality is hard to implement,");
   VG_(message)(Vg_UserMsg,
      "or because no reasonable program would behave this way,");
   VG_(message)(Vg_UserMsg,
      "or because nobody has yet needed it.  In any case, let us know at");
   VG_(message)(Vg_UserMsg,
      "%s and/or try to work around the problem, if you can.", VG_BUGS_TO);
   VG_(message)(Vg_UserMsg,
      "");
   VG_(message)(Vg_UserMsg,
      "Valgrind has to exit now.  Sorry.  Bye!");
   VG_(message)(Vg_UserMsg,
      "");
   pp_sched_status();
   VG_(exit)(1);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

