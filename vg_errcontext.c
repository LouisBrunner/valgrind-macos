
/*--------------------------------------------------------------------*/
/*--- Management of error messages.                vg_errcontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "vg_constants.h"


/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* Suppression is a type describing an error which we want to
   suppress, ie, not show the user, usually because it is caused by a
   problem in a library which we can't fix, replace or work around.
   Suppressions are read from a file at startup time, specified by
   vg_clo_suppressions, and placed in the vg_suppressions list.  This
   gives flexibility so that new suppressions can be added to the file
   as and when needed. 
*/
typedef 
   enum { 
      /* Bad syscall params */
      Param, 
      /* Use of invalid values of given size */
      Value0, Value1, Value2, Value4, Value8, 
      /* Invalid read/write attempt at given size */
      Addr1, Addr2, Addr4, Addr8,
      /* Invalid or mismatching free */
      FreeS,
      /* Pthreading error */
      PThread
   } 
   SuppressionKind;


/* For each caller specified for a suppression, record the nature of
   the caller name. */
typedef
   enum { 
      /* Name is of an shared object file. */
      ObjName,
      /* Name is of a function. */
      FunName 
   }
   SuppressionLocTy;


/* A complete suppression record. */
typedef
   struct _Suppression {
      struct _Suppression* next;
      /* The number of times this error has been suppressed. */
      Int count;
      /* The name by which the suppression is referred to. */
      Char* sname;
      /* What kind of suppression. */
      SuppressionKind skind;
      /* Name of syscall param if skind==Param */
      Char* param;
      /* Name of fn where err occurs, and immediate caller (mandatory). */
      SuppressionLocTy caller0_ty;
      Char*            caller0;
      SuppressionLocTy caller1_ty;
      Char*            caller1;
      /* Optional extra callers. */
      SuppressionLocTy caller2_ty;
      Char*            caller2;
      SuppressionLocTy caller3_ty;
      Char*            caller3;
   } 
   Suppression;


/* ErrContext is a type for recording just enough info to generate an
   error report for an illegal memory access.  The idea is that
   (typically) the same few points in the program generate thousands
   of illegal accesses, and we don't want to spew out a fresh error
   message for each one.  Instead, we use these structures to common
   up duplicates.  
*/

/* What kind of error it is. */
typedef 
   enum { ValueErr, AddrErr, 
          ParamErr, UserErr, /* behaves like an anonymous ParamErr */
          FreeErr, FreeMismatchErr,
          PThreadErr /* pthread API error */
   }
   ErrKind;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Top-level struct for recording errors. */
typedef
   struct _ErrContext {
      /* ALL */
      struct _ErrContext* next;
      /* ALL */
      /* NULL if unsuppressed; or ptr to suppression record. */
      Suppression* supp;
      /* ALL */
      Int count;
      /* ALL */
      ErrKind ekind;
      /* ALL */
      ExeContext* where;
      /* Addr */
      AxsKind axskind;
      /* Addr, Value */
      Int size;
      /* Addr, Free, Param, User */
      Addr addr;
      /* Addr, Free, Param, User */
      AddrInfo addrinfo;
      /* Param; hijacked for PThread as a description */
      Char* syscall_param;
      /* Param, User */
      Bool isWriteableLack;
      /* ALL */
      ThreadId tid;
      /* ALL */
      /* These record %EIP, %ESP and %EBP at the error point.  They
         are only used to make GDB-attaching convenient; there is no
         other purpose; specifically they are not used to do
         comparisons between errors. */
      UInt m_eip;
      UInt m_esp;
      UInt m_ebp;
   } 
   ErrContext;

/* The list of error contexts found, both suppressed and unsuppressed.
   Initially empty, and grows as errors are detected. */
static ErrContext* vg_err_contexts = NULL;

/* The list of suppression directives, as read from the specified
   suppressions file. */
static Suppression* vg_suppressions = NULL;

/* Running count of unsuppressed errors detected. */
static UInt vg_n_errs_found = 0;

/* Running count of suppressed errors detected. */
static UInt vg_n_errs_suppressed = 0;

/* Used to disable further error reporting once some huge number of
   errors have already been logged. */
static Bool vg_ignore_errors = False;

/* forwards ... */
static Suppression* is_suppressible_error ( ErrContext* ec );


/*------------------------------------------------------------*/
/*--- Helper fns                                           ---*/
/*------------------------------------------------------------*/


static void clear_AddrInfo ( AddrInfo* ai )
{
   ai->akind      = Unknown;
   ai->blksize    = 0;
   ai->rwoffset   = 0;
   ai->lastchange = NULL;
   ai->stack_tid  = VG_INVALID_THREADID;
   ai->maybe_gcc  = False;
}

static void clear_ErrContext ( ErrContext* ec )
{
   ec->next    = NULL;
   ec->supp    = NULL;
   ec->count   = 0;
   ec->ekind   = ValueErr;
   ec->where   = NULL;
   ec->axskind = ReadAxs;
   ec->size    = 0;
   ec->addr    = 0;
   clear_AddrInfo ( &ec->addrinfo );
   ec->syscall_param   = NULL;
   ec->isWriteableLack = False;
   ec->m_eip   = 0xDEADB00F;
   ec->m_esp   = 0xDEADBE0F;
   ec->m_ebp   = 0xDEADB0EF;
   ec->tid     = VG_INVALID_THREADID;
}


static __inline__
Bool vg_eq_ExeContext ( Bool top_2_only,
                        ExeContext* e1, ExeContext* e2 )
{
   /* Note that frames after the 4th are always ignored. */
   if (top_2_only) {
      return VG_(eq_ExeContext_top2(e1, e2));
   } else {
      return VG_(eq_ExeContext_top4(e1, e2));
   }
}


static Bool eq_AddrInfo ( Bool cheap_addr_cmp,
                          AddrInfo* ai1, AddrInfo* ai2 )
{
   if (ai1->akind != Undescribed 
       && ai2->akind != Undescribed
       && ai1->akind != ai2->akind) 
      return False;
   if (ai1->akind == Freed || ai1->akind == Mallocd) {
      if (ai1->blksize != ai2->blksize)
         return False;
      if (!vg_eq_ExeContext(cheap_addr_cmp, 
                            ai1->lastchange, ai2->lastchange))
         return False;
   }
   return True;
}

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */

static Bool eq_ErrContext ( Bool cheap_addr_cmp,
                            ErrContext* e1, ErrContext* e2 )
{
   if (e1->ekind != e2->ekind) 
      return False;
   if (!vg_eq_ExeContext(cheap_addr_cmp, e1->where, e2->where))
      return False;

   switch (e1->ekind) {
      case PThreadErr:
         if (e1->syscall_param == e2->syscall_param) 
            return True;
         if (0 == VG_(strcmp)(e1->syscall_param, e2->syscall_param))
            return True;
         return False;
      case UserErr:
      case ParamErr:
         if (e1->isWriteableLack != e2->isWriteableLack) return False;
         if (e1->ekind == ParamErr 
             && 0 != VG_(strcmp)(e1->syscall_param, e2->syscall_param))
            return False;
         return True;
      case FreeErr:
      case FreeMismatchErr:
         if (e1->addr != e2->addr) return False;
         if (!eq_AddrInfo(cheap_addr_cmp, &e1->addrinfo, &e2->addrinfo)) 
            return False;
         return True;
      case AddrErr:
         if (e1->axskind != e2->axskind) return False;
         if (e1->size != e2->size) return False;
         if (!eq_AddrInfo(cheap_addr_cmp, &e1->addrinfo, &e2->addrinfo)) 
            return False;
         return True;
      case ValueErr:
         if (e1->size != e2->size) return False;
         return True;
      default: 
         VG_(panic)("eq_ErrContext");
   }
}

static void pp_AddrInfo ( Addr a, AddrInfo* ai )
{
   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      "   Address 0x%x is on thread %d's stack", 
                      a, ai->stack_tid);
         break;
      case Unknown:
         if (ai->maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is just below %%esp.  Possibly a bug in GCC/G++",
               a);
            VG_(message)(Vg_UserMsg, 
               "   v 2.96 or 3.0.X.  To suppress, use: --workaround-gcc296-bugs=yes");
	 } else {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is not stack'd, malloc'd or free'd", a);
         }
         break;
      case Freed: case Mallocd: case UserG: case UserS: {
         UInt delta;
         UChar* relative;
         if (ai->rwoffset < 0) {
            delta    = (UInt)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
         if (ai->akind == UserS) {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is %d bytes %s a %d-byte stack red-zone created",
               a, delta, relative, 
               ai->blksize );
	 } else {
            VG_(message)(Vg_UserMsg, 
               "   Address 0x%x is %d bytes %s a block of size %d %s",
               a, delta, relative, 
               ai->blksize,
               ai->akind==Mallocd ? "alloc'd" 
                  : ai->akind==Freed ? "free'd" 
                                     : "client-defined");
         }
         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }
      default:
         VG_(panic)("pp_AddrInfo");
   }
}

static void pp_ErrContext ( ErrContext* ec, Bool printCount )
{
   if (printCount)
      VG_(message)(Vg_UserMsg, "Observed %d times:", ec->count );
   if (ec->tid > 1)
      VG_(message)(Vg_UserMsg, "Thread %d:", ec->tid );
   switch (ec->ekind) {
      case ValueErr:
         if (ec->size == 0) {
             VG_(message)(
                Vg_UserMsg,
                "Conditional jump or move depends on uninitialised value(s)");
         } else {
             VG_(message)(Vg_UserMsg,
                          "Use of uninitialised value of size %d",
                          ec->size);
         }
         VG_(pp_ExeContext)(ec->where);
         break;
      case AddrErr:
         switch (ec->axskind) {
            case ReadAxs:
               VG_(message)(Vg_UserMsg, "Invalid read of size %d", 
                                        ec->size ); 
               break;
            case WriteAxs:
               VG_(message)(Vg_UserMsg, "Invalid write of size %d", 
                                        ec->size ); 
               break;
            case ExecAxs:
               VG_(message)(Vg_UserMsg, "Jump to the invalid address "
                                        "stated on the next line");
               break;
            default: 
               VG_(panic)("pp_ErrContext(axskind)");
         }
         VG_(pp_ExeContext)(ec->where);
         pp_AddrInfo(ec->addr, &ec->addrinfo);
         break;
      case FreeErr:
         VG_(message)(Vg_UserMsg,"Invalid free() / delete / delete[]");
         /* fall through */
      case FreeMismatchErr:
         if (ec->ekind == FreeMismatchErr)
            VG_(message)(Vg_UserMsg, 
                         "Mismatched free() / delete / delete []");
         VG_(pp_ExeContext)(ec->where);
         pp_AddrInfo(ec->addr, &ec->addrinfo);
         break;
      case ParamErr:
         if (ec->isWriteableLack) {
            VG_(message)(Vg_UserMsg, 
               "Syscall param %s contains unaddressable byte(s)",
                ec->syscall_param );
         } else {
            VG_(message)(Vg_UserMsg, 
                "Syscall param %s contains uninitialised or "
                "unaddressable byte(s)",
            ec->syscall_param);
         }
         VG_(pp_ExeContext)(ec->where);
         pp_AddrInfo(ec->addr, &ec->addrinfo);
         break;
      case UserErr:
         if (ec->isWriteableLack) {
            VG_(message)(Vg_UserMsg, 
               "Unaddressable byte(s) found during client check request");
         } else {
            VG_(message)(Vg_UserMsg, 
               "Uninitialised or "
               "unaddressable byte(s) found during client check request");
         }
         VG_(pp_ExeContext)(ec->where);
         pp_AddrInfo(ec->addr, &ec->addrinfo);
         break;
      case PThreadErr:
         VG_(message)(Vg_UserMsg, "%s", ec->syscall_param );
         VG_(pp_ExeContext)(ec->where);
         break;
      default: 
         VG_(panic)("pp_ErrContext");
   }
}


/* Figure out if we want to attach for GDB for this error, possibly
   by asking the user. */
static
Bool vg_is_GDB_attach_requested ( void )
{
   Char ch, ch2;
   Int res;

   if (VG_(clo_GDB_attach) == False)
      return False;

   VG_(message)(Vg_UserMsg, "");

  again:
   VG_(printf)(
      "==%d== "
      "---- Attach to GDB ? --- [Return/N/n/Y/y/C/c] ---- ", 
      VG_(getpid)()
   );

   res = VG_(read)(0 /*stdin*/, &ch, 1);
   if (res != 1) goto ioerror;
   /* res == 1 */
   if (ch == '\n') return False;
   if (ch != 'N' && ch != 'n' && ch != 'Y' && ch != 'y' 
      && ch != 'C' && ch != 'c') goto again;

   res = VG_(read)(0 /*stdin*/, &ch2, 1);
   if (res != 1) goto ioerror;
   if (ch2 != '\n') goto again;

   /* No, don't want to attach. */
   if (ch == 'n' || ch == 'N') return False;
   /* Yes, want to attach. */
   if (ch == 'y' || ch == 'Y') return True;
   /* No, don't want to attach, and don't ask again either. */
   vg_assert(ch == 'c' || ch == 'C');

  ioerror:
   VG_(clo_GDB_attach) = False;
   return False;
}


/* Top-level entry point to the error management subsystem.  All
   detected errors are notified here; this routine decides if/when the
   user should see the error. */
static void VG_(maybe_add_context) ( ErrContext* ec )
{
   ErrContext* p;
   ErrContext* p_prev;
   Bool        cheap_addr_cmp         = False;
   static Bool is_first_shown_context = True;
   static Bool stopping_message       = False;
   static Bool slowdown_message       = False;
   static Int  vg_n_errs_shown        = 0;

   vg_assert(ec->tid >= 0 && ec->tid < VG_N_THREADS);

   /* After M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN different errors have
      been found, or M_VG_COLLECT_NO_ERRORS_AFTER_FOUND total errors
      have been found, just refuse to collect any more.  This stops
      the burden of the error-management system becoming excessive in
      extremely buggy programs, although it does make it pretty
      pointless to continue the Valgrind run after this point. */
   if (VG_(clo_error_limit) 
       && (vg_n_errs_shown >= M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN
           || vg_n_errs_found >= M_VG_COLLECT_NO_ERRORS_AFTER_FOUND)) {
      if (!stopping_message) {
         VG_(message)(Vg_UserMsg, "");

	 if (vg_n_errs_shown >= M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN) {
            VG_(message)(Vg_UserMsg, 
               "More than %d different errors detected.  "
               "I'm not reporting any more.",
               M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN );
         } else {
            VG_(message)(Vg_UserMsg, 
               "More than %d total errors detected.  "
               "I'm not reporting any more.",
               M_VG_COLLECT_NO_ERRORS_AFTER_FOUND );
	 }

         VG_(message)(Vg_UserMsg, 
            "Final error counts will be inaccurate.  Go fix your program!");
         VG_(message)(Vg_UserMsg, 
            "Rerun with --error-limit=no to disable this cutoff.  Note");
         VG_(message)(Vg_UserMsg, 
            "that your program may now segfault without prior warning from");
         VG_(message)(Vg_UserMsg, 
            "Valgrind, because errors are no longer being displayed.");
         VG_(message)(Vg_UserMsg, "");
         stopping_message = True;
         vg_ignore_errors = True;
      }
      return;
   }

   /* After M_VG_COLLECT_ERRORS_SLOWLY_AFTER different errors have
      been found, be much more conservative about collecting new
      ones. */
   if (vg_n_errs_shown >= M_VG_COLLECT_ERRORS_SLOWLY_AFTER) {
      cheap_addr_cmp = True;
      if (!slowdown_message) {
         VG_(message)(Vg_UserMsg, "");
         VG_(message)(Vg_UserMsg, 
            "More than %d errors detected.  Subsequent errors",
            M_VG_COLLECT_ERRORS_SLOWLY_AFTER);
         VG_(message)(Vg_UserMsg, 
            "will still be recorded, but in less detail than before.");
         slowdown_message = True;
      }
   }


   /* First, see if we've got an error record matching this one. */
   p      = vg_err_contexts;
   p_prev = NULL;
   while (p != NULL) {
      if (eq_ErrContext(cheap_addr_cmp, p, ec)) {
         /* Found it. */
         p->count++;
	 if (p->supp != NULL) {
            /* Deal correctly with suppressed errors. */
            p->supp->count++;
            vg_n_errs_suppressed++;	 
         } else {
            vg_n_errs_found++;
         }

         /* Move p to the front of the list so that future searches
            for it are faster. */
         if (p_prev != NULL) {
            vg_assert(p_prev->next == p);
            p_prev->next    = p->next;
            p->next         = vg_err_contexts;
            vg_err_contexts = p;
	 }
         return;
      }
      p_prev = p;
      p      = p->next;
   }

   /* Didn't see it.  Copy and add. */

   /* OK, we're really going to collect it.  First, describe any addr
      info in the error. */
   if (ec->addrinfo.akind == Undescribed)
      VG_(describe_addr) ( ec->addr, &ec->addrinfo );

   p = VG_(malloc)(VG_AR_ERRCTXT, sizeof(ErrContext));
   *p = *ec;
   p->next = vg_err_contexts;
   p->supp = is_suppressible_error(ec);
   vg_err_contexts = p;
   if (p->supp == NULL) {
      vg_n_errs_found++;
      if (!is_first_shown_context)
         VG_(message)(Vg_UserMsg, "");
      pp_ErrContext(p, False);      
      is_first_shown_context = False;
      vg_n_errs_shown++;
      /* Perhaps we want a GDB attach at this point? */
      if (vg_is_GDB_attach_requested()) {
         VG_(swizzle_esp_then_start_GDB)(
            ec->m_eip, ec->m_esp, ec->m_ebp);
      }
   } else {
      vg_n_errs_suppressed++;
      p->supp->count++;
   }
}




/*------------------------------------------------------------*/
/*--- Exported fns                                         ---*/
/*------------------------------------------------------------*/

/* These two are called from generated code, so that the %EIP/%EBP
   values that we need in order to create proper error messages are
   picked up out of VG_(baseBlock) rather than from the thread table
   (vg_threads in vg_scheduler.c). */

void VG_(record_value_error) ( Int size )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count = 1;
   ec.next  = NULL;
   ec.where = VG_(get_ExeContext)( False, VG_(baseBlock)[VGOFF_(m_eip)], 
                                          VG_(baseBlock)[VGOFF_(m_ebp)] );
   ec.ekind = ValueErr;
   ec.size  = size;
   ec.tid   = VG_(get_current_tid)();
   ec.m_eip = VG_(baseBlock)[VGOFF_(m_eip)];
   ec.m_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   ec.m_ebp = VG_(baseBlock)[VGOFF_(m_ebp)];
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_address_error) ( Addr a, Int size, Bool isWrite )
{
   ErrContext ec;
   Bool       just_below_esp;
   if (vg_ignore_errors) return;

   just_below_esp 
      = VG_(is_just_below_ESP)( VG_(baseBlock)[VGOFF_(m_esp)], a );

   /* If this is caused by an access immediately below %ESP, and the
      user asks nicely, we just ignore it. */
   if (VG_(clo_workaround_gcc296_bugs) && just_below_esp)
      return;

   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, VG_(baseBlock)[VGOFF_(m_eip)], 
                                            VG_(baseBlock)[VGOFF_(m_ebp)] );
   ec.ekind   = AddrErr;
   ec.axskind = isWrite ? WriteAxs : ReadAxs;
   ec.size    = size;
   ec.addr    = a;
   ec.tid     = VG_(get_current_tid)();
   ec.m_eip = VG_(baseBlock)[VGOFF_(m_eip)];
   ec.m_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   ec.m_ebp = VG_(baseBlock)[VGOFF_(m_ebp)];
   ec.addrinfo.akind     = Undescribed;
   ec.addrinfo.maybe_gcc = just_below_esp;
   VG_(maybe_add_context) ( &ec );
}


/* These five are called not from generated code but in response to
   requests passed back to the scheduler.  So we pick up %EIP/%EBP
   values from the stored thread state, not from VG_(baseBlock).  */

void VG_(record_free_error) ( ThreadState* tst, Addr a )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, tst->m_eip, tst->m_ebp );
   ec.ekind   = FreeErr;
   ec.addr    = a;
   ec.tid     = tst->tid;
   ec.m_eip   = tst->m_eip;
   ec.m_esp   = tst->m_esp;
   ec.m_ebp   = tst->m_ebp;
   ec.addrinfo.akind = Undescribed;
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_freemismatch_error) ( ThreadState* tst, Addr a )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, tst->m_eip, tst->m_ebp );
   ec.ekind   = FreeMismatchErr;
   ec.addr    = a;
   ec.tid     = tst->tid;
   ec.m_eip   = tst->m_eip;
   ec.m_esp   = tst->m_esp;
   ec.m_ebp   = tst->m_ebp;
   ec.addrinfo.akind = Undescribed;
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_jump_error) ( ThreadState* tst, Addr a )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, tst->m_eip, tst->m_ebp );
   ec.ekind   = AddrErr;
   ec.axskind = ExecAxs;
   ec.addr    = a;
   ec.tid     = tst->tid;
   ec.m_eip   = tst->m_eip;
   ec.m_esp   = tst->m_esp;
   ec.m_ebp   = tst->m_ebp;
   ec.addrinfo.akind = Undescribed;
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_param_err) ( ThreadState* tst, Addr a, Bool isWriteLack, 
                             Char* msg )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, tst->m_eip, tst->m_ebp );
   ec.ekind   = ParamErr;
   ec.addr    = a;
   ec.tid     = tst->tid;
   ec.m_eip   = tst->m_eip;
   ec.m_esp   = tst->m_esp;
   ec.m_ebp   = tst->m_ebp;
   ec.addrinfo.akind = Undescribed;
   ec.syscall_param = msg;
   ec.isWriteableLack = isWriteLack;
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_user_err) ( ThreadState* tst, Addr a, Bool isWriteLack )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, tst->m_eip, tst->m_ebp );
   ec.ekind   = UserErr;
   ec.addr    = a;
   ec.tid     = tst->tid;
   ec.m_eip   = tst->m_eip;
   ec.m_esp   = tst->m_esp;
   ec.m_ebp   = tst->m_ebp;
   ec.addrinfo.akind = Undescribed;
   ec.isWriteableLack = isWriteLack;
   VG_(maybe_add_context) ( &ec );
}

void VG_(record_pthread_err) ( ThreadId tid, Char* msg )
{
   ErrContext ec;
   if (vg_ignore_errors) return;
   if (!VG_(clo_instrument)) return;
   clear_ErrContext( &ec );
   ec.count   = 1;
   ec.next    = NULL;
   ec.where   = VG_(get_ExeContext)( False, VG_(threads)[tid].m_eip, 
                                            VG_(threads)[tid].m_ebp );
   ec.ekind   = PThreadErr;
   ec.tid     = tid;
   ec.syscall_param = msg;
   ec.m_eip   = VG_(threads)[tid].m_eip;
   ec.m_esp   = VG_(threads)[tid].m_esp;
   ec.m_ebp   = VG_(threads)[tid].m_ebp;
   VG_(maybe_add_context) ( &ec );
}


/*------------------------------*/

void VG_(show_all_errors) ( void )
{
   Int         i, n_min;
   Int         n_err_contexts, n_supp_contexts;
   ErrContext  *p, *p_min;
   Suppression *su;
   Bool        any_supp;

   if (VG_(clo_verbosity) == 0)
      return;

   n_err_contexts = 0;
   for (p = vg_err_contexts; p != NULL; p = p->next) {
      if (p->supp == NULL)
         n_err_contexts++;
   }

   n_supp_contexts = 0;
   for (su = vg_suppressions; su != NULL; su = su->next) {
      if (su->count > 0)
         n_supp_contexts++;
   }

   VG_(message)(Vg_UserMsg,
                "ERROR SUMMARY: "
                "%d errors from %d contexts (suppressed: %d from %d)",
                vg_n_errs_found, n_err_contexts, 
                vg_n_errs_suppressed, n_supp_contexts );

   if (VG_(clo_verbosity) <= 1)
      return;

   /* Print the contexts in order of increasing error count. */
   for (i = 0; i < n_err_contexts; i++) {
      n_min = (1 << 30) - 1;
      p_min = NULL;
      for (p = vg_err_contexts; p != NULL; p = p->next) {
         if (p->supp != NULL) continue;
         if (p->count < n_min) {
            n_min = p->count;
            p_min = p;
         }
      }
      if (p_min == NULL) VG_(panic)("pp_AllErrContexts");

      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "%d errors in context %d of %d:",
                   p_min->count,
                   i+1, n_err_contexts);
      pp_ErrContext( p_min, False );

      if ((i+1 == VG_(clo_dump_error))) {
	VG_(translate) ( 0 /* dummy ThreadId; irrelevant due to below NULLs */,
                         p_min->where->eips[0], NULL, NULL, NULL );
      }

      p_min->count = 1 << 30;
   } 

   if (n_supp_contexts > 0) 
      VG_(message)(Vg_DebugMsg, "");
   any_supp = False;
   for (su = vg_suppressions; su != NULL; su = su->next) {
      if (su->count > 0) {
         any_supp = True;
         VG_(message)(Vg_DebugMsg, "supp: %4d %s", su->count, 
                                   su->sname);
      }
   }

   if (n_err_contexts > 0) {
      if (any_supp) 
         VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg,
                   "IN SUMMARY: "
                   "%d errors from %d contexts (suppressed: %d from %d)",
                   vg_n_errs_found, n_err_contexts, 
                   vg_n_errs_suppressed,
                   n_supp_contexts );
      VG_(message)(Vg_UserMsg, "");
   }
}

/*------------------------------------------------------------*/
/*--- Standard suppressions                                ---*/
/*------------------------------------------------------------*/

/* Get a non-blank, non-comment line of at most nBuf chars from fd.
   Skips leading spaces on the line. Return True if EOF was hit instead. 
*/

#define VG_ISSPACE(ch) (((ch)==' ') || ((ch)=='\n') || ((ch)=='\t'))

static Bool getLine ( Int fd, Char* buf, Int nBuf )
{
   Char ch;
   Int  n, i;
   while (True) {
      /* First, read until a non-blank char appears. */
      while (True) {
         n = VG_(read)(fd, &ch, 1);
         if (n == 1 && !VG_ISSPACE(ch)) break;
         if (n == 0) return True;
      }

      /* Now, read the line into buf. */
      i = 0;
      buf[i++] = ch; buf[i] = 0;
      while (True) {
         n = VG_(read)(fd, &ch, 1);
         if (n == 0) return False; /* the next call will return True */
         if (ch == '\n') break;
         if (i > 0 && i == nBuf-1) i--;
         buf[i++] = ch; buf[i] = 0;
      }
      while (i > 1 && VG_ISSPACE(buf[i-1])) { 
         i--; buf[i] = 0; 
      };

      /* VG_(printf)("The line is `%s'\n", buf); */
      /* Ok, we have a line.  If a non-comment line, return.
         If a comment line, start all over again. */
      if (buf[0] != '#') return False;
   }
}


/* *p_caller contains the raw name of a caller, supposedly either
       fun:some_function_name   or
       obj:some_object_name.
   Set *p_ty accordingly and advance *p_caller over the descriptor
   (fun: or obj:) part.
   Returns False if failed.
*/
static Bool setLocationTy ( Char** p_caller, SuppressionLocTy* p_ty )
{
   if (VG_(strncmp)(*p_caller, "fun:", 4) == 0) {
      (*p_caller) += 4;
      *p_ty = FunName;
      return True;
   }
   if (VG_(strncmp)(*p_caller, "obj:", 4) == 0) {
      (*p_caller) += 4;
      *p_ty = ObjName;
      return True;
   }
   VG_(printf)("location should start with fun: or obj:\n");
   return False;
}


/* Read suppressions from the file specified in vg_clo_suppressions
   and place them in the suppressions list.  If there's any difficulty
   doing this, just give up -- there's no point in trying to recover.  
*/
#define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                      && VG_(strcmp)((s1),(s2))==0)

static Char* copyStr ( Char* str )
{
   Int   n, i;
   Char* str2;
   n    = VG_(strlen)(str);
   str2 = VG_(malloc)(VG_AR_PRIVATE, n+1);
   vg_assert(n > 0);
   for (i = 0; i < n+1; i++) str2[i] = str[i];
   return str2;
}

static void load_one_suppressions_file ( Char* filename )
{
#  define N_BUF 200
   Int  fd;
   Bool eof;
   Char buf[N_BUF+1];
   fd = VG_(open_read)( filename );
   if (fd == -1) {
      VG_(message)(Vg_UserMsg, 
                   "FATAL: can't open suppressions file `%s'", 
                   filename );
      VG_(exit)(1);
   }

   while (True) {
      Suppression* supp;
      supp = VG_(malloc)(VG_AR_PRIVATE, sizeof(Suppression));
      supp->count = 0;
      supp->param = supp->caller0 = supp->caller1 
                  = supp->caller2 = supp->caller3 = NULL;

      eof = getLine ( fd, buf, N_BUF );
      if (eof) break;

      if (!STREQ(buf, "{")) goto syntax_error;
      
      eof = getLine ( fd, buf, N_BUF );
      if (eof || STREQ(buf, "}")) goto syntax_error;
      supp->sname = copyStr(buf);

      eof = getLine ( fd, buf, N_BUF );
      if (eof) goto syntax_error;
      else if (STREQ(buf, "Param"))  supp->skind = Param;
      else if (STREQ(buf, "Value0")) supp->skind = Value0; /* backwards compat */
      else if (STREQ(buf, "Cond"))   supp->skind = Value0;
      else if (STREQ(buf, "Value1")) supp->skind = Value1;
      else if (STREQ(buf, "Value2")) supp->skind = Value2;
      else if (STREQ(buf, "Value4")) supp->skind = Value4;
      else if (STREQ(buf, "Value8")) supp->skind = Value8;
      else if (STREQ(buf, "Addr1"))  supp->skind = Addr1;
      else if (STREQ(buf, "Addr2"))  supp->skind = Addr2;
      else if (STREQ(buf, "Addr4"))  supp->skind = Addr4;
      else if (STREQ(buf, "Addr8"))  supp->skind = Addr8;
      else if (STREQ(buf, "Free"))   supp->skind = FreeS;
      else if (STREQ(buf, "PThread")) supp->skind = PThread;
      else goto syntax_error;

      if (supp->skind == Param) {
         eof = getLine ( fd, buf, N_BUF );
         if (eof) goto syntax_error;
         supp->param = copyStr(buf);
      }

      eof = getLine ( fd, buf, N_BUF );
      if (eof) goto syntax_error;
      supp->caller0 = copyStr(buf);
      if (!setLocationTy(&(supp->caller0), &(supp->caller0_ty)))
         goto syntax_error;

      eof = getLine ( fd, buf, N_BUF );
      if (eof) goto syntax_error;
      if (!STREQ(buf, "}")) {
         supp->caller1 = copyStr(buf);
         if (!setLocationTy(&(supp->caller1), &(supp->caller1_ty)))
            goto syntax_error;
      
         eof = getLine ( fd, buf, N_BUF );
         if (eof) goto syntax_error;
         if (!STREQ(buf, "}")) {
            supp->caller2 = copyStr(buf);
            if (!setLocationTy(&(supp->caller2), &(supp->caller2_ty)))
               goto syntax_error;

            eof = getLine ( fd, buf, N_BUF );
            if (eof) goto syntax_error;
            if (!STREQ(buf, "}")) {
               supp->caller3 = copyStr(buf);
              if (!setLocationTy(&(supp->caller3), &(supp->caller3_ty)))
                 goto syntax_error;

               eof = getLine ( fd, buf, N_BUF );
               if (eof || !STREQ(buf, "}")) goto syntax_error;
	    }
         }
      }

      supp->next = vg_suppressions;
      vg_suppressions = supp;
   }

   VG_(close)(fd);
   return;

  syntax_error:
   if (eof) {
      VG_(message)(Vg_UserMsg, 
                   "FATAL: in suppressions file `%s': unexpected EOF", 
                   filename );
   } else {
      VG_(message)(Vg_UserMsg, 
                   "FATAL: in suppressions file `%s': syntax error on: %s", 
                   filename, buf );
   }
   VG_(close)(fd);
   VG_(message)(Vg_UserMsg, "exiting now.");
    VG_(exit)(1);

#  undef N_BUF   
}


void VG_(load_suppressions) ( void )
{
   Int i;
   vg_suppressions = NULL;
   for (i = 0; i < VG_(clo_n_suppressions); i++) {
      if (VG_(clo_verbosity) > 1) {
         VG_(message)(Vg_UserMsg, "Reading suppressions file: %s", 
                                  VG_(clo_suppressions)[i] );
      }
      load_one_suppressions_file( VG_(clo_suppressions)[i] );
   }
}


/* Does an error context match a suppression?  ie is this a
   suppressible error?  If so, return a pointer to the Suppression
   record, otherwise NULL.
   Tries to minimise the number of calls to what_fn_is_this since they
   are expensive.  
*/
static Suppression* is_suppressible_error ( ErrContext* ec )
{
#  define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                        && VG_(strcmp)((s1),(s2))==0)

   Char caller0_obj[M_VG_ERRTXT];
   Char caller0_fun[M_VG_ERRTXT];
   Char caller1_obj[M_VG_ERRTXT];
   Char caller1_fun[M_VG_ERRTXT];
   Char caller2_obj[M_VG_ERRTXT];
   Char caller2_fun[M_VG_ERRTXT];
   Char caller3_obj[M_VG_ERRTXT];
   Char caller3_fun[M_VG_ERRTXT];

   Suppression* su;
   Int          su_size;

   /* vg_what_fn_or_object_is_this returns:
         <function_name>      or
         <object_name>        or
         ???
      so the strings in the suppression file should match these.
   */

   /* Initialise these strs so they are always safe to compare, even
      if what_fn_or_object_is_this doesn't write anything to them. */
   caller0_obj[0] = caller1_obj[0] = caller2_obj[0] = caller3_obj[0] = 0;
   caller0_fun[0] = caller1_fun[0] = caller2_obj[0] = caller3_obj[0] = 0;

   VG_(what_obj_and_fun_is_this)
      ( ec->where->eips[0], caller0_obj, M_VG_ERRTXT,
                            caller0_fun, M_VG_ERRTXT );
   VG_(what_obj_and_fun_is_this)
      ( ec->where->eips[1], caller1_obj, M_VG_ERRTXT,
                            caller1_fun, M_VG_ERRTXT );

   if (VG_(clo_backtrace_size) > 2) {
      VG_(what_obj_and_fun_is_this)
         ( ec->where->eips[2], caller2_obj, M_VG_ERRTXT,
                               caller2_fun, M_VG_ERRTXT );

      if (VG_(clo_backtrace_size) > 3) {
         VG_(what_obj_and_fun_is_this)
            ( ec->where->eips[3], caller3_obj, M_VG_ERRTXT,
                                  caller3_fun, M_VG_ERRTXT );
      }
   }

   /* See if the error context matches any suppression. */
   for (su = vg_suppressions; su != NULL; su = su->next) {
      switch (su->skind) {
         case FreeS:  case PThread:
         case Param:  case Value0: su_size = 0; break;
         case Value1: case Addr1:  su_size = 1; break;
         case Value2: case Addr2:  su_size = 2; break;
         case Value4: case Addr4:  su_size = 4; break;
         case Value8: case Addr8:  su_size = 8; break;
         default: VG_(panic)("errcontext_matches_suppression");
      }
      switch (su->skind) {
         case Param:
            if (ec->ekind != ParamErr) continue;
            if (!STREQ(su->param, ec->syscall_param)) continue;
            break;
         case Value0: case Value1: case Value2: case Value4: case Value8:
            if (ec->ekind != ValueErr) continue;
            if (ec->size  != su_size)  continue;
            break;
         case Addr1: case Addr2: case Addr4: case Addr8:
            if (ec->ekind != AddrErr) continue;
            if (ec->size  != su_size) continue;
            break;
         case FreeS:
            if (ec->ekind != FreeErr 
                && ec->ekind != FreeMismatchErr) continue;
            break;
         case PThread:
            if (ec->ekind != PThreadErr) continue;
            break;
      }

      switch (su->caller0_ty) {
         case ObjName: if (!VG_(stringMatch)(su->caller0, 
                                             caller0_obj)) continue;
                       break;
         case FunName: if (!VG_(stringMatch)(su->caller0, 
                                             caller0_fun)) continue;
                       break;
         default: goto baaaad;
      }

      if (su->caller1 != NULL) {
         vg_assert(VG_(clo_backtrace_size) >= 2);
         switch (su->caller1_ty) {
            case ObjName: if (!VG_(stringMatch)(su->caller1, 
                                                caller1_obj)) continue;
                          break;
            case FunName: if (!VG_(stringMatch)(su->caller1, 
                                                caller1_fun)) continue;
                          break;
            default: goto baaaad;
         }
      }

      if (VG_(clo_backtrace_size) > 2 && su->caller2 != NULL) {
         switch (su->caller2_ty) {
            case ObjName: if (!VG_(stringMatch)(su->caller2, 
                                                caller2_obj)) continue;
                          break;
            case FunName: if (!VG_(stringMatch)(su->caller2, 
                                                caller2_fun)) continue;
                          break;
            default: goto baaaad;
         }
      }

      if (VG_(clo_backtrace_size) > 3 && su->caller3 != NULL) {
         switch (su->caller3_ty) {
            case ObjName: if (!VG_(stringMatch)(su->caller3,
                                                caller3_obj)) continue;
                          break;
            case FunName: if (!VG_(stringMatch)(su->caller3, 
                                                caller3_fun)) continue;
                          break;
            default: goto baaaad;
         }
      }

      return su;
   }

   return NULL;

  baaaad:
   VG_(panic)("is_suppressible_error");

#  undef STREQ
}

/*--------------------------------------------------------------------*/
/*--- end                                          vg_errcontext.c ---*/
/*--------------------------------------------------------------------*/
