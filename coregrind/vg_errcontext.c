
/*--------------------------------------------------------------------*/
/*--- Management of error messages.                vg_errcontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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

   The GNU General Public License is contained in the file COPYING.
*/

#include "vg_include.h"

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* The list of error contexts found, both suppressed and unsuppressed.
   Initially empty, and grows as errors are detected. */
static Error* vg_errors = NULL;

/* The list of suppression directives, as read from the specified
   suppressions file. */
static Supp* vg_suppressions = NULL;

/* And a helper function so the leak detector can get hold of it. */
Supp* VG_(get_suppressions) ( void ) { return vg_suppressions; }

/* Running count of unsuppressed errors detected. */
static UInt vg_n_errs_found = 0;

/* Running count of suppressed errors detected. */
static UInt vg_n_errs_suppressed = 0;

/* forwards ... */
static Supp* is_suppressible_error ( Error* err );


/*------------------------------------------------------------*/
/*--- Helper fns                                           ---*/
/*------------------------------------------------------------*/

/* Compare error contexts, to detect duplicates.  Note that if they
   are otherwise the same, the faulting addrs and associated rwoffsets
   are allowed to be different.  */
static Bool eq_Error ( VgRes res, Error* e1, Error* e2 )
{
   if (e1->ekind != e2->ekind) 
      return False;
   if (!VG_(eq_ExeContext)(res, e1->where, e2->where))
      return False;

   switch (e1->ekind) {
      case PThreadErr:
         vg_assert(VG_(needs).core_errors);
         if (e1->string == e2->string) 
            return True;
         if (0 == VG_(strcmp)(e1->string, e2->string))
            return True;
         return False;
      default: 
         if (VG_(needs).skin_errors)
            return SK_(eq_SkinError)(res, e1, e2);
         else {
            VG_(printf)("\nUnhandled error type: %u. VG_(needs).skin_errors\n"
                        "probably needs to be set.\n",
                        e1->ekind);
            VG_(skin_panic)("unhandled error type");
         }
   }
}

static void pp_Error ( Error* err, Bool printCount )
{
   /* Closure for printing where the error occurred.  Abstracts details
      about the `where' field away from the skin. */
   void pp_ExeContextClosure(void)
   {
      VG_(pp_ExeContext) ( err->where );
   }
   
   if (printCount)
      VG_(message)(Vg_UserMsg, "Observed %d times:", err->count );
   if (err->tid > 1)
      VG_(message)(Vg_UserMsg, "Thread %d:", err->tid );

   switch (err->ekind) {
      case PThreadErr:
         vg_assert(VG_(needs).core_errors);
         VG_(message)(Vg_UserMsg, "%s", err->string );
         VG_(pp_ExeContext)(err->where);
         break;
      default: 
         if (VG_(needs).skin_errors)
            SK_(pp_SkinError)( err, &pp_ExeContextClosure );
         else {
            VG_(printf)("\nUnhandled error type: %u.  VG_(needs).skin_errors\n"
                        "probably needs to be set?\n",
                        err->ekind);
            VG_(skin_panic)("unhandled error type");
         }
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


/* I've gone all object-oriented... initialisation depends on where the
   error comes from:

   - If from generated code (tst == NULL), the %EIP/%EBP values that we
     need in order to create proper error messages are picked up out of
     VG_(baseBlock) rather than from the thread table (vg_threads in
     vg_scheduler.c).

   - If not from generated code but in response to requests passed back to
     the scheduler (tst != NULL), we pick up %EIP/%EBP values from the
     stored thread state, not from VG_(baseBlock).  
*/
static __inline__
void construct_error ( Error* err, ThreadState* tst, 
                       ErrorKind ekind, Addr a, Char* s, void* extra )
{
   /* Core-only parts */
   err->next     = NULL;
   err->supp     = NULL;
   err->count    = 1;
   err->where    = VG_(get_ExeContext)( tst );

   if (NULL == tst) {
      err->tid   = VG_(get_current_tid)();
      err->m_eip = VG_(baseBlock)[VGOFF_(m_eip)];
      err->m_esp = VG_(baseBlock)[VGOFF_(m_esp)];
      err->m_ebp = VG_(baseBlock)[VGOFF_(m_ebp)];
   } else {
      err->tid   = tst->tid;
      err->m_eip = tst->m_eip;
      err->m_esp = tst->m_esp;
      err->m_ebp = tst->m_ebp;
   }

   /* Skin-relevant parts */
   err->ekind  = ekind;
   err->addr   = a;
   err->string = s;
   err->extra  = extra;

   /* sanity... */
   vg_assert(err->tid >= 0 && err->tid < VG_N_THREADS);
}

/* Top-level entry point to the error management subsystem.
   All detected errors are notified here; this routine decides if/when the
   user should see the error. */
void VG_(maybe_record_error) ( ThreadState* tst, 
                               ErrorKind ekind, Addr a, Char* s, void* extra )
{
          Error  err;
          Error* p;
          Error* p_prev;
          VgRes  exe_res                = Vg_MedRes;
   static Bool   is_first_shown_context = True;
   static Bool   stopping_message       = False;
   static Bool   slowdown_message       = False;
   static Int    vg_n_errs_shown        = 0;

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
            "that errors may occur in your program without prior warning from");
         VG_(message)(Vg_UserMsg, 
            "Valgrind, because errors are no longer being displayed.");
         VG_(message)(Vg_UserMsg, "");
         stopping_message = True;
      }
      return;
   }

   /* After M_VG_COLLECT_ERRORS_SLOWLY_AFTER different errors have
      been found, be much more conservative about collecting new
      ones. */
   if (vg_n_errs_shown >= M_VG_COLLECT_ERRORS_SLOWLY_AFTER) {
      exe_res = Vg_LowRes;
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

   /* Build ourselves the error */
   construct_error ( &err, tst, ekind, a, s, extra );

   /* First, see if we've got an error record matching this one. */
   p      = vg_errors;
   p_prev = NULL;
   while (p != NULL) {
      if (eq_Error(exe_res, p, &err)) {
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
            p->next         = vg_errors;
            vg_errors = p;
	 }
         return;
      }
      p_prev = p;
      p      = p->next;
   }

   /* Didn't see it.  Copy and add. */

   /* OK, we're really going to collect it.  First make a copy,
      because the error context is on the stack and will disappear shortly.
      We can duplicate the main part ourselves, but use
      SK_(dup_extra_and_update) to duplicate the `extra' part (unless it's
      NULL).
     
      SK_(dup_extra_and_update) can also update the `extra' part.  This is
      for when there are more details to fill in which take time to work out
      but don't affect our earlier decision to include the error -- by
      postponing those details until now, we avoid the extra work in the
      case where we ignore the error.  Ugly.
    */
   p = VG_(arena_malloc)(VG_AR_ERRORS, sizeof(Error));
   *p = err;
   if (NULL != err.extra)
      p->extra = SK_(dup_extra_and_update)(p);

   p->next = vg_errors;
   p->supp = is_suppressible_error(&err);
   vg_errors = p;
   if (p->supp == NULL) {
      vg_n_errs_found++;
      if (!is_first_shown_context)
         VG_(message)(Vg_UserMsg, "");
      pp_Error(p, False);      
      is_first_shown_context = False;
      vg_n_errs_shown++;
      /* Perhaps we want a GDB attach at this point? */
      if (vg_is_GDB_attach_requested()) {
         VG_(swizzle_esp_then_start_GDB)(
            err.m_eip, err.m_esp, err.m_ebp);
      }
   } else {
      vg_n_errs_suppressed++;
      p->supp->count++;
   }
}


/*------------------------------------------------------------*/
/*--- Exported fns                                         ---*/
/*------------------------------------------------------------*/

/* These are called not from generated code but from the scheduler */

void VG_(record_pthread_error) ( ThreadId tid, Char* msg )
{
   if (! VG_(needs).core_errors) return;
   VG_(maybe_record_error)( &VG_(threads)[tid], PThreadErr, /*addr*/0, msg, 
                            /*extra*/NULL );
}

/*------------------------------*/

void VG_(show_all_errors) ( void )
{
   Int    i, n_min;
   Int    n_err_contexts, n_supp_contexts;
   Error *p, *p_min;
   Supp  *su;
   Bool   any_supp;

   if (VG_(clo_verbosity) == 0)
      return;

   n_err_contexts = 0;
   for (p = vg_errors; p != NULL; p = p->next) {
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
      for (p = vg_errors; p != NULL; p = p->next) {
         if (p->supp != NULL) continue;
         if (p->count < n_min) {
            n_min = p->count;
            p_min = p;
         }
      }
      if (p_min == NULL) VG_(skin_panic)("show_all_errors()");

      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "%d errors in context %d of %d:",
                   p_min->count,
                   i+1, n_err_contexts);
      pp_Error( p_min, False );

      if ((i+1 == VG_(clo_dump_error))) {
	VG_(translate) ( 0 /* dummy ThreadId; irrelevant due to below NULLs */,
                         p_min->where->eips[0], NULL, NULL, NULL, NULL );
      }

      p_min->count = 1 << 30;
   } 

   if (n_supp_contexts > 0) 
      VG_(message)(Vg_DebugMsg, "");
   any_supp = False;
   for (su = vg_suppressions; su != NULL; su = su->next) {
      if (su->count > 0) {
         any_supp = True;
         VG_(message)(Vg_DebugMsg, "supp: %4d %s", su->count, su->sname);
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

Bool VG_(get_line) ( Int fd, Char* buf, Int nBuf )
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
static Bool setLocationTy ( Char** p_caller, SuppLocTy* p_ty )
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


/* Look for "skin" in a string like "skin1,skin2,skin3" */
static __inline__
Bool skin_name_present(Char *name, Char *names)
{
   Bool  found;
   Char *s = NULL;   /* Shut gcc up */
   Int   len = VG_(strlen)(name);

   found = (NULL != (s = VG_(strstr)(names, name)) &&
            (s        == names || *(s-1)   == ',') &&
            (*(s+len) == ','   || *(s+len) == '\0')
           );

   return found;
}

#define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                      && VG_(strcmp)((s1),(s2))==0)

/* Read suppressions from the file specified in vg_clo_suppressions
   and place them in the suppressions list.  If there's any difficulty
   doing this, just give up -- there's no point in trying to recover.  
*/
static void load_one_suppressions_file ( Char* filename )
{
#  define N_BUF 200
   Int   fd, i;
   Bool  eof;
   Char  buf[N_BUF+1];
   Char* skin_names;
   Char* supp_name;

   fd = VG_(open)( filename, VKI_O_RDONLY, 0 );
   if (fd == -1) {
      VG_(message)(Vg_UserMsg, "FATAL: can't open suppressions file `%s'", 
                   filename );
      VG_(exit)(1);
   }

   while (True) {
      /* Assign and initialise the two suppression halves (core and skin) */
      Supp* supp;
      supp        = VG_(arena_malloc)(VG_AR_CORE, sizeof(Supp));
      supp->count = 0;
      for (i = 0; i < VG_N_SUPP_CALLERS; i++) supp->caller[i] = NULL;
      supp->string = supp->extra = NULL;

      eof = VG_(get_line) ( fd, buf, N_BUF );
      if (eof) break;

      if (!STREQ(buf, "{")) goto syntax_error;
      
      eof = VG_(get_line) ( fd, buf, N_BUF );
      if (eof || STREQ(buf, "}")) goto syntax_error;
      supp->sname = VG_(arena_strdup)(VG_AR_CORE, buf);

      eof = VG_(get_line) ( fd, buf, N_BUF );

      if (eof) goto syntax_error;

      /* Check it has the "skin1,skin2,...:supp" form (look for ':') */
      i = 0;
      while (True) {
         if (buf[i] == ':')  break;
         if (buf[i] == '\0') goto syntax_error;
         i++;
      }
      buf[i]    = '\0';    /* Replace ':', splitting into two strings */

      skin_names = & buf[0];
      supp_name  = & buf[i+1];

      /* Is it a core suppression? */
      if (VG_(needs).core_errors && skin_name_present("core", skin_names))
      {
         if (STREQ(supp_name, "PThread"))
            supp->skind = PThreadSupp;
         else
            goto syntax_error;
      }

      /* Is it a skin suppression? */
      else if (VG_(needs).skin_errors && 
               skin_name_present(VG_(details).name, skin_names))
      {
         if (SK_(recognised_suppression)(supp_name, supp)) 
         {
            /* Do nothing, function fills in supp->skind */
         } else
            goto syntax_error;
      }

      else {
         /* Ignore rest of suppression */
         while (True) {
            eof = VG_(get_line) ( fd, buf, N_BUF );
            if (eof) goto syntax_error;
            if (STREQ(buf, "}"))
               break;
         }
         continue;
      }

      if (VG_(needs).skin_errors && 
          !SK_(read_extra_suppression_info)(fd, buf, N_BUF, supp)) 
         goto syntax_error;

      /* "i > 0" ensures at least one caller read. */
      for (i = 0; i < VG_N_SUPP_CALLERS; i++) {
         eof = VG_(get_line) ( fd, buf, N_BUF );
         if (eof) goto syntax_error;
         if (i > 0 && STREQ(buf, "}")) 
            break;
         supp->caller[i] = VG_(arena_strdup)(VG_AR_CORE, buf);
         if (!setLocationTy(&(supp->caller[i]), &(supp->caller_ty[i])))
            goto syntax_error;
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
                   "FATAL: in suppressions file: `%s': syntax error on: %s", 
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

/* Return the name of an erring fn in a way which is useful
   for comparing against the contents of a suppressions file. 
   Doesn't demangle the fn name, because we want to refer to 
   mangled names in the suppressions file.
*/
void VG_(get_objname_fnname) ( Addr a,
                               Char* obj_buf, Int n_obj_buf,
                               Char* fun_buf, Int n_fun_buf )
{     
   (void)VG_(get_objname)          ( a, obj_buf, n_obj_buf );
   (void)VG_(get_fnname_nodemangle)( a, fun_buf, n_fun_buf );
}     

static __inline__
Bool supp_matches_error(Supp* su, Error* err)
{
   switch (su->skind) {
      case PThreadSupp:
         return (err->ekind == PThreadErr);
      default:
         if (VG_(needs).skin_errors) {
            return SK_(error_matches_suppression)(err, su);
         } else {
            VG_(printf)(
               "\nUnhandled suppression type: %u.  VG_(needs).skin_errors\n"
               "probably needs to be set.\n",
               err->ekind);
            VG_(skin_panic)("unhandled suppression type");
         }
   }
}

static __inline__
Bool supp_matches_callers(Supp* su, Char caller_obj[][M_VG_ERRTXT], 
                                    Char caller_fun[][M_VG_ERRTXT])
{
   Int i;

   for (i = 0; su->caller[i] != NULL; i++) {
      switch (su->caller_ty[i]) {
         case ObjName: if (VG_(string_match)(su->caller[i],
                                             caller_obj[i])) break;
                       return False;
         case FunName: if (VG_(string_match)(su->caller[i], 
                                             caller_fun[i])) break;
                       return False;
         default: VG_(skin_panic)("is_suppressible_error");
      }
   }

   /* If we reach here, it's a match */
   return True;
}

/* Does an error context match a suppression?  ie is this a suppressible
   error?  If so, return a pointer to the Supp record, otherwise NULL.
   Tries to minimise the number of symbol searches since they are expensive.  
*/
static Supp* is_suppressible_error ( Error* err )
{
   Int i;

   Char caller_obj[VG_N_SUPP_CALLERS][M_VG_ERRTXT];
   Char caller_fun[VG_N_SUPP_CALLERS][M_VG_ERRTXT];

   Supp* su;

   /* get_objname_fnname() writes the function name and object name if
      it finds them in the debug info.  so the strings in the suppression
      file should match these.
   */

   /* Initialise these strs so they are always safe to compare, even
      if get_objname_fnname doesn't write anything to them. */
   for (i = 0; i < VG_N_SUPP_CALLERS; i++)
      caller_obj[i][0] = caller_fun[i][0] = 0;

   for (i = 0; i < VG_N_SUPP_CALLERS && i < VG_(clo_backtrace_size); i++) {
      VG_(get_objname_fnname) ( err->where->eips[i], 
                                caller_obj[i], M_VG_ERRTXT,
                                caller_fun[i], M_VG_ERRTXT );
   }

   /* See if the error context matches any suppression. */
   for (su = vg_suppressions; su != NULL; su = su->next) {
      if (supp_matches_error(su, err) &&
          supp_matches_callers(su, caller_obj, caller_fun)) {
         return su;
      }
   }
   return NULL;      /* no matches */
}

#undef STREQ

/*--------------------------------------------------------------------*/
/*--- end                                          vg_errcontext.c ---*/
/*--------------------------------------------------------------------*/
