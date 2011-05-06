
/*--------------------------------------------------------------------*/
/*--- Management of error messages.                   m_errormgr.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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
#include "pub_core_vki.h"
#include "pub_core_libcsetjmp.h"
#include "pub_core_threadstate.h"      // For VG_N_THREADS
#include "pub_core_debugger.h"
#include "pub_core_debuginfo.h"
#include "pub_core_errormgr.h"
#include "pub_core_execontext.h"
#include "pub_core_gdbserver.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"         // For VG_(getpid)()
#include "pub_core_seqmatch.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_stacktrace.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"        // for VG_(translate)()
#include "pub_core_xarray.h"           // VG_(xaprintf) et al

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* After this many different unsuppressed errors have been observed,
   be more conservative about collecting new ones. */
#define M_COLLECT_ERRORS_SLOWLY_AFTER 100

/* After this many different unsuppressed errors have been observed,
   stop collecting errors at all, and tell the user their program is
   evidently a steaming pile of camel dung. */
#define M_COLLECT_NO_ERRORS_AFTER_SHOWN 1000

/* After this many total errors have been observed, stop collecting
   errors at all.  Counterpart to M_COLLECT_NO_ERRORS_AFTER_SHOWN. */
#define M_COLLECT_NO_ERRORS_AFTER_FOUND 10000000

/* The list of error contexts found, both suppressed and unsuppressed.
   Initially empty, and grows as errors are detected. */
static Error* errors = NULL;

/* The list of suppression directives, as read from the specified
   suppressions file.  Note that the list gets rearranged as a result
   of the searches done by is_suppressible_error(). */
static Supp* suppressions = NULL;

/* Running count of unsuppressed errors detected. */
static UInt n_errs_found = 0;

/* Running count of suppressed errors detected. */
static UInt n_errs_suppressed = 0;

/* Running count of unsuppressed error contexts. */
static UInt n_err_contexts = 0;

/* Running count of suppressed error contexts. */
static UInt n_supp_contexts = 0;


/* forwards ... */
static Supp* is_suppressible_error ( Error* err );

static ThreadId last_tid_printed = 1;

/* Stats: number of searches of the error list initiated. */
static UWord em_errlist_searches = 0;

/* Stats: number of comparisons done during error list
   searching. */
static UWord em_errlist_cmps = 0;

/* Stats: number of searches of the suppression list initiated. */
static UWord em_supplist_searches = 0;

/* Stats: number of comparisons done during suppression list
   searching. */
static UWord em_supplist_cmps = 0;

/*------------------------------------------------------------*/
/*--- Error type                                           ---*/
/*------------------------------------------------------------*/

/* Errors.  Extensible (via the 'extra' field).  Tools can use a normal
   enum (with element values in the normal range (0..)) for 'ekind'. 
   Functions for getting/setting the tool-relevant fields are in
   include/pub_tool_errormgr.h.

   When errors are found and recorded with VG_(maybe_record_error)(), all
   the tool must do is pass in the four parameters;  core will
   allocate/initialise the error record.
*/
struct _Error {
   struct _Error* next;
   // Unique tag.  This gives the error a unique identity (handle) by
   // which it can be referred to afterwords.  Currently only used for
   // XML printing.
   UInt unique;
   // NULL if unsuppressed; or ptr to suppression record.
   Supp* supp;
   Int count;

   // The tool-specific part
   ThreadId tid;           // Initialised by core
   ExeContext* where;      // Initialised by core
   ErrorKind ekind;        // Used by ALL.  Must be in the range (0..)
   Addr addr;              // Used frequently
   Char* string;           // Used frequently
   void* extra;            // For any tool-specific extras
};


ExeContext* VG_(get_error_where) ( Error* err )
{
   return err->where;
}

ErrorKind VG_(get_error_kind) ( Error* err )
{
   return err->ekind;
}

Addr VG_(get_error_address) ( Error* err )
{
   return err->addr;
}

Char* VG_(get_error_string) ( Error* err )
{
   return err->string;
}

void* VG_(get_error_extra)  ( Error* err )
{
   return err->extra;
}

UInt VG_(get_n_errs_found)( void )
{
   return n_errs_found;
}

/*------------------------------------------------------------*/
/*--- Suppression type                                     ---*/
/*------------------------------------------------------------*/

/* Note: it is imperative this doesn't overlap with (0..) at all, as tools
 * effectively extend it by defining their own enums in the (0..) range. */
typedef
   enum {
      // Nb: thread errors are a relic of the time when Valgrind's core
      // could detect them.  This example is left commented-out as an
      // example should new core errors ever be added.
      ThreadSupp = -1,    /* Matches ThreadErr */
   }
   CoreSuppKind;

/* Max number of callers for context in a suppression. */
#define VG_MAX_SUPP_CALLERS  24

/* For each caller specified for a suppression, record the nature of
   the caller name.  Not of interest to tools. */
typedef
   enum { 
      NoName,     /* Error case */
      ObjName,    /* Name is of an shared object file. */
      FunName,    /* Name is of a function. */
      DotDotDot   /* Frame-level wildcard */
   }
   SuppLocTy;

typedef
   struct {
      SuppLocTy ty;
      Char*     name; /* NULL for NoName and DotDotDot */
   }
   SuppLoc;

/* Suppressions.  Tools can get/set tool-relevant parts with functions
   declared in include/pub_tool_errormgr.h.  Extensible via the 'extra' field. 
   Tools can use a normal enum (with element values in the normal range
   (0..)) for 'skind'. */
struct _Supp {
   struct _Supp* next;
   Int count;     // The number of times this error has been suppressed.
   Char* sname;   // The name by which the suppression is referred to.

   // Length of 'callers'
   Int n_callers;
   // Array of callers, for matching stack traces.  First one (name of fn
   // where err occurs) is mandatory;  rest are optional.
   SuppLoc* callers;

   /* The tool-specific part */
   SuppKind skind;   // What kind of suppression.  Must use the range (0..).
   Char* string;     // String -- use is optional.  NULL by default.
   void* extra;      // Anything else -- use is optional.  NULL by default.
};

SuppKind VG_(get_supp_kind) ( Supp* su )
{
   return su->skind;
}

Char* VG_(get_supp_string) ( Supp* su )
{
   return su->string;
}

void* VG_(get_supp_extra)  ( Supp* su )
{
   return su->extra;
}


void VG_(set_supp_kind)   ( Supp* su, SuppKind skind )
{
   su->skind = skind;
}

void VG_(set_supp_string) ( Supp* su, Char* string )
{
   su->string = string;
}

void VG_(set_supp_extra)  ( Supp* su, void* extra )
{
   su->extra = extra;
}


/*------------------------------------------------------------*/
/*--- Helper fns                                           ---*/
/*------------------------------------------------------------*/

// Only show core errors if the tool wants to, we're not running with -q,
// and were not outputting XML.
Bool VG_(showing_core_errors)(void)
{
   return VG_(needs).core_errors && VG_(clo_verbosity) >= 1 && !VG_(clo_xml);
}

/* Compare errors, to detect duplicates. 
*/
static Bool eq_Error ( VgRes res, Error* e1, Error* e2 )
{
   if (e1->ekind != e2->ekind) 
      return False;
   if (!VG_(eq_ExeContext)(res, e1->where, e2->where))
      return False;

   switch (e1->ekind) {
      //(example code, see comment on CoreSuppKind above)
      //case ThreadErr:
      //   vg_assert(VG_(needs).core_errors);
      //   return <something>
      default: 
         if (VG_(needs).tool_errors) {
            return VG_TDICT_CALL(tool_eq_Error, res, e1, e2);
         } else {
            VG_(printf)("\nUnhandled error type: %u. VG_(needs).tool_errors\n"
                        "probably needs to be set.\n",
                        e1->ekind);
            VG_(tool_panic)("unhandled error type");
         }
   }
}


/* Helper functions for suppression generation: print a single line of
   a suppression pseudo-stack-trace, either in XML or text mode.  It's
   important that the behaviour of these two functions exactly
   corresponds.
*/
#define ERRTXT_LEN   4096

static void printSuppForIp_XML(UInt n, Addr ip, void* uu_opaque)
{
   static UChar buf[ERRTXT_LEN];
   if ( VG_(get_fnname_no_cxx_demangle) (ip, buf,  ERRTXT_LEN) ) {
      VG_(printf_xml_no_f_c)("    <sframe> <fun>%t</fun> </sframe>\n", buf);
   } else
   if ( VG_(get_objname)(ip, buf, ERRTXT_LEN) ) {
      VG_(printf_xml_no_f_c)("    <sframe> <obj>%t</obj> </sframe>\n", buf);
   } else {
      VG_(printf_xml_no_f_c)("    <sframe> <obj>*</obj> </sframe>\n");
   }
}

static void printSuppForIp_nonXML(UInt n, Addr ip, void* textV)
{
   static UChar buf[ERRTXT_LEN];
   XArray* /* of HChar */ text = (XArray*)textV;
   if ( VG_(get_fnname_no_cxx_demangle) (ip, buf,  ERRTXT_LEN) ) {
      VG_(xaprintf)(text, "   fun:%s\n", buf);
   } else
   if ( VG_(get_objname)(ip, buf, ERRTXT_LEN) ) {
      VG_(xaprintf)(text, "   obj:%s\n", buf);
   } else {
      VG_(xaprintf)(text, "   obj:*\n");
   }
}

/* Generate a suppression for an error, either in text or XML mode.
*/
static void gen_suppression(Error* err)
{
   Char        xtra[256]; /* assumed big enough (is overrun-safe) */
   Bool        anyXtra;
   Char*       name;
   ExeContext* ec;
   XArray* /* HChar */ text;

   const HChar* dummy_name = "insert_a_suppression_name_here";

   vg_assert(err);

   /* In XML mode, we also need to print the plain text version of the
      suppresion in a CDATA section.  What that really means is, we
      need to generate the plaintext version both in XML and text
      mode.  So generate it into TEXT. */
   text = VG_(newXA)( VG_(malloc), "errormgr.gen_suppression.1",
                      VG_(free), sizeof(HChar) );
   vg_assert(text);

   ec = VG_(get_error_where)(err);
   vg_assert(ec);

   name = VG_TDICT_CALL(tool_get_error_name, err);
   if (NULL == name) {
      VG_(umsg)("(%s does not allow error to be suppressed)\n",
                VG_(details).name);
      return;
   }

   /* Ok.  Generate the plain text version into TEXT. */
   VG_(xaprintf)(text, "{\n");
   VG_(xaprintf)(text, "   <%s>\n", dummy_name);
   VG_(xaprintf)(text, "   %s:%s\n", VG_(details).name, name);

   VG_(memset)(xtra, 0, sizeof(xtra));
   anyXtra = VG_TDICT_CALL(tool_get_extra_suppression_info,
                           err, xtra, sizeof(xtra));
   vg_assert(xtra[sizeof(xtra)-1] == 0);

   if (anyXtra)
      VG_(xaprintf)(text, "   %s\n", xtra);

   // Print stack trace elements
   UInt n_ips = VG_(get_ExeContext_n_ips)(ec);
   tl_assert(n_ips > 0);
   if (n_ips > VG_MAX_SUPP_CALLERS)
      n_ips = VG_MAX_SUPP_CALLERS;
   VG_(apply_StackTrace)(printSuppForIp_nonXML,
                         text,
                         VG_(get_ExeContext_StackTrace)(ec),
                         n_ips);

   VG_(xaprintf)(text, "}\n");
   // zero terminate
   VG_(xaprintf)(text, "%c", (HChar)0 );
   // VG_(printf) of text

   /* And now display it. */
   if (! VG_(clo_xml) ) {

      // the simple case
      VG_(printf)("%s", (HChar*) VG_(indexXA)(text, 0) );

   } else {

      /* Now we have to print the XML directly.  No need to go to the
         effort of stuffing it in an XArray, since we won't need it
         again. */
      VG_(printf_xml)("  <suppression>\n");
      VG_(printf_xml)("    <sname>%s</sname>\n", dummy_name);
      VG_(printf_xml_no_f_c)(
                      "    <skind>%t:%t</skind>\n", VG_(details).name, name);
      if (anyXtra)
         VG_(printf_xml_no_f_c)("    <skaux>%t</skaux>\n", xtra);

      // Print stack trace elements
      VG_(apply_StackTrace)(printSuppForIp_XML,
                            NULL,
                            VG_(get_ExeContext_StackTrace)(ec),
                            VG_(get_ExeContext_n_ips)(ec));

      // And now the cdata bit
      // XXX FIXME!  properly handle the case where the raw text
      // itself contains "]]>", as specified in Protocol 4.
      VG_(printf_xml)("    <rawtext>\n");
      VG_(printf_xml)("<![CDATA[\n");
      VG_(printf_xml)("%s", (HChar*) VG_(indexXA)(text, 0) );
      VG_(printf_xml)("]]>\n");
      VG_(printf_xml)("    </rawtext>\n");
      VG_(printf_xml)("  </suppression>\n");

   }

   VG_(deleteXA)(text);
}


/* Figure out if we want to perform a given action for this error,
   possibly by asking the user.
*/
Bool VG_(is_action_requested) ( Char* action, Bool* clo )
{
   Char ch, ch2;
   Int res;

   /* First off, we shouldn't be asking the user anything if
      we're in XML mode. */
   if (VG_(clo_xml))
      return False; /* That's a Nein, oder Nay as they say down here in B-W */

   if (*clo == False)
      return False;

   VG_(umsg)("\n");

  again:
   VG_(printf)(
      "==%d== "
      "---- %s ? --- [Return/N/n/Y/y/C/c] ---- ", 
      VG_(getpid)(), action
   );

   res = VG_(read)(VG_(clo_input_fd), &ch, 1);
   if (res != 1) goto ioerror;
   /* res == 1 */
   if (ch == '\n') return False;
   if (ch != 'N' && ch != 'n' && ch != 'Y' && ch != 'y' 
      && ch != 'C' && ch != 'c') goto again;

   res = VG_(read)(VG_(clo_input_fd), &ch2, 1);
   if (res != 1) goto ioerror;
   if (ch2 != '\n') goto again;

   /* No, don't want to do action. */
   if (ch == 'n' || ch == 'N') return False;
   /* Yes, want to do action. */
   if (ch == 'y' || ch == 'Y') return True;
   /* No, don't want to do action, and don't ask again either. */
   vg_assert(ch == 'c' || ch == 'C');

  ioerror:
   *clo = False;
   return False;
}


/* Do text-mode actions on error, that is, immediately after an error
   is printed.  These are:
   * possibly, attach to a debugger
   * possibly, generate a suppression.
   Note this should not be called in XML mode! 
*/
static 
void do_actions_on_error(Error* err, Bool allow_db_attach)
{
   Bool still_noisy = True;

   /* Should be assured by caller */
   vg_assert( ! VG_(clo_xml) );

   /* if user wants to debug from a certain error nr, then wait for gdb/vgdb */
   if (VG_(clo_vgdb) != Vg_VgdbNo
       && allow_db_attach 
       && VG_(dyn_vgdb_error) <= n_errs_found) {
      VG_(umsg)("(action on error) vgdb me ... \n");
      VG_(gdbserver)( err->tid );
      VG_(umsg)("Continuing ...\n");
   }

   /* Perhaps we want a debugger attach at this point? */
   /* GDBTD ??? maybe we should/could remove the below assuming the
      gdbserver interface is better ??? */
   if (allow_db_attach &&
       VG_(is_action_requested)( "Attach to debugger", & VG_(clo_db_attach) ))
   {   
      if (0) VG_(printf)("starting debugger\n");
      VG_(start_debugger)( err->tid );
   }  
   /* Or maybe we want to generate the error's suppression? */
   if (VG_(clo_gen_suppressions) == 2
       || (VG_(clo_gen_suppressions) == 1
           && VG_(is_action_requested)( "Print suppression", &still_noisy ))
      ) {
      gen_suppression(err);
   }
   if (VG_(clo_gen_suppressions) == 1 && !still_noisy)
      VG_(clo_gen_suppressions) = 0;
}


/* Prints an error.  Not entirely simple because of the differences
   between XML and text mode output.
 
   In XML mode:

   * calls the tool's pre-show method, so the tool can create any
     preamble ahead of the message, if it wants.

   * prints the opening tag, and the <unique> and <tid> fields

   * prints the tool-specific parts of the message

   * if suppression generation is required, a suppression

   * the closing tag

   In text mode:

   * calls the tool's pre-show method, so the tool can create any
     preamble ahead of the message, if it wants.

   * prints the tool-specific parts of the message

   * calls do_actions_on_error.  This optionally does a debugger
     attach (and detach), and optionally prints a suppression; both
     of these may require user input.
*/
static void pp_Error ( Error* err, Bool allow_db_attach, Bool xml )
{
   /* If this fails, you probably specified your tool's method
      dictionary incorrectly. */
   vg_assert(VG_(needs).tool_errors);

   if (xml) {

      /* Note, allow_db_attach is ignored in here. */
 
      /* Ensure that suppression generation is either completely
         enabled or completely disabled; either way, we won't require
         any user input.  m_main.process_cmd_line_options should
         ensure the asserted condition holds. */
      vg_assert( VG_(clo_gen_suppressions) == 0 /* disabled */
                 || VG_(clo_gen_suppressions) == 2 /* for all errors */ );

      /* Pre-show it to the tool */
      VG_TDICT_CALL( tool_before_pp_Error, err );
   
      /* standard preamble */
      VG_(printf_xml)("<error>\n");
      VG_(printf_xml)("  <unique>0x%x</unique>\n", err->unique);
      VG_(printf_xml)("  <tid>%d</tid>\n", err->tid);

      /* actually print it */
      VG_TDICT_CALL( tool_pp_Error, err );

      if (VG_(clo_gen_suppressions) > 0)
        gen_suppression(err);

      /* postamble */
      VG_(printf_xml)("</error>\n");
      VG_(printf_xml)("\n");

   } else {

      VG_TDICT_CALL( tool_before_pp_Error, err );

      if (VG_(tdict).tool_show_ThreadIDs_for_errors
          && err->tid > 0 && err->tid != last_tid_printed) {
         VG_(umsg)("Thread %d:\n", err->tid );
         last_tid_printed = err->tid;
      }
   
      VG_TDICT_CALL( tool_pp_Error, err );
      VG_(umsg)("\n");

      do_actions_on_error(err, allow_db_attach);
   }
}


/* Construct an error */
static
void construct_error ( Error* err, ThreadId tid, ErrorKind ekind, Addr a,
                       Char* s, void* extra, ExeContext* where )
{
   /* DO NOT MAKE unique_counter NON-STATIC */
   static UInt unique_counter = 0;

   tl_assert(tid < VG_N_THREADS);

   /* Core-only parts */
   err->unique   = unique_counter++;
   err->next     = NULL;
   err->supp     = NULL;
   err->count    = 1;
   err->tid      = tid;
   if (NULL == where)
     err->where = VG_(record_ExeContext)( tid, 0 );
   else
      err->where = where;

   /* Tool-relevant parts */
   err->ekind  = ekind;
   err->addr   = a;
   err->extra  = extra;
   err->string = s;

   /* sanity... */
   vg_assert( tid < VG_N_THREADS );
}



static Int  n_errs_shown = 0;

/* Top-level entry point to the error management subsystem.
   All detected errors are notified here; this routine decides if/when the
   user should see the error. */
void VG_(maybe_record_error) ( ThreadId tid, 
                               ErrorKind ekind, Addr a, Char* s, void* extra )
{
          Error  err;
          Error* p;
          Error* p_prev;
          UInt   extra_size;
          VgRes  exe_res          = Vg_MedRes;
   static Bool   stopping_message = False;
   static Bool   slowdown_message = False;

   /* After M_COLLECT_NO_ERRORS_AFTER_SHOWN different errors have
      been found, or M_COLLECT_NO_ERRORS_AFTER_FOUND total errors
      have been found, just refuse to collect any more.  This stops
      the burden of the error-management system becoming excessive in
      extremely buggy programs, although it does make it pretty
      pointless to continue the Valgrind run after this point. */
   if (VG_(clo_error_limit) 
       && (n_errs_shown >= M_COLLECT_NO_ERRORS_AFTER_SHOWN
           || n_errs_found >= M_COLLECT_NO_ERRORS_AFTER_FOUND)
       && !VG_(clo_xml)) {
      if (!stopping_message) {
         VG_(umsg)("\n");

	 if (n_errs_shown >= M_COLLECT_NO_ERRORS_AFTER_SHOWN) {
            VG_(umsg)(
               "More than %d different errors detected.  "
               "I'm not reporting any more.\n",
               M_COLLECT_NO_ERRORS_AFTER_SHOWN );
         } else {
            VG_(umsg)(
               "More than %d total errors detected.  "
               "I'm not reporting any more.\n",
               M_COLLECT_NO_ERRORS_AFTER_FOUND );
	 }

         VG_(umsg)("Final error counts will be inaccurate.  "
                   "Go fix your program!\n");
         VG_(umsg)("Rerun with --error-limit=no to disable "
                   "this cutoff.  Note\n");
         VG_(umsg)("that errors may occur in your program without "
                   "prior warning from\n");
         VG_(umsg)("Valgrind, because errors are no longer "
                   "being displayed.\n");
         VG_(umsg)("\n");
         stopping_message = True;
      }
      return;
   }

   /* After M_COLLECT_ERRORS_SLOWLY_AFTER different errors have
      been found, be much more conservative about collecting new
      ones. */
   if (n_errs_shown >= M_COLLECT_ERRORS_SLOWLY_AFTER
       && !VG_(clo_xml)) {
      exe_res = Vg_LowRes;
      if (!slowdown_message) {
         VG_(umsg)("\n");
         VG_(umsg)("More than %d errors detected.  Subsequent errors\n",
                   M_COLLECT_ERRORS_SLOWLY_AFTER);
         VG_(umsg)("will still be recorded, but in less "
                   "detail than before.\n");
         slowdown_message = True;
      }
   }

   /* Build ourselves the error */
   construct_error ( &err, tid, ekind, a, s, extra, NULL );

   /* First, see if we've got an error record matching this one. */
   em_errlist_searches++;
   p       = errors;
   p_prev  = NULL;
   while (p != NULL) {
      em_errlist_cmps++;
      if (eq_Error(exe_res, p, &err)) {
         /* Found it. */
         p->count++;
	 if (p->supp != NULL) {
            /* Deal correctly with suppressed errors. */
            p->supp->count++;
            n_errs_suppressed++;	 
         } else {
            n_errs_found++;
         }

         /* Move p to the front of the list so that future searches
            for it are faster. It also allows to print the last
            error (see VG_(show_last_error). */
         if (p_prev != NULL) {
            vg_assert(p_prev->next == p);
            p_prev->next = p->next;
            p->next      = errors;
            errors       = p;
	 }

         return;
      }
      p_prev = p;
      p      = p->next;
   }

   /* Didn't see it.  Copy and add. */

   /* OK, we're really going to collect it.  The context is on the stack and
      will disappear shortly, so we must copy it.  First do the main
      (non-'extra') part.
     
      Then VG_(tdict).tool_update_extra can update the 'extra' part.  This
      is for when there are more details to fill in which take time to work
      out but don't affect our earlier decision to include the error -- by
      postponing those details until now, we avoid the extra work in the
      case where we ignore the error.  Ugly.

      Then, if there is an 'extra' part, copy it too, using the size that
      VG_(tdict).tool_update_extra returned.  Also allow for people using
      the void* extra field for a scalar value like an integer.
   */

   /* copy main part */
   p = VG_(arena_malloc)(VG_AR_ERRORS, "errormgr.mre.1", sizeof(Error));
   *p = err;

   /* update 'extra' */
   switch (ekind) {
      //(example code, see comment on CoreSuppKind above)
      //case ThreadErr:
      //   vg_assert(VG_(needs).core_errors);
      //   extra_size = <something>
      //   break;
      default:
         vg_assert(VG_(needs).tool_errors);
         extra_size = VG_TDICT_CALL(tool_update_extra, p);
         break;
   }

   /* copy block pointed to by 'extra', if there is one */
   if (NULL != p->extra && 0 != extra_size) { 
      void* new_extra = VG_(malloc)("errormgr.mre.2", extra_size);
      VG_(memcpy)(new_extra, p->extra, extra_size);
      p->extra = new_extra;
   }

   p->next = errors;
   p->supp = is_suppressible_error(&err);
   errors  = p;
   if (p->supp == NULL) {
      n_err_contexts++;
      n_errs_found++;
      /* Actually show the error; more complex than you might think. */
      pp_Error( p, /*allow_db_attach*/True, VG_(clo_xml) );
      /* update stats */
      n_errs_shown++;
   } else {
      n_supp_contexts++;
      n_errs_suppressed++;
      p->supp->count++;
   }
}

/* Second top-level entry point to the error management subsystem, for
   errors that the tool wants to report immediately, eg. because they're
   guaranteed to only happen once.  This avoids all the recording and
   comparing stuff.  But they can be suppressed;  returns True if it is
   suppressed.  Bool 'print_error' dictates whether to print the error. 
   Bool 'count_error' dictates whether to count the error in n_errs_found.
*/
Bool VG_(unique_error) ( ThreadId tid, ErrorKind ekind, Addr a, Char* s,
                         void* extra, ExeContext* where, Bool print_error,
                         Bool allow_db_attach, Bool count_error )
{
   Error err;
   Supp *su;

   /* Build ourselves the error */
   construct_error ( &err, tid, ekind, a, s, extra, where );

   /* Unless it's suppressed, we're going to show it.  Don't need to make
      a copy, because it's only temporary anyway.

      Then update the 'extra' part with VG_(tdict).tool_update_extra),
      because that can have an affect on whether it's suppressed.  Ignore
      the size return value of VG_(tdict).tool_update_extra, because we're
      not copying 'extra'. */
   (void)VG_TDICT_CALL(tool_update_extra, &err);

   su = is_suppressible_error(&err);
   if (NULL == su) {
      if (count_error) {
         n_errs_found++;
         n_err_contexts++;
      }

      if (print_error) {
         /* Actually show the error; more complex than you might think. */
         pp_Error(&err, allow_db_attach, VG_(clo_xml));
         /* update stats */
         n_errs_shown++;
      }
      return False;

   } else {
      if (count_error) {
         n_errs_suppressed++;
         n_supp_contexts++;
      }
      su->count++;
      return True;
   }
}


/*------------------------------------------------------------*/
/*--- Exported fns                                         ---*/
/*------------------------------------------------------------*/

/* Show the used suppressions.  Returns False if no suppression
   got used. */
static Bool show_used_suppressions ( void )
{
   Supp  *su;
   Bool  any_supp;

   if (VG_(clo_xml))
      VG_(printf_xml)("<suppcounts>\n");

   any_supp = False;
   for (su = suppressions; su != NULL; su = su->next) {
      if (su->count <= 0)
         continue;
      if (VG_(clo_xml)) {
         VG_(printf_xml_no_f_c)( "  <pair>\n"
                                 "    <count>%d</count>\n"
                                 "    <name>%t</name>\n"
                                 "  </pair>\n",
                                 su->count, su->sname );
      } else {
         // blank line before the first shown suppression, if any
         if (!any_supp)
            VG_(dmsg)("\n");
         VG_(dmsg)("used_suppression: %6d %s\n", su->count, su->sname);
      }
      any_supp = True;
   }

   if (VG_(clo_xml))
      VG_(printf_xml)("</suppcounts>\n");

   return any_supp;
}

/* Show all the errors that occurred, and possibly also the
   suppressions used. */
void VG_(show_all_errors) (  Int verbosity, Bool xml )
{
   Int    i, n_min;
   Error *p, *p_min;
   Bool   any_supp;

   if (verbosity == 0)
      return;

   /* If we're printing XML, just show the suppressions and stop. */
   if (xml) {
      (void)show_used_suppressions();
      return;
   }

   /* We only get here if not printing XML. */
   VG_(umsg)("ERROR SUMMARY: "
             "%d errors from %d contexts (suppressed: %d from %d)\n",
             n_errs_found, n_err_contexts, 
             n_errs_suppressed, n_supp_contexts );

   if (verbosity <= 1)
      return;

   // We do the following only at -v or above, and only in non-XML
   // mode

   /* Print the contexts in order of increasing error count. 
      Once an error is shown, we add a huge value to its count to filter it
      out. After having shown all errors, we reset count to the original value. */
   for (i = 0; i < n_err_contexts; i++) {
      n_min = (1 << 30) - 1;
      p_min = NULL;
      for (p = errors; p != NULL; p = p->next) {
         if (p->supp != NULL) continue;
         if (p->count < n_min) {
            n_min = p->count;
            p_min = p;
         }
      }
      // XXX: this isn't right.  See bug 203651.
      if (p_min == NULL) continue; //VG_(tool_panic)("show_all_errors()");

      VG_(umsg)("\n");
      VG_(umsg)("%d errors in context %d of %d:\n",
                p_min->count, i+1, n_err_contexts);
      pp_Error( p_min, False/*allow_db_attach*/, False /* xml */ );

      // We're not printing XML -- we'd have exited above if so.
      vg_assert(! xml);

      if ((i+1 == VG_(clo_dump_error))) {
         StackTrace ips = VG_(get_ExeContext_StackTrace)(p_min->where);
         VG_(translate) ( 0 /* dummy ThreadId; irrelevant due to debugging*/,
                          ips[0], /*debugging*/True, 0xFE/*verbosity*/,
                          /*bbs_done*/0,
                          /*allow redir?*/True);
      }

      p_min->count = p_min->count + (1 << 30);
   } 

   /* reset the counts, otherwise a 2nd call does not show anything anymore */ 
   for (p = errors; p != NULL; p = p->next) {
      if (p->count >= (1 << 30))
         p->count = p->count - (1 << 30);
   }


   any_supp = show_used_suppressions();

   if (any_supp) 
      VG_(umsg)("\n");
   // reprint this, so users don't have to scroll way up to find
   // the first printing
   VG_(umsg)("ERROR SUMMARY: "
             "%d errors from %d contexts (suppressed: %d from %d)\n",
             n_errs_found, n_err_contexts, n_errs_suppressed,
             n_supp_contexts );
}

void VG_(show_last_error) ( void )
{
   if (n_err_contexts == 0) {
      VG_(umsg)("No errors yet\n");
      return;
   }

   pp_Error( errors, False/*allow_db_attach*/, False/*xml*/ );
}


/* Show occurrence counts of all errors, in XML form. */
void VG_(show_error_counts_as_XML) ( void )
{
   Error* err;
   VG_(printf_xml)("<errorcounts>\n");
   for (err = errors; err != NULL; err = err->next) {
      if (err->supp != NULL)
         continue;
      if (err->count <= 0)
         continue;
      VG_(printf_xml)("  <pair>\n");
      VG_(printf_xml)("    <count>%d</count>\n", err->count);
      VG_(printf_xml)("    <unique>0x%x</unique>\n", err->unique);
      VG_(printf_xml)("  </pair>\n");
   }
   VG_(printf_xml)("</errorcounts>\n");
   VG_(printf_xml)("\n");
}


/*------------------------------------------------------------*/
/*--- Suppression parsing                                  ---*/
/*------------------------------------------------------------*/

/* Get the next char from fd into *out_buf.  Returns 1 if success,
   0 if eof or < 0 if error. */

static Int get_char ( Int fd, Char* out_buf )
{
   Int r;
   static Char buf[256];
   static Int buf_size = 0;
   static Int buf_used = 0;
   vg_assert(buf_size >= 0 && buf_size <= 256);
   vg_assert(buf_used >= 0 && buf_used <= buf_size);
   if (buf_used == buf_size) {
      r = VG_(read)(fd, buf, 256);
      if (r < 0) return r; /* read failed */
      vg_assert(r >= 0 && r <= 256);
      buf_size = r;
      buf_used = 0;
   }
   if (buf_size == 0)
     return 0; /* eof */
   vg_assert(buf_size >= 0 && buf_size <= 256);
   vg_assert(buf_used >= 0 && buf_used < buf_size);
   *out_buf = buf[buf_used];
   buf_used++;
   return 1;
}

Bool VG_(get_line) ( Int fd, Char** bufpp, SizeT* nBufp, Int* lineno )
{
   Char* buf  = *bufpp;
   SizeT nBuf = *nBufp;
   Char  ch;
   Int   n, i;
   while (True) {
      /* First, read until a non-blank char appears. */
      while (True) {
         n = get_char(fd, &ch);
         if (n == 1 && !VG_(isspace)(ch)) break;
         if (n == 1 && ch == '\n' && lineno)
            (*lineno)++;
         if (n <= 0) return True;
      }

      /* Now, read the line into buf. */
      i = 0;
      buf[i++] = ch; buf[i] = 0;
      while (True) {
         n = get_char(fd, &ch);
         if (n <= 0) return False; /* the next call will return True */
         if (ch == '\n' && lineno)
            (*lineno)++;
         if (ch == '\n') break;
         if (i > 0 && i == nBuf-1) {
            *nBufp = nBuf = nBuf * 2;
            #define RIDICULOUS   100000
            vg_assert2(nBuf < RIDICULOUS,  // Just a sanity check, really.
               "VG_(get_line): line longer than %d chars, aborting\n",
               RIDICULOUS);
            *bufpp = buf = VG_(realloc)("errormgr.get_line.1", buf, nBuf);
         }
         buf[i++] = ch; buf[i] = 0;
      }
      while (i > 1 && VG_(isspace)(buf[i-1])) { 
         i--; buf[i] = 0; 
      };

      /* VG_(printf)("The line is '%s'\n", buf); */
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
static Bool setLocationTy ( SuppLoc* p )
{
   if (VG_(strncmp)(p->name, "fun:", 4) == 0) {
      p->name += 4;
      p->ty = FunName;
      return True;
   }
   if (VG_(strncmp)(p->name, "obj:", 4) == 0) {
      p->name += 4;
      p->ty = ObjName;
      return True;
   }
   if (VG_(strcmp)(p->name, "...") == 0) {
      p->name = NULL;
      p->ty = DotDotDot;
      return True;
   }
   VG_(printf)("location should be \"...\", or should start "
               "with \"fun:\" or \"obj:\"\n");
   return False;
}


/* Look for "tool" in a string like "tool1,tool2,tool3" */
static Bool tool_name_present(Char *name, Char *names)
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

/* Read suppressions from the file specified in VG_(clo_suppressions)
   and place them in the suppressions list.  If there's any difficulty
   doing this, just give up -- there's no point in trying to recover.  
*/
static void load_one_suppressions_file ( Char* filename )
{
   SysRes sres;
   Int    fd, i, j, lineno = 0;
   Bool   eof;
   SizeT  nBuf = 200;
   Char*  buf = VG_(malloc)("errormgr.losf.1", nBuf);
   Char*  tool_names;
   Char*  supp_name;
   Char*  err_str = NULL;
   SuppLoc tmp_callers[VG_MAX_SUPP_CALLERS];

   // Check it's not a directory.
   if (VG_(is_dir)( filename )) {
      if (VG_(clo_xml))
         VG_(printf_xml)("</valgrindoutput>\n");
      VG_(umsg)("FATAL: suppressions file \"%s\" is a directory\n", filename );
      VG_(exit)(1);
   }

   // Open the suppression file.
   sres = VG_(open)( filename, VKI_O_RDONLY, 0 );
   if (sr_isError(sres)) {
      if (VG_(clo_xml))
         VG_(printf_xml)("</valgrindoutput>\n");
      VG_(umsg)("FATAL: can't open suppressions file \"%s\"\n", filename );
      VG_(exit)(1);
   }
   fd = sr_Res(sres);

#  define BOMB(S)  { err_str = S;  goto syntax_error; }

   while (True) {
      /* Assign and initialise the two suppression halves (core and tool) */
      Supp* supp;
      supp        = VG_(arena_malloc)(VG_AR_CORE, "errormgr.losf.1",
                                      sizeof(Supp));
      supp->count = 0;

      // Initialise temporary reading-in buffer.
      for (i = 0; i < VG_MAX_SUPP_CALLERS; i++) {
         tmp_callers[i].ty   = NoName;
         tmp_callers[i].name = NULL;
      }

      supp->string = supp->extra = NULL;

      eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );
      if (eof) break;

      if (!VG_STREQ(buf, "{")) BOMB("expected '{' or end-of-file");
      
      eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );

      if (eof || VG_STREQ(buf, "}")) BOMB("unexpected '}'");

      supp->sname = VG_(arena_strdup)(VG_AR_CORE, "errormgr.losf.2", buf);

      eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );

      if (eof) BOMB("unexpected end-of-file");

      /* Check it has the "tool1,tool2,...:supp" form (look for ':') */
      i = 0;
      while (True) {
         if (buf[i] == ':')  break;
         if (buf[i] == '\0') BOMB("malformed 'tool1,tool2,...:supp' line");
         i++;
      }
      buf[i]    = '\0';    /* Replace ':', splitting into two strings */

      tool_names = & buf[0];
      supp_name  = & buf[i+1];

      if (VG_(needs).core_errors && tool_name_present("core", tool_names))
      {
         // A core suppression
         //(example code, see comment on CoreSuppKind above)
         //if (VG_STREQ(supp_name, "Thread"))
         //   supp->skind = ThreadSupp;
         //else
            BOMB("unknown core suppression type");
      }
      else if (VG_(needs).tool_errors && 
               tool_name_present(VG_(details).name, tool_names))
      {
         // A tool suppression
         if (VG_TDICT_CALL(tool_recognised_suppression, supp_name, supp)) {
            /* Do nothing, function fills in supp->skind */
         } else {
            BOMB("unknown tool suppression type");
         }
      }
      else {
         // Ignore rest of suppression
         while (True) {
            eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );
            if (eof) BOMB("unexpected end-of-file");
            if (VG_STREQ(buf, "}"))
               break;
         }
         continue;
      }

      if (VG_(needs).tool_errors && 
          !VG_TDICT_CALL(tool_read_extra_suppression_info,
                         fd, &buf, &nBuf, supp))
      {
         BOMB("bad or missing extra suppression info");
      }

      /* the main frame-descriptor reading loop */
      i = 0;
      while (True) {
         eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );
         if (eof)
            BOMB("unexpected end-of-file");
         if (VG_STREQ(buf, "}")) {
            if (i > 0) {
               break;
            } else {
               BOMB("missing stack trace");
            }
         }
         if (i == VG_MAX_SUPP_CALLERS)
            BOMB("too many callers in stack trace");
         if (i > 0 && i >= VG_(clo_backtrace_size)) 
            break;
         tmp_callers[i].name = VG_(arena_strdup)(VG_AR_CORE,
                                                 "errormgr.losf.3", buf);
         if (!setLocationTy(&(tmp_callers[i])))
            BOMB("location should be \"...\", or should start "
                 "with \"fun:\" or \"obj:\"");
         i++;
      }

      // If the num callers is >= VG_(clo_backtrace_size), ignore any extra
      // lines and grab the '}'.
      if (!VG_STREQ(buf, "}")) {
         do {
            eof = VG_(get_line) ( fd, &buf, &nBuf, &lineno );
         } while (!eof && !VG_STREQ(buf, "}"));
      }

      // Reject entries which are entirely composed of frame
      // level wildcards.
      vg_assert(i > 0); // guaranteed by frame-descriptor reading loop
      for (j = 0; j < i; j++) {
         if (tmp_callers[j].ty == FunName || tmp_callers[j].ty == ObjName)
            break;
         vg_assert(tmp_callers[j].ty == DotDotDot);
      }
      vg_assert(j >= 0 && j <= i);
      if (j == i) {
         // we didn't find any non-"..." entries
         BOMB("suppression must contain at least one location "
              "line which is not \"...\"");
      } 

      // Copy tmp_callers[] into supp->callers[]
      supp->n_callers = i;
      supp->callers = VG_(arena_malloc)(VG_AR_CORE, "errormgr.losf.4",
                                        i*sizeof(SuppLoc));
      for (i = 0; i < supp->n_callers; i++) {
         supp->callers[i] = tmp_callers[i];
      }

      supp->next = suppressions;
      suppressions = supp;
   }
   VG_(free)(buf);
   VG_(close)(fd);
   return;

  syntax_error:
   if (VG_(clo_xml))
      VG_(printf_xml)("</valgrindoutput>\n");
   VG_(umsg)("FATAL: in suppressions file \"%s\" near line %d:\n",
           filename, lineno );
   VG_(umsg)("   %s\n", err_str );
   
   VG_(close)(fd);
   VG_(umsg)("exiting now.\n");
   VG_(exit)(1);

#  undef BOMB
}


void VG_(load_suppressions) ( void )
{
   Int i;
   suppressions = NULL;
   for (i = 0; i < VG_(clo_n_suppressions); i++) {
      if (VG_(clo_verbosity) > 1) {
         VG_(dmsg)("Reading suppressions file: %s\n", 
                   VG_(clo_suppressions)[i] );
      }
      load_one_suppressions_file( VG_(clo_suppressions)[i] );
   }
}


/*------------------------------------------------------------*/
/*--- Matching errors to suppressions                      ---*/
/*------------------------------------------------------------*/

/* Parameterising functions for the use of VG_(generic_match) in
   suppression-vs-error matching.  The suppression frames (SuppLoc)
   play the role of 'pattern'-element, and the error frames (IPs,
   hence simply Addrs) play the role of 'input'.  In short then, we're
   matching a sequence of Addrs against a pattern composed of a
   sequence of SuppLocs.
*/
static Bool supploc_IsStar ( void* supplocV )
{
   SuppLoc* supploc = (SuppLoc*)supplocV;
   return supploc->ty == DotDotDot;
}

static Bool supploc_IsQuery ( void* supplocV )
{
   return False; /* there's no '?' equivalent in the supp syntax */
}

static Bool supp_pattEQinp ( void* supplocV, void* addrV )
{
   SuppLoc* supploc = (SuppLoc*)supplocV; /* PATTERN */
   Addr     ip      = *(Addr*)addrV; /* INPUT */

   Char caller_name[ERRTXT_LEN];
   caller_name[0] = 0;

   /* So, does this IP address match this suppression-line? */
   switch (supploc->ty) {
      case DotDotDot:
         /* supp_pattEQinp is a callback from VG_(generic_match).  As
            per the spec thereof (see include/pub_tool_seqmatch.h), we
            should never get called with a pattern value for which the
            _IsStar or _IsQuery function would return True.  Hence
            this can't happen. */
         vg_assert(0);
      case ObjName:
         /* Get the object name into 'caller_name', or "???"
            if unknown. */
         if (!VG_(get_objname)(ip, caller_name, ERRTXT_LEN))
            VG_(strcpy)(caller_name, "???");
         break; 
      case FunName: 
         /* Get the function name into 'caller_name', or "???"
            if unknown. */
         // Nb: C++-mangled names are used in suppressions.  Do, though,
         // Z-demangle them, since otherwise it's possible to wind
         // up comparing "malloc" in the suppression against
         // "_vgrZU_libcZdsoZa_malloc" in the backtrace, and the
         // two of them need to be made to match.
         if (!VG_(get_fnname_no_cxx_demangle)(ip, caller_name, ERRTXT_LEN))
            VG_(strcpy)(caller_name, "???");
         break;
      default:
        vg_assert(0);
   }

   /* So now we have the function or object name in caller_name, and
      the pattern (at the character level) to match against is in
      supploc->name.  Hence (and leading to a re-entrant call of
      VG_(generic_match)): */
   return VG_(string_match)(supploc->name, caller_name);
}

/////////////////////////////////////////////////////

static Bool supp_matches_callers(Error* err, Supp* su)
{
   /* Unwrap the args and set up the correct parameterisation of
      VG_(generic_match), using supploc_IsStar, supploc_IsQuery and
      supp_pattEQinp. */
   /* note, StackTrace === Addr* */
   StackTrace ips      = VG_(get_ExeContext_StackTrace)(err->where);
   UWord      n_ips    = VG_(get_ExeContext_n_ips)(err->where);
   SuppLoc*   supps    = su->callers;
   UWord      n_supps  = su->n_callers;
   UWord      szbPatt  = sizeof(SuppLoc);
   UWord      szbInput = sizeof(Addr);
   Bool       matchAll = False; /* we just want to match a prefix */
   return
      VG_(generic_match)(
         matchAll,
         /*PATT*/supps, szbPatt, n_supps, 0/*initial Ix*/,
         /*INPUT*/ips, szbInput, n_ips,  0/*initial Ix*/,
         supploc_IsStar, supploc_IsQuery, supp_pattEQinp
      );
}

/////////////////////////////////////////////////////

static
Bool supp_matches_error(Supp* su, Error* err)
{
   switch (su->skind) {
      //(example code, see comment on CoreSuppKind above)
      //case ThreadSupp:
      //   return (err->ekind == ThreadErr);
      default:
         if (VG_(needs).tool_errors) {
            return VG_TDICT_CALL(tool_error_matches_suppression, err, su);
         } else {
            VG_(printf)(
               "\nUnhandled suppression type: %u.  VG_(needs).tool_errors\n"
               "probably needs to be set.\n",
               err->ekind);
            VG_(tool_panic)("unhandled suppression type");
         }
   }
}

/////////////////////////////////////////////////////

/* Does an error context match a suppression?  ie is this a suppressible
   error?  If so, return a pointer to the Supp record, otherwise NULL.
   Tries to minimise the number of symbol searches since they are expensive.  
*/
static Supp* is_suppressible_error ( Error* err )
{
   Supp* su;
   Supp* su_prev;

   /* stats gathering */
   em_supplist_searches++;

   /* See if the error context matches any suppression. */
   su_prev = NULL;
   for (su = suppressions; su != NULL; su = su->next) {
      em_supplist_cmps++;
      if (supp_matches_error(su, err) && supp_matches_callers(err, su)) {
         /* got a match.  Move this entry to the head of the list
            in the hope of making future searches cheaper. */
         if (su_prev) {
            vg_assert(su_prev->next == su);
            su_prev->next = su->next;
            su->next = suppressions;
            suppressions = su;
         }
         return su;
      }
      su_prev = su;
   }
   return NULL;      /* no matches */
}

/* Show accumulated error-list and suppression-list search stats. 
*/
void VG_(print_errormgr_stats) ( void )
{
   VG_(dmsg)(
      " errormgr: %'lu supplist searches, %'lu comparisons during search\n",
      em_supplist_searches, em_supplist_cmps
   );
   VG_(dmsg)(
      " errormgr: %'lu errlist searches, %'lu comparisons during search\n",
      em_errlist_searches, em_errlist_cmps
   );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
