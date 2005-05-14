/*--------------------------------------------------------------------*/
/*--- Management of function redirection and wrapping.             ---*/
/*---                                                   vg_redir.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2005 Julian Seward 
      jseward@acm.org
   Copyright (C) 2003-2005 Jeremy Fitzhardinge
      jeremy@goop.org

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
#include "vg_symtab2.h"

#include "pub_core_aspacemgr.h"
#include "pub_core_skiplist.h"
#include "pub_core_transtab.h"

/*------------------------------------------------------------*/
/*--- General purpose redirection.                         ---*/
/*------------------------------------------------------------*/

/*
  wraps and redirections, indexed by from_addr

  Redirection and wrapping are two distinct mechanisms which Valgrind
  can use to change the client's control flow.

  Redirection intercepts a call to a client function, and re-points it
  to a new piece of code (presumably functionally equivalent).  The
  original code is never run.

  Wrapping does call the client's original code, but calls "before"
  and "after" functions which can inspect (and perhaps modify) the
  function's arguments and return value.
 */
struct _CodeRedirect {
   enum redir_type {
      R_REDIRECT,		/* plain redirection */
      R_WRAPPER,		/* wrap with valgrind-internal code */
      R_CLIENT_WRAPPER,		/* wrap with client-side code */
   }		type;
   
   const Char	*from_lib;	/* library qualifier pattern */
   const Char	*from_sym;	/* symbol */
   Addr		from_addr;	/* old addr */

   /* used for redirection */
   const Char	*to_lib;	/* library qualifier pattern */
   const Char	*to_sym;	/* symbol */
   Addr		to_addr;	/* new addr */

   /* used for wrapping */
   const FuncWrapper *wrapper;

   CodeRedirect *next;	/* next pointer on unresolved list */
};

static Char *straddr(void *p)
{
   static Char buf[16];

   VG_(sprintf)(buf, "%p", *(Addr *)p);

   return buf;
}

static SkipList sk_resolved_redir = VG_SKIPLIST_INIT(CodeRedirect, from_addr, 
						  VG_(cmp_Addr), straddr, VG_AR_SYMTAB);
static CodeRedirect *unresolved_redir = NULL;

static Bool match_lib(const Char *pattern, const SegInfo *si)
{
   /* pattern == NULL matches everything, otherwise use globbing

      If the pattern starts with:
	file:, then match filename
	soname:, then match soname
	something else, match filename
   */
   const Char *name = si->filename;

   if (pattern == NULL)
      return True;

   if (VG_(strncmp)(pattern, "file:", 5) == 0) {
      pattern += 5;
      name = si->filename;
   }
   if (VG_(strncmp)(pattern, "soname:", 7) == 0) {
      pattern += 7;
      name = si->soname;
   }

   if (name == NULL)
      return False;
   
   return VG_(string_match)(pattern, name);
}

static inline Bool from_resolved(const CodeRedirect *redir)
{
   return redir->from_addr != 0;
}

static inline Bool to_resolved(const CodeRedirect *redir)
{
   if (redir->type == R_REDIRECT)
      return redir->to_addr != 0;
   vg_assert(redir->wrapper != NULL);
   return True;
}

Bool VG_(is_resolved)(const CodeRedirect *redir)
{
   return from_resolved(redir) && to_resolved(redir);
}

static void add_resolved(CodeRedirect *redir)
{
   switch(redir->type) {
   case R_REDIRECT:
      if (VG_(clo_trace_redir)) {
         VG_(message)(Vg_DebugMsg, "  redir resolved (%s:%s=%p -> ",
                      redir->from_lib, redir->from_sym, redir->from_addr);
         VG_(message)(Vg_DebugMsg, "                  %s:%s=%p)",
                      redir->to_lib, redir->to_sym, redir->to_addr);
      }

      if (VG_(search_transtab)(NULL, (Addr64)redir->from_addr, False)) {
         /* For some given (from, to) redir, the "from" function got
            called before the .so containing "to" became available.  We
            know this because there is already a translation for the
            entry point of the original "from".  So the redirect will
            never actually take effect unless that translation is
            discarded.  

            Note, we only really need to discard the first bb of the
            old entry point, and so we avoid the problem of having to
            figure out how big that bb was -- since it is at least 1
            byte of original code, we can just pass 1 as the original
            size to invalidate_translations() and it will indeed get
            rid of the translation. 

            Note, this is potentially expensive -- discarding
            translations causes complete unchaining.  
         */
         if (VG_(clo_verbosity) > 2 && VG_(clo_trace_redir)) {
            VG_(message)(Vg_UserMsg,   
                         "Discarding translation due to redirect of already called function" );
            VG_(message)(Vg_UserMsg,
                         "   %s (%p -> %p)",
                         redir->from_sym, redir->from_addr, redir->to_addr );
         }
         VG_(discard_translations)((Addr64)redir->from_addr, 1);
      }

      {
         CodeRedirect *r = VG_(SkipList_Find_Exact)(&sk_resolved_redir, &redir->from_addr);

         if (r == NULL)
            VG_(SkipList_Insert)(&sk_resolved_redir, redir);
         else {
            /* XXX leak redir */
            if (VG_(clo_trace_redir))
               VG_(message)(Vg_DebugMsg, "  redir %s:%s:%p->%s:%s:%p duplicated\n",
                            redir->from_lib, redir->from_sym, redir->from_addr,
                            redir->to_lib, redir->to_sym, redir->to_addr);
         }
      }
      break;

   case R_WRAPPER:
      if (VG_(clo_trace_redir)) {
         VG_(message)(Vg_DebugMsg, "  wrapper resolved (%s:%s=%p -> wrapper)",
                      redir->from_lib, redir->from_sym, redir->from_addr);
      }

      /* XXX redir leaked */
      //VG_(wrap_function)(redir->from_addr, redir->wrapper);
      break;

   case R_CLIENT_WRAPPER:
      VG_(core_panic)("not implemented");
      break;
   }
}

/* Resolve a redir using si if possible, and add it to the resolved
   list */
static Bool resolve_redir(CodeRedirect *redir, const SegInfo *si)
{
   Bool resolved;

   vg_assert(si != NULL);
   vg_assert(si->seg != NULL);

   /* no redirection from Valgrind segments */
   if (si->seg->flags & SF_VALGRIND)
      return False;

   resolved = VG_(is_resolved)(redir);

   if (0 && VG_(clo_trace_redir))
      VG_(printf)("   consider FROM binding %s:%s -> %s:%s in %s(%s)\n",
		  redir->from_lib, redir->from_sym,
		  redir->to_lib, redir->to_sym,
		  si->filename, si->soname);

   vg_assert(!resolved);

   if (!from_resolved(redir)) {
      vg_assert(redir->from_sym != NULL);

      if (match_lib(redir->from_lib, si)) {
	 redir->from_addr = VG_(reverse_search_one_symtab)(si, redir->from_sym);
	 if (VG_(clo_trace_redir) && redir->from_addr != 0)
	    VG_(printf)("   bind FROM: %p = %s:%s\n", 
                        redir->from_addr,redir->from_lib, redir->from_sym );
      }
   }

   if (!to_resolved(redir)) {
      vg_assert(redir->type == R_REDIRECT);
      vg_assert(redir->to_sym != NULL);

      if (match_lib(redir->to_lib, si)) {
	 redir->to_addr = VG_(reverse_search_one_symtab)(si, redir->to_sym);
	 if (VG_(clo_trace_redir) && redir->to_addr != 0)
	    VG_(printf)("   bind   TO: %p = %s:%s\n", 
                        redir->to_addr,redir->to_lib, redir->to_sym );

      }
   }

   resolved = from_resolved(redir) && to_resolved(redir);

   if (0 && VG_(clo_trace_redir))
      VG_(printf)("resolve_redir: %s:%s from=%p %s:%s to=%p\n",
		  redir->from_lib, redir->from_sym, redir->from_addr, 
		  redir->to_lib, redir->to_sym, redir->to_addr);

   if (resolved) add_resolved(redir);

   return resolved;
}

static Bool resolve_redir_allsegs(CodeRedirect *redir)
{
   const SegInfo *si;

   for(si = VG_(next_seginfo)(NULL); 
       si != NULL; 
       si = VG_(next_seginfo)(si))
   {
      if (resolve_redir(redir, si))
	 return True;
   }
   return False;
}

/* Go through the complete redir list, resolving as much as possible with this SegInfo.

    This should be called when a new SegInfo symtab is loaded.
 */
void VG_(resolve_seg_redirs)(SegInfo *si)
{
   CodeRedirect **prevp = &unresolved_redir;
   CodeRedirect *redir, *next;

   if (VG_(clo_trace_redir))
      VG_(printf)("Considering redirs to/from %s(soname=%s)\n",
                  si->filename, si->soname);

   /* visit each unresolved redir - if it becomes resolved, then
      remove it from the unresolved list */
   for(redir = unresolved_redir; redir != NULL; redir = next) {
      next = redir->next;

      if (resolve_redir(redir, si)) {
	 *prevp = next;
	 redir->next = NULL;
      } else
	 prevp = &redir->next;
   }
}

/* Redirect a lib/symbol reference to a function at lib/symbol */
static void add_redirect_sym_to_sym(const Char *from_lib, const Char *from_sym,
				    const Char *to_lib, const Char *to_sym)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   redir->type = R_REDIRECT;

   redir->from_lib = VG_(arena_strdup)(VG_AR_SYMTAB, from_lib);
   redir->from_sym = VG_(arena_strdup)(VG_AR_SYMTAB, from_sym);
   redir->from_addr = 0;

   redir->to_lib = VG_(arena_strdup)(VG_AR_SYMTAB, to_lib);
   redir->to_sym = VG_(arena_strdup)(VG_AR_SYMTAB, to_sym);
   redir->to_addr = 0;

   if (VG_(clo_verbosity) >= 2 && VG_(clo_trace_redir))
      VG_(message)(Vg_UserMsg, 
                   "REDIRECT %s(%s) to %s(%s)",
                   from_lib, from_sym, to_lib, to_sym);

   /* Check against all existing segments to see if this redirection
      can be resolved immediately */
   if (!resolve_redir_allsegs(redir)) {
      /* nope, add to list */
      redir->next = unresolved_redir;
      unresolved_redir = redir;
   }
}

/* Redirect a lib/symbol reference to a function at addr */
void VG_(add_redirect_sym_to_addr)(const Char *from_lib, const Char *from_sym,
				   Addr to_addr)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   redir->type = R_REDIRECT;

   redir->from_lib = VG_(arena_strdup)(VG_AR_SYMTAB, from_lib);
   redir->from_sym = VG_(arena_strdup)(VG_AR_SYMTAB, from_sym);
   redir->from_addr = 0;

   redir->to_lib = NULL;
   redir->to_sym = NULL;
   redir->to_addr = to_addr;

   if (VG_(clo_verbosity) >= 2 && VG_(clo_trace_redir))
      VG_(message)(Vg_UserMsg, 
                   "REDIRECT %s(%s) to %p",
                   from_lib, from_sym, to_addr);

    /* Check against all existing segments to see if this redirection
       can be resolved immediately */
   if (!resolve_redir_allsegs(redir)) {
      /* nope, add to list */
      redir->next = unresolved_redir;
      unresolved_redir = redir;
   }
}

/* Redirect a function at from_addr to a function at to_addr */
void VG_(add_redirect_addr_to_addr)(Addr from_addr, Addr to_addr)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   redir->type = R_REDIRECT;

   redir->from_lib = NULL;
   redir->from_sym = NULL;
   redir->from_addr = from_addr;

   redir->to_lib = NULL;
   redir->to_sym = NULL;
   redir->to_addr = to_addr;

   if (VG_(clo_verbosity) >= 2 && VG_(clo_trace_redir))
      VG_(message)(Vg_UserMsg, 
                   "REDIRECT %p to %p",
                   from_addr, to_addr);

   add_resolved(redir);
}

CodeRedirect *VG_(add_wrapper)(const Char *from_lib, const Char *from_sym,
			       const FuncWrapper *wrapper)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   if (0)
      VG_(printf)("adding wrapper for %s:%s -> (%p,%p)\n",
		  from_lib, from_sym, wrapper->before, wrapper->after);

   redir->type = R_WRAPPER;

   redir->from_lib = VG_(arena_strdup)(VG_AR_SYMTAB, from_lib);
   redir->from_sym = VG_(arena_strdup)(VG_AR_SYMTAB, from_sym);
   redir->from_addr = 0;

   redir->to_lib = NULL;
   redir->to_sym = NULL;
   redir->to_addr = 0;

   redir->wrapper = wrapper;
   
   /* Check against all existing segments to see if this redirection
      can be resolved immediately */
   if (!resolve_redir_allsegs(redir)) {
      /* nope, add to list */
      redir->next = unresolved_redir;
      unresolved_redir = redir;
   }

   return redir;
}

/* If address 'a' is being redirected, return the redirected-to
   address. */
Addr VG_(code_redirect)(Addr a)
{
   CodeRedirect* r;

   r = VG_(SkipList_Find_Exact)(&sk_resolved_redir, &a);
   if (r == NULL)
      return a;

   vg_assert(r->to_addr != 0);

   return r->to_addr;
}

void VG_(setup_code_redirect_table) ( void )
{
   /* Overenthusiastic use of PLT bypassing by the glibc people also
      means we need to patch the following functions to our own
      implementations of said, in mac_replace_strmem.c.
    */
   add_redirect_sym_to_sym("soname:libc.so.6", "stpcpy",
                           "*vgpreload_memcheck.so*", "stpcpy");

   add_redirect_sym_to_sym("soname:ld-linux.so.2", "strlen",
                           "*vgpreload_memcheck.so*", "strlen");
   add_redirect_sym_to_sym("soname:libc.so.6", "strlen",
                           "*vgpreload_memcheck.so*", "strlen");

   add_redirect_sym_to_sym("soname:libc.so.6", "strnlen",
                           "*vgpreload_memcheck.so*", "strnlen");

   add_redirect_sym_to_sym("soname:ld-linux.so.2", "stpcpy",
                           "*vgpreload_memcheck.so*", "stpcpy");
   add_redirect_sym_to_sym("soname:libc.so.6", "stpcpy",
                           "*vgpreload_memcheck.so*", "stpcpy");

   add_redirect_sym_to_sym("soname:libc.so.6", "strchr",
                           "*vgpreload_memcheck.so*", "strchr");
   add_redirect_sym_to_sym("soname:ld-linux.so.2", "strchr",
                           "*vgpreload_memcheck.so*", "strchr");

   add_redirect_sym_to_sym("soname:libc.so.6", "strchrnul",
                           "*vgpreload_memcheck.so*", "glibc232_strchrnul");

   add_redirect_sym_to_sym("soname:libc.so.6", "rawmemchr",
                           "*vgpreload_memcheck.so*", "glibc232_rawmemchr");

   /* amd64-linux (glibc 2.3.3, SuSE 9.2) */
   /* apparently index is the same thing as strchr */
   add_redirect_sym_to_sym("soname:libc.so.6", "index",
                           "*vgpreload_memcheck.so*", "strchr");
   add_redirect_sym_to_sym("soname:ld-linux-x86-64.so.2", "index",
                           "*vgpreload_memcheck.so*", "strchr");

   add_redirect_sym_to_sym("soname:libc.so.6", "strcpy",
                           "*vgpreload_memcheck.so*", "strcpy");

   add_redirect_sym_to_sym("soname:ld-linux-x86-64.so.2", "strcmp",
                           "*vgpreload_memcheck.so*", "strcmp");
   add_redirect_sym_to_sym("soname:libc.so.6", "strcmp",
                           "*vgpreload_memcheck.so*", "strcmp");

   add_redirect_sym_to_sym("soname:ld-linux-x86-64.so.2", "strlen",
                           "*vgpreload_memcheck.so*", "strlen");
}


//:: /*------------------------------------------------------------*/
//:: /*--- General function wrapping.                           ---*/
//:: /*------------------------------------------------------------*/
//:: 
//:: /* 
//::    TODO:
//::    - hook into the symtab machinery
//::    - client-side wrappers?
//::    - better interfaces for before() functions to get to arguments
//::    - handle munmap of code (dlclose())
//::    - handle thread exit
//::    - handle longjmp
//::  */
//:: struct callkey {
//::    ThreadId	tid;		/* calling thread	    */
//::    Addr		esp;		/* address of args on stack */
//::    Addr		eip;		/* return address	    */
//:: };
//:: 
//:: struct call_instance {
//::    struct callkey key;
//:: 
//::    const FuncWrapper	*wrapper;
//::    void		*nonce;
//:: };
//:: 
//:: static inline Addr addrcmp(Addr a, Addr b)
//:: {
//::    if (a < b)
//::       return -1;
//::    else if (a > b)
//::       return 1;
//::    else 
//::       return 0;
//:: }
//:: 
//:: static inline Int cmp(UInt a, UInt b)
//:: {
//::    if (a < b)
//::       return -1;
//::    else if (a > b)
//::       return 1;
//::    else 
//::       return 0;
//:: }
//:: 
//:: static Int keycmp(const void *pa, const void *pb)
//:: {
//::    const struct callkey *a = (const struct callkey *)pa;
//::    const struct callkey *b = (const struct callkey *)pb;
//::    Int ret;
//:: 
//::    if ((ret = cmp(a->tid, b->tid)))
//::       return ret;
//:: 
//::    if ((ret = addrcmp(a->esp, b->esp)))
//::       return ret;
//:: 
//::    return addrcmp(a->eip, b->eip);
//:: }
//:: 
//:: /* List of wrapped call invocations which are currently active */
//:: static SkipList wrapped_frames = VG_SKIPLIST_INIT(struct call_instance, key, keycmp, 
//:: 					       NULL, VG_AR_SYMTAB);
//:: 
//:: static struct call_instance *find_call(Addr retaddr, Addr argsp, ThreadId tid)
//:: {
//::    struct callkey key = { tid, argsp, retaddr };
//:: 
//::    return VG_(SkipList_Find_Exact)(&wrapped_frames, &key);
//:: }
//:: 
//:: static void wrapper_return(Addr retaddr);
//:: 
//:: /* Called from generated code via helper */
//:: void VG_(wrap_before)(ThreadState *tst, const FuncWrapper *wrapper)
//:: {
//::    Addr retaddr = VGA_RETADDR(tst->arch);
//::    Addr argp = (Addr)&VGA_FUNC_ARG(tst->arch, 0);
//::    void *nonce = NULL;
//::    Bool mf = VG_(my_fault);
//::    VG_(my_fault) = True;
//:: 
//::    if (wrapper->before) {
//::       va_list args = VGA_VA_LIST(tst->arch);
//::       nonce = (*wrapper->before)(args);
//::    }
//:: 
//::    if (wrapper->after) {
//::       /* If there's an after function, make sure it gets called */
//::       struct call_instance *call;
//:: 
//::       call = find_call(retaddr, argp, tst->tid);
//:: 
//::       if (call != NULL) {
//:: 	 /* Found a stale outstanding call; clean it up and recycle
//:: 	    the structure */
//:: 	 if (call->wrapper->after)
//:: 	    (*call->wrapper->after)(call->nonce, RT_LONGJMP, 0);
//::       } else {
//:: 	 call = VG_(SkipNode_Alloc)(&wrapped_frames);
//:: 	 
//:: 	 call->key.tid = tst->tid;
//:: 	 call->key.esp = argp;
//:: 	 call->key.eip = retaddr;
//:: 
//:: 	 VG_(SkipList_Insert)(&wrapped_frames, call);
//:: 
//:: 	 wrapper_return(retaddr);
//::       }
//:: 
//::       call->wrapper = wrapper;
//::       call->nonce = nonce;
//::    } else 
//::       vg_assert(nonce == NULL);
//:: 
//::    VG_(my_fault) = mf;
//:: }
//:: 
//:: /* Called from generated code via helper */
//:: void VG_(wrap_after)(ThreadState *tst)
//:: {
//::    Addr EIP = VGA_INSTR_PTR(tst->arch);	/* instruction after call */
//::    Addr ESP = VGA_STACK_PTR(tst->arch);	/* pointer to args */
//::    Word ret = VGA_RETVAL(tst->arch);		/* return value */
//::    struct call_instance *call;
//::    Bool mf = VG_(my_fault);
//:: 
//::    VG_(my_fault) = True;
//::    call = find_call(EIP, ESP, tst->tid);
//:: 
//::    if (0)
//::       VG_(printf)("wrap_after(%p,%p,%d) -> %p\n", EIP, ESP, tst->tid, call);
//:: 
//::    if (call != NULL) {
//::       if (call->wrapper->after)
//:: 	 (*call->wrapper->after)(call->nonce, RT_RETURN, ret);
//:: 
//::       VG_(SkipList_Remove)(&wrapped_frames, &call->key);
//::       VG_(SkipNode_Free)(&wrapped_frames, call);
//::    }
//::    VG_(my_fault) = mf;
//:: }
//:: 
//:: 
//:: struct wrapped_function {
//::    Addr	eip;			/* eip of function entrypoint */
//::    const FuncWrapper *wrapper;
//:: };
//:: 
//:: struct wrapper_return {
//::    Addr eip;			/* return address */
//:: };
//:: 
//:: /* A mapping from eip of wrapped function entrypoints to actual wrappers */
//:: static SkipList wrapped_functions = VG_SKIPLIST_INIT(struct wrapped_function, eip, VG_(cmp_Addr),
//:: 						  NULL, VG_AR_SYMTAB);
//:: 
//:: /* A set of EIPs which are return addresses for wrapped functions */
//:: static SkipList wrapper_returns = VG_SKIPLIST_INIT(struct wrapper_return, eip, VG_(cmp_Addr),
//:: 						NULL, VG_AR_SYMTAB);
//:: 
//:: /* Wrap function starting at eip */
//:: void VG_(wrap_function)(Addr eip, const FuncWrapper *wrapper)
//:: {
//::    struct wrapped_function *func;
//:: 
//::    if (0)
//::       VG_(printf)("wrapping %p with (%p,%p)\n", eip, wrapper->before, wrapper->after);
//:: 
//::    func = VG_(SkipList_Find_Exact)(&wrapped_functions, &eip);
//:: 
//::    if (func == NULL) {
//::       func = VG_(SkipNode_Alloc)(&wrapped_functions);
//::       VG_(invalidate_translations)(eip, 1, True);
//:: 
//::       func->eip = eip;
//::       VG_(SkipList_Insert)(&wrapped_functions, func);
//::    }
//:: 
//::    func->wrapper = wrapper;
//:: }
//:: 
//:: const FuncWrapper *VG_(is_wrapped)(Addr eip)
//:: {
//::    struct wrapped_function *func = VG_(SkipList_Find_Exact)(&wrapped_functions, &eip);
//:: 
//::    if (func)
//::       return func->wrapper;
//::    return NULL;
//:: }
//:: 
//:: Bool VG_(is_wrapper_return)(Addr eip)
//:: {
//::    struct wrapper_return *ret = VG_(SkipList_Find_Exact)(&wrapper_returns, &eip);
//:: 
//::    return ret != NULL;
//:: }
//:: 
//:: /* Mark eip as being the return address of a wrapper, so that the
//::    codegen will generate the appropriate call. */
//:: void wrapper_return(Addr eip)
//:: {
//::    struct wrapper_return *ret;
//:: 
//::    if (VG_(is_wrapper_return)(eip))
//::       return;
//:: 
//::    VG_(invalidate_translations)(eip, 1, True);
//:: 
//::    ret = VG_(SkipNode_Alloc)(&wrapper_returns);
//::    ret->eip = eip;
//:: 
//::    VG_(SkipList_Insert)(&wrapper_returns, ret);
//:: }
