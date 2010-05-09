
/*--------------------------------------------------------------------*/
/*--- Function replacement and wrapping.          pub_core_redir.h ---*/
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

#ifndef __PUB_CORE_REDIR_H
#define __PUB_CORE_REDIR_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with:
//
// - code replacement: intercepting calls to client functions, and
//   pointing them to a different piece of code.
//
// - loading notification: telling the core where certain client-space
//   functions are when they get loaded.
//
// - function wrapping: add calls to code before and after client
//   functions execute, for inspection and/or modification.
//
// - checking of --require-text-symbol= specifications: when a new
//   object is loaded, its symbol table is examined, and if a symbol
//   (as required by the specifications) is not found then the run
//   is aborted.  See comment by VG_(clo_n_req_tsyms) in
//   pub_core_options.h for background.  This doesn't have anything
//   to do with function intercepting or wrapping, but it does have
//   to do with examining all symbols at object load time, so this
//   module seems like a logical place to put it.
//
//--------------------------------------------------------------------

#include "pub_tool_redir.h"


//--------------------------------------------------------------------
// Notifications - by which we are told of state changes
//--------------------------------------------------------------------

/* Notify the module of a new DebugInfo (called from m_debuginfo). */
extern void VG_(redir_notify_new_DebugInfo)( DebugInfo* );

/* Notify the module of the disappearance of a DebugInfo (also called
   from m_debuginfo). */
extern void VG_(redir_notify_delete_DebugInfo)( DebugInfo* );

/* Initialise the module, and load initial "hardwired" redirects. */
extern void VG_(redir_initialise)( void );

/* Notify the module of a new target for an indirect function. */
extern void VG_(redir_add_ifunc_target)( Addr old_from, Addr new_from );

//--------------------------------------------------------------------
// Queries
//--------------------------------------------------------------------

/* This is the crucial redirection function.  It answers the question:
   should this code address be redirected somewhere else?  It's used
   just before translating a basic block.  If a redir is found,
   *isWrap allows to distinguish wrap- from replace- style
   redirections. */
extern Addr VG_(redir_do_lookup) ( Addr orig, Bool* isWrap );


//--------------------------------------------------------------------
// Loading notification
//--------------------------------------------------------------------

/* Functions named with this macro have the property that the core will
   be told what their address is when they are loaded.  This can be useful
   if the core wants to call them at some point, and so needs to know their
   address.  This is a weaker but more general mechanism than code
   replacement.

   Functions named with this macro should be in client space, ie. in
   vgpreload_<tool>.h or vgpreload_core.h. */

#define VG_NOTIFY_ON_LOAD(name)           _vgnU_##name
#define VG_NOTIFY_ON_LOAD_PREFIX          "_vgnU_"
#define VG_NOTIFY_ON_LOAD_PREFIX_LEN      6


//--------------------------------------------------------------------
// Function wrapping
//--------------------------------------------------------------------

// This is currently not working(?) --njn

/* Wrapping machinery */
//enum return_type {
  //   RT_RETURN,
   //   RT_LONGJMP,
   //   RT_EXIT,
   //};
//
//typedef struct _FuncWrapper FuncWrapper;
//struct _FuncWrapper {
  //   void *(*before)(va_list args);
  //   void  (*after) (void *nonce, enum return_type, Word retval);
  //};
//
//extern void VG_(wrap_function)(Addr eip, const FuncWrapper *wrapper);
//extern const FuncWrapper *VG_(is_wrapped)(Addr eip);
//extern Bool VG_(is_wrapper_return)(Addr eip);

/* Primary interface for adding wrappers for client-side functions. */
//extern CodeRedirect *VG_(add_wrapper)(const Char *from_lib, const Char *from_sym,
//				      const FuncWrapper *wrapper);
//
//extern Bool VG_(is_resolved)(const CodeRedirect *redir);

#endif   // __PUB_CORE_REDIR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
