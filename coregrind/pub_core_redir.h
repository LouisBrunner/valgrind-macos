
/*--------------------------------------------------------------------*/
/*--- Code redirections.                          pub_core_redir.h ---*/
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

#ifndef __PUB_CORE_REDIR_H
#define __PUB_CORE_REDIR_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with:
// - code replacement: intercepting calls to client functions, and
//   pointing them to a different piece of code.
// - loading notification: telling the core where certain client-space
//   functions are when they get loaded.
// - function wrapping: add calls to code before and after client
//   functions execute, for inspection and/or modification.
//
// It's possible that this should be two or three modules.
//--------------------------------------------------------------------

#include "pub_tool_redir.h"

//--------------------------------------------------------------------
// General
//--------------------------------------------------------------------

// This module needs be told about all the symbols that get loaded, so 
// it can check if it needs to do anything special.  This is the function
// that does that checking.  It modifies 'symbol' in-place by Z-decoding
// it if necessary.
void VG_(maybe_redir_or_notify) ( Char* symbol, Addr addr );

//--------------------------------------------------------------------
// Code replacement
//--------------------------------------------------------------------

// See include/pub_tool_redir.h for details on how to do code replacement.

typedef struct _CodeRedirect CodeRedirect;

// This is the crucial redirection function.  It answers the question: 
// should this code address be redirected somewhere else?  It's used just
// before translating a basic block.
extern Addr VG_(code_redirect) ( Addr orig );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

extern void VG_(resolve_existing_redirs_with_seginfo)(SegInfo *si);


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

#define VG_NOTIFY_ON_LOAD(name)           _vgw_##name
#define VG_NOTIFY_ON_LOAD_PREFIX          "_vgw_"
#define VG_NOTIFY_ON_LOAD_PREFIX_LEN      5

// Called by m_main to get our __libc_freeres wrapper.
extern Addr VG_(get_libc_freeres_wrapper)(void);

//--------------------------------------------------------------------
// Function wrapping
//--------------------------------------------------------------------

// This is currently not working(?) --njn

/* Wrapping machinery */
enum return_type {
   RT_RETURN,
   RT_LONGJMP,
   RT_EXIT,
};

typedef struct _FuncWrapper FuncWrapper;
struct _FuncWrapper {
   void *(*before)(va_list args);
   void  (*after) (void *nonce, enum return_type, Word retval);
};

extern void VG_(wrap_function)(Addr eip, const FuncWrapper *wrapper);
extern const FuncWrapper *VG_(is_wrapped)(Addr eip);
extern Bool VG_(is_wrapper_return)(Addr eip);

/* Primary interface for adding wrappers for client-side functions. */
extern CodeRedirect *VG_(add_wrapper)(const Char *from_lib, const Char *from_sym,
				      const FuncWrapper *wrapper);

extern Bool VG_(is_resolved)(const CodeRedirect *redir);

#endif   // __PUB_CORE_REDIR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
