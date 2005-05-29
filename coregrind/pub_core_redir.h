
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
// - code redirection: intercepting calls to client functions, and
//   pointing them to a different piece of code.
// - function wrapping: add calls to code before and after client
//   functions execute, for inspection and/or modification.
//
// Nb: It's possible that this should be two modules.
//--------------------------------------------------------------------

/* Redirection machinery */
extern Addr VG_(code_redirect) ( Addr orig );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

extern void VG_(add_redirect_sym_to_addr)(const Char *from_lib,
					  const Char *from_sym,
					  Addr to_addr);
extern void VG_(add_redirect_addr_to_addr)(Addr from_addr, Addr to_addr);
extern void VG_(resolve_seg_redirs)(SegInfo *si);

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
