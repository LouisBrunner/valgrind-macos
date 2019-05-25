/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Module-local header file for m_sigframe.                     ---*/
/*---                                              priv_sigframe.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2015-2017   Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SIGFRAME_H
#define __PRIV_SIGFRAME_H

#include "pub_core_basics.h"        // types
#include "pub_core_threadstate.h"   // ThreadState

/* --------------- Implemented in sigframe-common.c ---------------*/

Bool ML_(sf_maybe_extend_stack)( const ThreadState *tst, Addr addr,
                                 SizeT size, UInt flags );

#endif   // __PRIV_SIGFRAME_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
