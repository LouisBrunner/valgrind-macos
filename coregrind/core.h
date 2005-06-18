
/*--------------------------------------------------------------------*/
/*--- A header file for various private parts of Valgrind's core.  ---*/
/*---                                                       core.h ---*/
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

#ifndef __CORE_H
#define __CORE_H

#include "tool.h"          // tool stuff

#include "libvex.h"

// XXX: this is needed because pub_core_scheduler needs VexGuestXXXState...
#if defined(VGA_x86)
#  include "libvex_guest_x86.h"
#elif defined(VGA_amd64)
#  include "libvex_guest_amd64.h"
#else
#  error Unknown arch
#endif

#include <setjmp.h>        // for jmp_buf

#include "pub_core_scheduler.h"   // for types 'ThreadArchState'

/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

#endif /* ndef __CORE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

