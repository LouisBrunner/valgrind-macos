
/*------------------------------------------------------------------------*/
/*--- Definitions needing to be shared between source files.           ---*/
/*---                                                          omega.h ---*/
/*------------------------------------------------------------------------*/

/*
   This file is part of Omega, a Valgrind tool for instantly detecting
   memory leaks.

   Copyright (C) 2006-2008 Bryan "Brain Murders" Meredith

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

   The current maintainer is Rich Coe <richard.coe@med.ge.com>.
*/

#ifndef __omega_h
#define __omega_h

#include "valgrind.h"

/*
** Setup client request calls so we can track entering and leaving main().
*/
typedef
enum {
  VG_USERREQ__ENTERING_MAIN = VG_USERREQ_TOOL_BASE('O','M'),
  VG_USERREQ__LEAVING_MAIN
} Vg_OmegaClientRequest;

#define VALGRIND_DO_ENTER_MAIN \
   {unsigned int _qzz_res;                                       \
    VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                      \
                            VG_USERREQ__ENTERING_MAIN,           \
                            0, 0, 0, 0, 0);                      \
   }

#define VALGRIND_DO_LEAVE_MAIN \
   {unsigned int _qzz_res;                                       \
    VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0,                      \
                            VG_USERREQ__LEAVING_MAIN,            \
                            0, 0, 0, 0, 0);                      \
   }

#endif
