/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Reading of ARM(32) EXIDX unwind information                  ---*/
/*--                                              priv_readexidx.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Mozilla Foundation

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

/* Contributed by Julian Seward <jseward@acm.org> */

#ifndef __PRIV_READEXIDX_H
#define __PRIV_READEXIDX_H

#include "pub_core_debuginfo.h"   // DebugInfo

extern
void ML_(read_exidx) ( /*MOD*/DebugInfo* di,
                       UChar*   exidx_img, SizeT exidx_size,
                       UChar*   extab_img, SizeT extab_size,
                       Addr     text_last_svma,
                       PtrdiffT text_bias );

#endif /* ndef __PRIV_READEXIDX_H */

/*--------------------------------------------------------------------*/
/*--- end                                         priv_readexidx.h ---*/
/*--------------------------------------------------------------------*/
