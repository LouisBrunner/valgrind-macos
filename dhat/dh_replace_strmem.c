/*--------------------------------------------------------------------*/
/*--- Replacements for memcpy(), which run on the simulated CPU    ---*/
/*--- simulated CPU.                                               ---*/
/*---                                          dh_replace_strmem.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of DHAT, a Valgrind tool for profiling the
   heap usage of programs.

   Copyright (C) 2020-2020 Nicholas Nethercote

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

#include "dhat.h"

#define RECORD_COPY(_qzz_len) \
  VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__DHAT_COPY, \
                                  (_qzz_len), 0, 0, 0, 0)

#include "../shared/vg_replace_strmem.c"

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
