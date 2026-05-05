/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                     s390_disasm.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __VEX_S390_DISASM_H
#define __VEX_S390_DISASM_H

#include "libvex_basictypes.h"

HChar *s390_disasm(const UChar *, Int);

/*---------------------------------------------------------------*/
/*--- end                                       s390_disasm.h ---*/
/*---------------------------------------------------------------*/

#endif /* __VEX_S390_DISASM_H */
