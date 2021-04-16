
/*--------------------------------------------------------------------*/
/*--- Search PATH for an executable                                ---*/
/*---                                                 m_pathscan.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2017 OpenWorks LLP
      info@open-works.co.uk

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

#ifndef __PUB_CORE_PATHSCAN_H
#define __PUB_CORE_PATHSCAN_H

#include "pub_core_basics.h"   // HChar

extern const HChar* VG_(find_executable) ( const HChar* exec );

#endif // ndef __PUB_CORE_PATHSCAN_H
