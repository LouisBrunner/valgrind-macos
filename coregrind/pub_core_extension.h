/*--------------------------------------------------------------------*/
/*--- Extensions                              pub_core_extension.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) IBM Corp. 2024

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

/* Contributed by Andreas Arnez */

#ifndef __PUB_CORE_EXTENSION_H
#define __PUB_CORE_EXTENSION_H

#include "pub_core_basics.h"        // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module contains the extension handling stuff
//--------------------------------------------------------------------

enum ExtensionError {
   ExtErr_OK,
   ExtErr_Illop,
};

extern enum ExtensionError VG_(client_extension) ( ThreadId tid );

#endif
