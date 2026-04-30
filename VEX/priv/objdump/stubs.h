/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#ifndef __VEX_STUBS_H
#define __VEX_STUBS_H

/* This file contains things that need to be defined to get the
   objdump stuff compiled and to avoid pulling in more header files. */

#define bool _Bool
#define true  1
#define false 0

typedef unsigned char bfd_byte;     // uint8_t  originally
typedef unsigned long long bfd_vma; // uint64_t originally
typedef unsigned long size_t;

#define ATTRIBUTE_FPTR_PRINTF_2                   \
  __attribute__ ((__format__ (__printf__, 2, 3))) \
  __attribute__ ((__nonnull__ (2)));
#define ATTRIBUTE_FPTR_PRINTF_3                   \
  __attribute__ ((__format__ (__printf__, 3, 4))) \
  __attribute__ ((__nonnull__ (3)))
#define ATTRIBUTE_UNUSED __attribute__ ((unused))

/* This is defined in VEX, too. But we want to avoid mixing VEX
   and binutils headers. */
#ifndef NULL
#define NULL ((void *)0)
#endif

#endif // __VEX_STUBS_H
