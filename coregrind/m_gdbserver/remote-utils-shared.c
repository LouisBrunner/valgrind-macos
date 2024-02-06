/* Shared remote utility routines for the remote server for GDB.
   Copyright (C) 1986, 1989, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005, 2006, 2011
   Free Software Foundation, Inc.

   This file is based on parts of GDB.
   It has been modified to integrate it in valgrind

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* Note this is shared code between vgdb.c and m_gdbserver/server.c.
   It is used directly by vgdb.c using libc functions.
   There is another file m_gdbserver/remote-utils-shared-gdbserver.c
   which first redefines the libc functions to use valgrind VG_ variants
   that is used with server.c.  */

#include "remote-utils-shared.h"

/* When included through remote-utils-shared-gdbserver.c strlen
   is already defined to make to VG_(strlen). Otherwise we are
   building for vgdb, which will use the standard libc functions.  */
#ifndef strlen
#include <string.h>
#endif

/* Convert number NIB to a hex digit.  */
int tohex (int nib)
{
   if (nib < 10)
      return '0' + nib;
   else
      return 'a' + nib - 10;
}

int hexify (char *hex, const char *bin, int count)
{
   int i;

   /* May use a length, or a nul-terminated string as input. */
   if (count == 0)
      count = strlen (bin);

  for (i = 0; i < count; i++) {
     *hex++ = tohex ((*bin >> 4) & 0xf);
     *hex++ = tohex (*bin++ & 0xf);
  }
  *hex = 0;
  return i;
}
