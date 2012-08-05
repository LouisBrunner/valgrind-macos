
/*--------------------------------------------------------------------*/
/*--- Misc client state info                pub_tool_clientstate.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward
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

#ifndef __PUB_TOOL_CLIENTSTATE_H
#define __PUB_TOOL_CLIENTSTATE_H

/* Note, this header requires pub_{core,tool}_xarray.h to be
   included ahead of it. */

// Command line pieces, after they have been extracted from argv in
// m_main.main().  These are all NULL-terminated vectors.

/* Args for the client. */
extern XArray* /* of HChar* */ VG_(args_for_client);

/* Args for V.  This is the concatenation of the following:
   - contents of ~/.valgrindrc
   - contents of $VALGRIND_OPTS
   - contents of ./.valgrindrc
   - args from the command line
   in the stated order.

   Only the last of these is passed onwards to child Valgrinds at
   client sys_execve, since the children will re-acquire the first 3
   categories for themselves.  Therefore we also record the number of
   these no-pass-at-execve arguments -- that is what
   VG_(args_for_valgrind_noexecpass) is. */
extern XArray* /* of HChar* */ VG_(args_for_valgrind);

/* Number of leading args in VG_(args_for_valgrind) not to pass on at
   exec time. */
extern Int VG_(args_for_valgrind_noexecpass);

/* The name of the client executable, as specified on the command
   line. */
extern const HChar* VG_(args_the_exename);


#endif   // __PUB_TOOL_CLIENTSTATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
