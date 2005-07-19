
/*--------------------------------------------------------------------*/
/*--- Command line options.                     pub_tool_options.h ---*/
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

#ifndef __PUB_TOOL_OPTIONS_H
#define __PUB_TOOL_OPTIONS_H

/* Use these for recognising tool command line options -- stops comparing
   once whitespace is reached. */
#define VG_CLO_STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#define VG_CLO_STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

/* Higher-level command-line option recognisers;  use in if/else chains */

#define VG_BOOL_CLO(qq_arg, qq_option, qq_var) \
        if (VG_CLO_STREQ(qq_arg, qq_option"=yes")) { (qq_var) = True; } \
   else if (VG_CLO_STREQ(qq_arg, qq_option"=no"))  { (qq_var) = False; }

#define VG_STR_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = &qq_arg[ VG_(strlen)(qq_option)+1 ]; \
   }

#define VG_NUM_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = (Int)VG_(atoll)( &qq_arg[ VG_(strlen)(qq_option)+1 ] ); \
   }

/* Bounded integer arg */
#define VG_BNUM_CLO(qq_arg, qq_option, qq_var, qq_lo, qq_hi) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      (qq_var) = (Int)VG_(atoll)( &qq_arg[ VG_(strlen)(qq_option)+1 ] ); \
      if ((qq_var) < (qq_lo)) (qq_var) = (qq_lo); \
      if ((qq_var) > (qq_hi)) (qq_var) = (qq_hi); \
   }

/* Verbosity level: 0 = silent, 1 (default), > 1 = more verbose. */
extern Int  VG_(clo_verbosity);

/* Profile?  default: NO */
extern Bool VG_(clo_profile);

/* Emit all messages as XML? default: NO */
/* If clo_xml is set, various other options are set in a non-default
   way.  See vg_main.c and mc_main.c. */
extern Bool VG_(clo_xml);

/* An arbitrary user-supplied string which is copied into the
   XML output, in between <usercomment> tags. */
extern HChar* VG_(clo_xml_user_comment);

/* Call this if a recognised option was bad for some reason.
   Note: don't use it just because an option was unrecognised -- return 'False'
   from VG_(tdict).tool_process_cmd_line_option) to indicate that. */
extern void VG_(bad_option) ( Char* opt );

#endif   // __PUB_TOOL_OPTIONS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
