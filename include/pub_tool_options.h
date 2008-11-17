
/*--------------------------------------------------------------------*/
/*--- Command line options.                     pub_tool_options.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2008 Julian Seward
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

#include "libvex.h"              // for VexControl


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

/* Unbounded integer arg */
#define VG_NUM_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      Char* s; \
      Long n = VG_(strtoll10)( &qq_arg[ VG_(strlen)(qq_option)+1 ], &s );\
      (qq_var) = n; \
      /* Check for non-numeralness, or overflow */ \
      if ('\0' != s[0] || (qq_var) != n) VG_(err_bad_option)(qq_arg); \
   }

/* Bounded integer arg */
#define VG_BNUM_CLO(qq_arg, qq_option, qq_var, qq_lo, qq_hi) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      Char* s; \
      Long n = VG_(strtoll10)( &qq_arg[ VG_(strlen)(qq_option)+1 ], &s );\
      (qq_var) = n; \
      /* Check for non-numeralness, or overflow */ \
      if ('\0' != s[0] || (qq_var) != n) VG_(err_bad_option)(qq_arg); \
      if ((qq_var) < (qq_lo)) (qq_var) = (qq_lo); \
      if ((qq_var) > (qq_hi)) (qq_var) = (qq_hi); \
   }

/* Bounded hexadecimal arg */
#define VG_BHEX_CLO(qq_arg, qq_option, qq_var, qq_lo, qq_hi) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      Char* s; \
      Long n = VG_(strtoll16)( &qq_arg[ VG_(strlen)(qq_option)+1 ], &s );\
      (qq_var) = n; \
      /* Check for non-numeralness, or overflow */ \
      if ('\0' != s[0] || (qq_var) != n) VG_(err_bad_option)(qq_arg); \
      if ((qq_var) < (qq_lo)) (qq_var) = (qq_lo); \
      if ((qq_var) > (qq_hi)) (qq_var) = (qq_hi); \
   }

/* Double arg */
#define VG_DBL_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQN(VG_(strlen)(qq_option)+1, qq_arg, qq_option"=")) { \
      Char* s; \
      double n = VG_(strtod)( &qq_arg[ VG_(strlen)(qq_option)+1 ], &s );\
      (qq_var) = n; \
      /* Check for non-numeralness */ \
      if ('\0' != s[0]) VG_(err_bad_option)(qq_arg); \
   }

/* Bool arg whose value is denoted by the exact presence of the given string. */
#define VG_XACT_CLO(qq_arg, qq_option, qq_var) \
   if (VG_CLO_STREQ(qq_arg, qq_option)) { \
      (qq_var) = True; \
   } /* else leave it alone */

/* Verbosity level: 0 = silent, 1 (default), > 1 = more verbose. */
extern Int  VG_(clo_verbosity);

/* Emit all messages as XML? default: NO */
/* If clo_xml is set, various other options are set in a non-default
   way.  See vg_main.c and mc_main.c. */
extern Bool VG_(clo_xml);

/* An arbitrary user-supplied string which is copied into the
   XML output, in between <usercomment> tags. */
extern HChar* VG_(clo_xml_user_comment);

/* Vex iropt control.  Tool-visible so tools can make Vex optimise
   less aggressively if that is needed (callgrind needs this). */
extern VexControl VG_(clo_vex_control);

/* Number of parents of a backtrace.  Default: 8.  */
extern Int   VG_(clo_backtrace_size);

/* Call this if a recognised option was bad for some reason.  Note:
   don't use it just because an option was unrecognised -- return
   'False' from VG_(tdict).tool_process_cmd_line_option) to indicate
   that.  This function prints an error message, then shuts down the
   entire system. */
__attribute__((noreturn))
extern void VG_(err_bad_option) ( Char* opt );

/* Used to expand file names.  "option_name" is the option name, eg.
   "--log-file".  'format' is what follows, eg. "cachegrind.out.%p".  In
   'format': 
   - "%p" is replaced with PID.
   - "%q{QUAL}" is replaced with the environment variable $QUAL.  If $QUAL
     isn't set, we abort.  If the "{QUAL}" part is malformed, we abort.
   - "%%" is replaced with "%".
   Anything else after '%' causes an abort.
   If the format specifies a relative file name, it's put in the program's
   initial working directory.  If it specifies an absolute file name (ie.
   starts with '/') then it is put there.

   Note that "option_name" has no effect on the returned string: the
   returned string depends only on "format" and the PIDs and
   environment variables that it references (if any). "option_name" is
   merely used in printing error messages, if an error message needs
   to be printed due to malformedness of the "format" argument.
*/
extern Char* VG_(expand_file_name)(Char* option_name, Char* format);

#endif   // __PUB_TOOL_OPTIONS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
