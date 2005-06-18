
/*--------------------------------------------------------------------*/
/*--- Redirections, etc.                          pub_tool_redir.h ---*/
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

#ifndef __PUB_TOOL_REDIR_H
#define __PUB_TOOL_REDIR_H

/* The following macros facilitate function replacement, which is one form
   of code replacement.

   The general idea is: you can write a function like this:

      ret_type VG_REPLACE_FUNCTION(zEncodedSoname, fnname) ( ... args ... )
      {
         ... body ...
      }

   zEncodedSoname should be a Z-encoded soname (see below for Z-encoding
   details) and fnname should be an unencoded fn name.  The resulting name is

      _vgi_zEncodedSoname_fnname

   The "_vgi_" is a prefix that gets discarded upon decoding.
   
   When it sees this name, the core's symbol-table reading machinery
   and redirection machinery will conspire to cause calls to the function
   'fnname' in object with soname 'zEncodedSoname' to actually be routed to
   the function written here.  We use this below to define dozens of
   replacements of malloc, free, etc.

   The soname must be a Z-encoded bit of text because sonames can
   contain dots etc which are not valid symbol names.  But don't Z-encode
   the function name, since it will already be a valid symbol name, and the
   Z-encoding might screw up the C++ demangling.

   Note that the soname can contain '*' as a wildcard meaning "match
   anything".

   Note also that the replacement function should probably (must be?) in
   client space, so it runs on the simulated CPU.  So it must be in
   either vg_preload_<tool>.so or vg_preload_core.so.
   
   It is important that the Z-encoded soname contains no unencoded 
   underscores, since the intercept-handlers in vg_symtab2.c detect
   the end of the soname by looking for the first trailing underscore.

   Z-encoding details:  the scheme is like GHC's.  It is just about
   readable enough to make a preprocessor unnecessary.  First the "_vgi_"
   prefix is added, and then the following characters are transformed.

     *         -->  Za    ('a' for "asterisk")
     +         -->  Zp
     :         -->  Zc
     .         -->  Zd
     _         -->  Zu
     -         -->  Zh    ('h' for "hyphen")
     (space)   -->  Zs
     Z         -->  ZZ

   Everything else is left unchanged.
*/

#define VG_REPLACE_FUNCTION(soname, fnname)  _vgi_##soname##_##fnname
#define VG_REPLACE_FUNCTION_PREFIX           "_vgi_"
#define VG_REPLACE_FUNCTION_PREFIX_LEN       5

#endif   // __PUB_TOOL_REDIR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
