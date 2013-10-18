
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vkiscnums.                   ---*/
/*---                                                m_vkiscnums.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2013 OpenWorks LLP
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"

#include "pub_core_vkiscnums.h"     /* self */

/* We have pub_{core,tool}_vkiscnums.h.  This is the matching implementation
   for that interface.  

   On Linux, the interface exports a bunch of "#define __NR_foo 42" style
   definitions, so there is no implementation.
*/

//---------------------------------------------------------------------------
#if defined(VGO_linux)
//---------------------------------------------------------------------------

HChar* VG_(sysnum_string)(Word sysnum, SizeT n_buf, HChar* buf)
{
   VG_(snprintf)(buf, n_buf, "%3ld", sysnum);
   return buf;
}

HChar* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, HChar* buf)
{
   return VG_(sysnum_string)(sysnum, n_buf, buf);
}

//---------------------------------------------------------------------------
#elif defined(VGO_darwin)
//---------------------------------------------------------------------------

HChar* VG_(sysnum_string)(Word sysnum, SizeT n_buf, HChar* buf)
{
   const HChar* classname = NULL;
   switch (VG_DARWIN_SYSNO_CLASS(sysnum)) {
      case VG_DARWIN_SYSCALL_CLASS_MACH: classname = "mach"; break;
      case VG_DARWIN_SYSCALL_CLASS_UNIX: classname = "unix"; break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP: classname = "mdep"; break;
      case VG_DARWIN_SYSCALL_CLASS_DIAG: classname = "diag"; break;
      default: classname = "UNKNOWN"; break;
   }
   VG_(snprintf)(buf, n_buf, "%s:%3ld",
                             classname, VG_DARWIN_SYSNO_INDEX(sysnum));
   return buf;
}

HChar* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, HChar* buf)
{
   return VG_(sysnum_string)(sysnum, n_buf, buf);
}

//---------------------------------------------------------------------------
#else
//---------------------------------------------------------------------------
#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
