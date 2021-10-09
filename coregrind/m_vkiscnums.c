
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vkiscnums.                   ---*/
/*---                                                m_vkiscnums.c ---*/
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

#include "pub_core_basics.h"
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

const HChar* VG_(sysnum_string)(Word sysnum)
{
   static HChar buf[20+1];   // large enough

   VG_(sprintf)(buf, "%ld", sysnum);
   return buf;
}

/* include/pub_tool_basics.h hardcodes the following syscall numbers
   on mips{32,64}-linux so as to avoid a module cycle.  We make that
   safe here by causing the build to fail if those numbers should ever
   change.  See comments in function sr_EQ in the mips{32,64}-linux
   section of include/pub_tool_basics.h for more details. */
#if defined(VGP_mips32_linux)
STATIC_ASSERT(__NR_pipe  == 4042);
STATIC_ASSERT(__NR_pipe2 == 4328);
#elif defined(VGP_mips64_linux) && defined(VGABI_N32)
STATIC_ASSERT(__NR_pipe  == 6021);
STATIC_ASSERT(__NR_pipe2 == 6291);
#elif defined(VGP_mips64_linux)
STATIC_ASSERT(__NR_pipe  == 5021);
STATIC_ASSERT(__NR_pipe2 == 5287);
#endif

//---------------------------------------------------------------------------
#elif defined(VGO_freebsd)
//---------------------------------------------------------------------------

const HChar* VG_(sysnum_string)(Word sysnum)
{
   static HChar buf[20+1];   // large enough

   VG_(snprintf)(buf, sizeof(buf), "%3ld", sysnum);
   return buf;
}

#elif defined(VGO_darwin)
//---------------------------------------------------------------------------

const HChar* VG_(sysnum_string)(Word sysnum)
{
   static HChar buf[7+1+20+1];   // large enough

   const HChar* classname = NULL;
   switch (VG_DARWIN_SYSNO_CLASS(sysnum)) {
      case VG_DARWIN_SYSCALL_CLASS_MACH: classname = "mach"; break;
      case VG_DARWIN_SYSCALL_CLASS_UNIX: classname = "unix"; break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP: classname = "mdep"; break;
      case VG_DARWIN_SYSCALL_CLASS_DIAG: classname = "diag"; break;
      default: classname = "UNKNOWN"; break;
   }
   VG_(sprintf)(buf, "%s:%ld", classname, VG_DARWIN_SYSNO_INDEX(sysnum));
   return buf;
}

//---------------------------------------------------------------------------
#elif defined(VGO_solaris)
//---------------------------------------------------------------------------

const HChar *VG_(sysnum_string)(Word sysnum)
{
   static HChar buf[8+20+1];   // large enough

   const HChar* classname = NULL;
   switch (VG_SOLARIS_SYSNO_CLASS(sysnum)) {
      case VG_SOLARIS_SYSCALL_CLASS_CLASSIC: classname = ""; break;
      case VG_SOLARIS_SYSCALL_CLASS_FASTTRAP: classname = "fast:"; break;
      default: classname = "UNKNOWN:"; break;
   }
   VG_(sprintf)(buf, "%s%ld", classname, VG_SOLARIS_SYSNO_INDEX(sysnum));
   return buf;
}

//---------------------------------------------------------------------------
#else
//---------------------------------------------------------------------------
#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
