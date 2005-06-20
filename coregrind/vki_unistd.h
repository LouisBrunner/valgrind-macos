
/*--------------------------------------------------------------------*/
/*--- Generic header for Valgrind's kernel interface.              ---*/
/*---                                                 vki_unistd.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __VKI_UNISTD_H
#define __VKI_UNISTD_H

#if defined(VGP_x86_linux)
#  include "vki_unistd-x86-linux.h"   
#elif defined(VGP_amd64_linux)
#  include "vki_unistd-amd64-linux.h" 
#elif defined(VGP_arm_linux)
#  include "vki_unistd-arm-linux.h" 
#elif defined(VGP_ppc32_linux)
#  include "vki_unistd-ppc32-linux.h" 
#else
#  error Unknown platform
#endif

#endif   // __VKI_UNISTD_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
