
/*--------------------------------------------------------------------*/
/*---                                               mc_constants.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors.

   Copyright (C) 2000-2003 Julian Seward 
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

#ifndef __MC_CONSTANTS_H
#define __MC_CONSTANTS_H

#include "vg_constants_skin.h"

#define MC_(str)    VGAPPEND(vgMemCheck_,str)

#endif   /* __MC_CONSTANTS_H */

/*--------------------------------------------------------------------*/
/*--- end                                           mc_constants.h ---*/
/*--------------------------------------------------------------------*/

