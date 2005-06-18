
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                               arm/dispatch.S ---##
##--------------------------------------------------------------------##

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

#include "pub_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"

.globl VG_(run_innerloop)
VG_(run_innerloop):
    # this needs some improvement...
    swi

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
