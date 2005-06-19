
##--------------------------------------------------------------------##
##--- Support for doing system calls.          arm-linux/syscall.S ---##
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

#include "pub_tool_basics_asm.h"
#include "vki_unistd.h"

# XXX: must reinstate comments also -- see x86-linux/syscall.S

# XXX: this is now all way out of date...

.globl	VG_(do_syscall)
VG_(do_syscall):
	swi

.globl VG_(clone)
VG_(clone):
	swi

# ToDo XXX: Assembler did not like this...
#/* Let the linker know we don't need an executable stack */
#.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
