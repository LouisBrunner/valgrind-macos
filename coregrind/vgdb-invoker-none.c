/*--------------------------------------------------------------------*/
/*--- Empty implementation of vgdb invoker subsystem.              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2013 Philippe Waroquiers

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

#include "vgdb.h"

#include <stdio.h>
#include <sys/time.h>

void invoker_restrictions_msg(void)
{
   fprintf(stderr, 
           "Note: vgdb invoker not implemented on this platform.\n"
           "For more info: read user manual section"
           " 'Limitations of the Valgrind gdbserver'.\n");
}

void invoker_cleanup_restore_and_detach(void *v_pid)
{
   DEBUG(1, "invoker_cleanup_restore_and_detach");
}

Bool invoker_invoke_gdbserver(pid_t pid)
{
   DEBUG(2, "invoker_invoke_gdbserver not implemented\n");
   /* Returning True signals to not retry (too soon) to invoke. */
   return True;
}

void invoker_valgrind_dying(void)
{
}
