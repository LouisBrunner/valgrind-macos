/* Target operations for the remote server for GDB.
   Copyright (C) 2002, 2004, 2005, 2011
   Free Software Foundation, Inc.

   Contributed by MontaVista Software.

   This file is part of GDB.
   It has been modified to integrate it in valgrind

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "server.h"

struct target_ops *the_target;

void set_desired_inferior (int use_general)
{
  struct thread_info *found;

  if (use_general == 1) {
     found = (struct thread_info *) find_inferior_id (&all_threads,
                                                      general_thread);
  } else {
     found = NULL;

     /* If we are continuing any (all) thread(s), use step_thread
        to decide which thread to step and/or send the specified
        signal to.  */
     if ((step_thread != 0 && step_thread != -1)
         && (cont_thread == 0 || cont_thread == -1))
	found = (struct thread_info *) find_inferior_id (&all_threads,
							 step_thread);

     if (found == NULL)
	found = (struct thread_info *) find_inferior_id (&all_threads,
							 cont_thread);
  }

  if (found == NULL)
     current_inferior = (struct thread_info *) all_threads.head;
  else
     current_inferior = found;
  {
     ThreadState *tst = (ThreadState *) inferior_target_data (current_inferior);
     ThreadId tid = tst->tid;
     dlog(1, "set_desired_inferior use_general %d found %p tid %d lwpid %d\n",
          use_general, found, tid, tst->os_state.lwpid);
  }
}

int read_inferior_memory (CORE_ADDR memaddr, unsigned char *myaddr, int len)
{
   int res;
   res = (*the_target->read_memory) (memaddr, myaddr, len);
   return res;
}

int write_inferior_memory (CORE_ADDR memaddr, const unsigned char *myaddr,
                           int len)
{
   /* Lacking cleanups, there is some potential for a memory leak if the
      write fails and we go through error().  Make sure that no more than
      one buffer is ever pending by making BUFFER static.  */
   static unsigned char *buffer = 0;
   int res;

   if (buffer != NULL)
      free (buffer);

   buffer = malloc (len);
   VG_(memcpy) (buffer, myaddr, len);
   res = (*the_target->write_memory) (memaddr, buffer, len);
   free (buffer);
   buffer = NULL;

   return res;
}

void set_target_ops (struct target_ops *target)
{
   the_target = (struct target_ops *) malloc (sizeof (*the_target));
   VG_(memcpy) (the_target, target, sizeof (*the_target));
}

void* VG_(dmemcpy) ( void *d, const void *s, SizeT sz, Bool *mod )
{
   if (VG_(memcmp) (d, s, sz)) {
      *mod = True;
      return VG_(memcpy) (d, s, sz);
   } else {
      *mod = False;
      return d;
   }
}

void VG_(transfer) (void *valgrind,
                    void *gdbserver,
                    transfer_direction dir,
                    SizeT sz,
                    Bool *mod)
{
   if (dir == valgrind_to_gdbserver)
      VG_(dmemcpy) (gdbserver, valgrind, sz, mod);
   else if (dir == gdbserver_to_valgrind)
      VG_(dmemcpy) (valgrind, gdbserver, sz, mod);
   else
      vg_assert (0);
}
