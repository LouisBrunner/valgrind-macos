
/*--------------------------------------------------------------------*/
/*--- Used to make a dummy valgrinq.so, which does nothing at all. ---*/
/*---                                          vg_valgrinq_dummy.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

/* For the rationale behind this file, look at
   VG_(mash_LD_PRELOAD_string) in vg_main.c. */

/* Remember not to use a variable of this name in any program you want
   to debug :-) */
int dont_mess_with_the_RSCDS = 0;

/* If you are bored, perhaps have a look at http://www.rscds.org. */

/*--------------------------------------------------------------------*/
/*--- end                                      vg_valgrinq_dummy.c ---*/
/*--------------------------------------------------------------------*/
