
/*--------------------------------------------------------------------*/
/*--- L2 cache simulation.                                         ---*/
/*---                                                  cg_sim_L2.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "cg_sim_gen.c"

CACHESIM(L2, (*m2)++ );

/*--------------------------------------------------------------------*/
/*--- end                                              cg_sim_L2.c ---*/
/*--------------------------------------------------------------------*/

