
/*--------------------------------------------------------------------*/
/*--- Profiling machinery -- not for release builds!               ---*/
/*---                                                 vg_profile.c ---*/
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"

#ifdef VG_PROFILE

/* get rid of these, if possible */
#include <signal.h>
#include <sys/time.h>

#define VGP_PAIR(enumname,str) str
static const Char* vgp_names[VGP_M_CCS] = { VGP_LIST };
#undef VGP_PAIR

static Int   vgp_nticks;
static Int   vgp_counts[VGP_M_CCS];
static Int   vgp_entries[VGP_M_CCS];

static Int   vgp_sp;
static VgpCC vgp_stack[VGP_M_STACK];

void VGP_(tick) ( int sigNo )
{
   Int cc;
   vgp_nticks++;
   cc = vgp_stack[vgp_sp];
   vg_assert(cc >= 0 && cc < VGP_M_CCS);
   vgp_counts[ cc ]++;
}

void VGP_(init_profiling) ( void )
{
   struct itimerval value;
   Int i, ret;

   for (i = 0; i < VGP_M_CCS; i++)
     vgp_counts[i] = vgp_entries[i] = 0;

   vgp_nticks = 0;
   vgp_sp = -1;
   VGP_(pushcc) ( VgpRun );

   value.it_interval.tv_sec  = 0;
   value.it_interval.tv_usec = 10 * 1000;
   value.it_value = value.it_interval;

   signal(SIGPROF, VGP_(tick) );
   ret = setitimer(ITIMER_PROF, &value, NULL);
   if (ret != 0) VG_(panic)("vgp_init_profiling");
}

void VGP_(done_profiling) ( void )
{
   Int i;
   VG_(printf)("Profiling done, %d ticks\n", vgp_nticks);
   for (i = 0; i < VGP_M_CCS; i++)
      VG_(printf)("%2d: %4d (%3d %%%%) ticks,  %8d entries   for  %s\n",
                  i, vgp_counts[i], 
                  (Int)(1000.0 * (double)vgp_counts[i] / (double)vgp_nticks),
                  vgp_entries[i],
                  vgp_names[i] );
}

void VGP_(pushcc) ( VgpCC cc )
{
   if (vgp_sp >= VGP_M_STACK-1) VG_(panic)("vgp_pushcc");
   vgp_sp++;
   vgp_stack[vgp_sp] = cc;
   vgp_entries[ cc ] ++;
}

void VGP_(popcc) ( void )
{
   if (vgp_sp <= 0) VG_(panic)("vgp_popcc");
   vgp_sp--;
}

#endif /* VG_PROFILE */

/*--------------------------------------------------------------------*/
/*--- end                                             vg_profile.c ---*/
/*--------------------------------------------------------------------*/
