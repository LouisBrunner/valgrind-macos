
/*--------------------------------------------------------------------*/
/*--- Basic definitions for all of Helgrind.                       ---*/
/*---                                                  hg_basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2008 OpenWorks Ltd
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __HG_BASICS_H
#define __HG_BASICS_H


/*----------------------------------------------------------------*/
/*--- Very basic stuff                                         ---*/
/*----------------------------------------------------------------*/

#define HG_(str) VGAPPEND(vgHelgrind_,str)

void* HG_(zalloc) ( HChar* cc, SizeT n );
void  HG_(free)   ( void* p );
Char* HG_(strdup) ( HChar* cc, const Char* s );

static inline Bool HG_(is_sane_ThreadId) ( ThreadId coretid ) {
   return coretid >= 0 && coretid < VG_N_THREADS;
}


/*----------------------------------------------------------------*/
/*--- Command line options                                     ---*/
/*----------------------------------------------------------------*/

/* Flags for controlling for which events sanity checking is done */
#define SCE_THREADS  (1<<0)  // Sanity check at thread create/join
#define SCE_LOCKS    (1<<1)  // Sanity check at lock events
#define SCE_BIGRANGE (1<<2)  // Sanity check at big mem range events
#define SCE_ACCESS   (1<<3)  // Sanity check at mem accesses
#define SCE_LAOG     (1<<4)  // Sanity check at significant LAOG events

#define SCE_BIGRANGE_T 256  // big mem range minimum size


/* Enable/disable lock order checking.  Sometimes it produces a lot of
   errors, possibly genuine, which nevertheless can be very
   annoying. */
extern Bool HG_(clo_track_lockorders);

/* When comparing race errors for equality, should the race address be
   taken into account?  For users, no, but for verification purposes
   (regtesting) this is sometimes important. */
extern Bool HG_(clo_cmp_race_err_addrs);

/* Show conflicting accesses?  This involves collecting and storing
   large numbers of call stacks just in case we might need to show
   them later, and so is expensive (although very useful).  Hence
   allow it to be optionally disabled. */
extern Bool HG_(clo_show_conflicts);

/* Size of the conflicting-access cache, measured in terms of
   maximum possible number of elements in the previous-access map.
   Must be between 10k amd 10 million.  Default is 1 million. */
extern UWord HG_(clo_conflict_cache_size);

/* Sanity check level.  This is an or-ing of
   SCE_{THREADS,LOCKS,BIGRANGE,ACCESS,LAOG}. */
extern Word HG_(clo_sanity_flags);




#endif /* ! __HG_BASICS_H */

/*--------------------------------------------------------------------*/
/*--- end                                              hg_basics.h ---*/
/*--------------------------------------------------------------------*/
