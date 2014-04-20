
/*--------------------------------------------------------------------*/
/*--- The address space manager.              pub_core_aspacemgr.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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

#ifndef __PUB_CORE_ASPACEMGR_H
#define __PUB_CORE_ASPACEMGR_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with management of the entire process
// address space.  Almost everything depends upon it, including dynamic
// memory management.  Hence this module is almost completely
// standalone; the only module it uses is m_debuglog.  DO NOT CHANGE
// THIS.
//--------------------------------------------------------------------

#include "pub_tool_aspacemgr.h"

//--------------------------------------------------------------
// Definition of address-space segments

/* types SegKind, ShrinkMode and NSegment are described in
   the tool-visible header file, not here. */


//--------------------------------------------------------------
// Initialisation

/* Initialise the address space manager, setting up the initial
   segment list, and reading /proc/self/maps into it.  This must
   be called before any other function.

   Takes a pointer to the SP at the time V gained control.  This is
   taken to be the highest usable address (more or less).  Based on
   that (and general consultation of tea leaves, etc) return a
   suggested end address for the client's stack. */
extern Addr VG_(am_startup) ( Addr sp_at_startup );


//--------------------------------------------------------------
// Querying current status

/* Finds the segment containing 'a'.  Only returns file/anon/resvn
   segments.  This returns a 'NSegment const *' - a pointer to
   readonly data. */
// Is in tool-visible header file.
// extern NSegment const * VG_(am_find_nsegment) ( Addr a );

/* Find the next segment along from 'here', if it is a file/anon/resvn
   segment. */
extern NSegment const* VG_(am_next_nsegment) ( const NSegment* here,
                                               Bool fwds );

/* Is the area [start .. start+len-1] validly accessible by the 
   client with at least the permissions 'prot' ?  To find out
   simply if said area merely belongs to the client, pass 
   VKI_PROT_NONE as 'prot'.  Will return False if any part of the
   area does not belong to the client or does not have at least
   the stated permissions. */
// Is in tool-visible header file.
// extern Bool VG_(am_is_valid_for_client)
//   ( Addr start, SizeT len, UInt prot );

/* Same as VG_(am_is_valid_for_client) but for valgrind :
   test if memory is addressable by valgrind with at least
   the protection 'prot'. */
extern Bool VG_(am_is_valid_for_valgrind)
   ( Addr start, SizeT len, UInt prot );

/* Variant of VG_(am_is_valid_for_client) which allows free areas to
   be consider part of the client's addressable space.  It also
   considers reservations to be allowable, since from the client's
   point of view they don't exist. */
extern Bool VG_(am_is_valid_for_client_or_free_or_resvn)
   ( Addr start, SizeT len, UInt prot );

/* Trivial fn: return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
extern ULong VG_(am_get_anonsize_total)( void );

/* Show the segment array on the debug log, at given loglevel. */
extern void VG_(am_show_nsegments) ( Int logLevel, const HChar* who );

/* Get the filename corresponding to this segment, if known and if it
   has one.  The returned name's storage cannot be assumed to be
   persistent, so the caller should immediately copy the name
   elsewhere.  This may return NULL if the file name is not known or
   for arbitrary other implementation-dependent reasons, so callers
   need to be able to handle a NULL return value. */
// Is in tool-visible header file.
// extern HChar* VG_(am_get_filename)( NSegment* );

/* VG_(am_get_segment_starts) is also part of this section, but its
   prototype is tool-visible, hence not in this header file. */

/* Sanity check: check that Valgrind and the kernel agree on the
   address space layout.  Prints offending segments and call point if
   a discrepancy is detected, but does not abort the system.  Returned
   Bool is False if a discrepancy was found. */

extern Bool VG_(am_do_sync_check) ( const HChar* fn, 
                                    const HChar* file, Int line );

//--------------------------------------------------------------
// Functions pertaining to the central query-notify mechanism
// used to handle mmap/munmap/mprotect resulting from client
// syscalls.

/* Describes a request for VG_(am_get_advisory). */
typedef
   struct {
      enum { MFixed, MHint, MAny } rkind;
      Addr start;
      Addr len;
   }
   MapRequest;

/* Query aspacem to ask where a mapping should go.  On success, the
   advised placement is returned, and *ok is set to True.  On failure,
   zero is returned and *ok is set to False.  Note that *ok must be
   consulted by the caller to establish success or failure; that
   cannot be established reliably from the returned value.  If *ok is
   set to False, it means aspacem has vetoed the mapping, and so the
   caller should not proceed with it. */
extern Addr VG_(am_get_advisory)
   ( MapRequest* req, Bool forClient, /*OUT*/Bool* ok );

/* Convenience wrapper for VG_(am_get_advisory) for client floating or
   fixed requests.  If start is zero, a floating request is issued; if
   nonzero, a fixed request at that address is issued.  Same comments
   about return values apply. */
extern Addr VG_(am_get_advisory_client_simple) 
   ( Addr start, SizeT len, /*OUT*/Bool* ok );

/* Returns True if [start, start + len - 1] is covered by a single
   free segment, otherwise returns False.
   This allows to check the following case:
   VG_(am_get_advisory_client_simple) (first arg == 0, meaning
   this-or-nothing) is too lenient, and may allow us to trash
   the next segment along.  So make very sure that the proposed
   new area really is free.  This is perhaps overly
   conservative, but it fixes #129866. */
extern Bool VG_(am_covered_by_single_free_segment)
   ( Addr start, SizeT len);

/* Notifies aspacem that the client completed an mmap successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */
extern Bool VG_(am_notify_client_mmap)
   ( Addr a, SizeT len, UInt prot, UInt flags, Int fd, Off64T offset );

/* Notifies aspacem that the client completed a shmat successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */
extern Bool VG_(am_notify_client_shmat)( Addr a, SizeT len, UInt prot );

/* Notifies aspacem that an mprotect was completed successfully.  The
   segment array is updated accordingly.  Note, as with
   VG_(am_notify_munmap), it is not the job of this function to reject
   stupid mprotects, for example the client doing mprotect of
   non-client areas.  Such requests should be intercepted earlier, by
   the syscall wrapper for mprotect.  This function merely records
   whatever it is told.  If the returned Bool is True, the caller
   should immediately discard translations from the specified address
   range. */
extern Bool VG_(am_notify_mprotect)( Addr start, SizeT len, UInt prot );

/* Notifies aspacem that an munmap completed successfully.  The
   segment array is updated accordingly.  As with
   VG_(am_notify_munmap), we merely record the given info, and don't
   check it for sensibleness.  If the returned Bool is True, the
   caller should immediately discard translations from the specified
   address range. */
extern Bool VG_(am_notify_munmap)( Addr start, SizeT len );

/* Hand a raw mmap to the kernel, without aspacem updating the segment
   array.  THIS FUNCTION IS DANGEROUS -- it will cause aspacem's view
   of the address space to diverge from that of the kernel.  DO NOT
   USE IT UNLESS YOU UNDERSTAND the request-notify model used by
   aspacem.  In short, DO NOT USE THIS FUNCTION. */
extern SysRes VG_(am_do_mmap_NO_NOTIFY)
   ( Addr start, SizeT length, UInt prot, UInt flags, Int fd, Off64T offset);


//--------------------------------------------------------------
// Dealing with mappings which do not arise directly from the
// simulation of the client.  These are typically used for
// loading the client and building its stack/data segment, before
// execution begins.  Also for V's own administrative use.

/* --- --- --- map, unmap, protect  --- --- --- */

/* Map a file at a fixed address for the client, and update the
   segment array accordingly. */
extern SysRes VG_(am_mmap_file_fixed_client)
   ( Addr start, SizeT length, UInt prot, Int fd, Off64T offset );
extern SysRes VG_(am_mmap_named_file_fixed_client)
   ( Addr start, SizeT length, UInt prot, Int fd, Off64T offset, const HChar *name );

/* Map anonymously at a fixed address for the client, and update
   the segment array accordingly. */
extern SysRes VG_(am_mmap_anon_fixed_client)
   ( Addr start, SizeT length, UInt prot );


/* Map anonymously at an unconstrained address for the client, and
   update the segment array accordingly.  */
extern SysRes VG_(am_mmap_anon_float_client) ( SizeT length, Int prot );

/* Map anonymously at an unconstrained address for V, and update the
   segment array accordingly.  This is fundamentally how V allocates
   itself more address space when needed. */
extern SysRes VG_(am_mmap_anon_float_valgrind)( SizeT cszB );

/* Map privately a file at an unconstrained address for V, and update the
   segment array accordingly.  This is used by V for transiently
   mapping in object files to read their debug info.  */
extern SysRes VG_(am_mmap_file_float_valgrind)
   ( SizeT length, UInt prot, Int fd, Off64T offset );

/* Map shared a file at an unconstrained address for V, and update the
   segment array accordingly.  This is used by V for communicating
   with vgdb.  */
extern SysRes VG_(am_shared_mmap_file_float_valgrind)
   ( SizeT length, UInt prot, Int fd, Off64T offset );

/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for the client.
   If *need_discard is True after a successful return, the caller
   should immediately discard translations from the specified address
   range. */
extern SysRes VG_(am_munmap_client)( /*OUT*/Bool* need_discard,
                                     Addr start, SizeT length );

/* Let (start,len) denote an area within a single Valgrind-owned
  segment (anon or file).  Change the ownership of [start, start+len)
  to the client instead.  Fails if (start,len) does not denote a
  suitable segment. */
extern Bool VG_(am_change_ownership_v_to_c)( Addr start, SizeT len );

/* 'seg' must be NULL or have been obtained from
   VG_(am_find_nsegment), and still valid.  If non-NULL, and if it
   denotes a SkAnonC (anonymous client mapping) area, set the .isCH
   (is-client-heap) flag for that area.  Otherwise do nothing.
   (Bizarre interface so that the same code works for both Linux and
   AIX and does not impose inefficiencies on the Linux version.) */
extern void VG_(am_set_segment_isCH_if_SkAnonC)( const NSegment* seg );

/* Same idea as VG_(am_set_segment_isCH_if_SkAnonC), except set the
   segment's hasT bit (has-cached-code) if this is SkFileC or SkAnonC
   segment. */
extern void VG_(am_set_segment_hasT_if_SkFileC_or_SkAnonC)( const NSegment* );

/* --- --- --- reservations --- --- --- */

/* Create a reservation from START .. START+LENGTH-1, with the given
   ShrinkMode.  When checking whether the reservation can be created,
   also ensure that at least abs(EXTRA) extra free bytes will remain
   above (> 0) or below (< 0) the reservation.

   The reservation will only be created if it, plus the extra-zone,
   falls entirely within a single free segment.  The returned Bool
   indicates whether the creation succeeded. */
extern Bool VG_(am_create_reservation) 
   ( Addr start, SizeT length, ShrinkMode smode, SSizeT extra );

/* Let SEG be an anonymous client mapping.  This fn extends the
   mapping by DELTA bytes, taking the space from a reservation section
   which must be adjacent.  If DELTA is positive, the segment is
   extended forwards in the address space, and the reservation must be
   the next one along.  If DELTA is negative, the segment is extended
   backwards in the address space and the reservation must be the
   previous one.  DELTA must be page aligned.  abs(DELTA) must not
   exceed the size of the reservation segment minus one page, that is,
   the reservation segment after the operation must be at least one
   page long. */
extern Bool VG_(am_extend_into_adjacent_reservation_client) 
   ( const NSegment* seg, SSizeT delta );

/* --- --- --- resizing/move a mapping --- --- --- */

/* Let SEG be a client mapping (anonymous or file).  This fn extends
   the mapping forwards only by DELTA bytes, and trashes whatever was
   in the new area.  Fails if SEG is not a single client mapping or if
   the new area is not accessible to the client.  Fails if DELTA is
   not page aligned.  *seg is invalid after a successful return.  If
   *need_discard is True after a successful return, the caller should
   immediately discard translations from the new area. */
extern Bool VG_(am_extend_map_client)( /*OUT*/Bool* need_discard,
                                       const NSegment* seg, SizeT delta );

/* Remap the old address range to the new address range.  Fails if any
   parameter is not page aligned, if the either size is zero, if any
   wraparound is implied, if the old address range does not fall
   entirely within a single segment, if the new address range overlaps
   with the old one, or if the old address range is not a valid client
   mapping.  If *need_discard is True after a successful return, the
   caller should immediately discard translations from both specified
   address ranges.  */
extern Bool VG_(am_relocate_nooverlap_client)( /*OUT*/Bool* need_discard,
                                               Addr old_addr, SizeT old_len,
                                               Addr new_addr, SizeT new_len );

//--------------------------------------------------------------
// Valgrind (non-client) thread stacks.  V itself runs on such
// stacks.  The address space manager provides and suitably
// protects such stacks.

#if defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux) \
    || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
    || defined(VGP_arm64_linux)
# define VG_STACK_GUARD_SZB  65536  // 1 or 16 pages
# define VG_STACK_ACTIVE_SZB (4096 * 256) // 1Mb
#else
# define VG_STACK_GUARD_SZB  8192   // 2 pages
# define VG_STACK_ACTIVE_SZB (4096 * 256) // 1Mb
#endif

typedef
   struct {
      HChar bytes[VG_STACK_GUARD_SZB 
                  + VG_STACK_ACTIVE_SZB 
                  + VG_STACK_GUARD_SZB];
   }
   VgStack;


/* Allocate and initialise a VgStack (anonymous valgrind space).
   Protect the stack active area and the guard areas appropriately.
   Returns NULL on failure, else the address of the bottom of the
   stack.  On success, also sets *initial_sp to what the stack pointer
   should be set to. */

extern VgStack* VG_(am_alloc_VgStack)( /*OUT*/Addr* initial_sp );

/* Figure out how many bytes of the stack's active area have not been
   used.  Used for estimating if we are close to overflowing it.  If
   the free area is larger than 'limit', just return 'limit'. */
extern SizeT VG_(am_get_VgStack_unused_szB)( VgStack* stack, SizeT limit ); 

// DDD: this is ugly
#if defined(VGO_darwin)
typedef 
   struct {
      Bool   is_added;  // Added or removed seg?
      Addr   start;
      SizeT  end;
      UInt   prot;      // Not used for removed segs.
      Off64T offset;    // Not used for removed segs.
   }
   ChangedSeg;

extern Bool VG_(get_changed_segments)(
      const HChar* when, const HChar* where, /*OUT*/ChangedSeg* css,
      Int css_size, /*OUT*/Int* css_used);
#endif

#endif   // __PUB_CORE_ASPACEMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
