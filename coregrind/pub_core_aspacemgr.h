
/*--------------------------------------------------------------------*/
/*--- The address space manager.              pub_core_aspacemgr.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
   suggested end address (highest addressable byte) for the client's stack. */
extern Addr VG_(am_startup) ( Addr sp_at_startup );

/* Check whether ADDR is OK to be used as aspacem_minAddr. If not, *ERRMSG
   will be set to identify what's wrong. ERRMSG may be NULL. */
extern Bool VG_(am_is_valid_for_aspacem_minAddr)( Addr addr,
                                                  const HChar **errmsg );

//--------------------------------------------------------------
// Querying current status


/* Finds an anonymous segment containing 'a'. Returned pointer is read only. */
extern NSegment const *VG_(am_find_anon_segment) ( Addr a );

/* Find the next segment along from 'here', if it is a file/anon/resvn
   segment. */
extern NSegment const* VG_(am_next_nsegment) ( const NSegment* here,
                                               Bool fwds );

/* Is the area [start .. start+len-1] validly accessible by
   valgrind with at least the permissions 'prot' ?  To find out
   simply if said area merely belongs to valgrind, pass 
   VKI_PROT_NONE as 'prot'.  Will return False if any part of the
   area does not belong to valgrind or does not have at least
   the stated permissions. */
extern Bool VG_(am_is_valid_for_valgrind)
   ( Addr start, SizeT len, UInt prot );

/* Variant of VG_(am_is_valid_for_client) which allows free areas to
   be consider part of the client's addressable space.  It also
   considers reservations to be allowable, since from the client's
   point of view they don't exist. */
extern Bool VG_(am_is_valid_for_client_or_free_or_resvn)
   ( Addr start, SizeT len, UInt prot );

/* Checks if a piece of memory consists of either free or reservation
   segments. */
extern Bool VG_(am_is_free_or_resvn)( Addr start, SizeT len );

/* Check whether ADDR looks like an address or address-to-be located in an
   extensible client stack segment. */
extern Bool VG_(am_addr_is_in_extensible_client_stack)( Addr addr );

/* Trivial fn: return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
extern ULong VG_(am_get_anonsize_total)( void );

/* Show the segment array on the debug log, at given loglevel. */
extern void VG_(am_show_nsegments) ( Int logLevel, const HChar* who );

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
      /* Note: if rkind == MAlign then start specifies alignment. This is
         Solaris specific. */
      enum { MFixed, MHint, MAny, MAlign } rkind;
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
   ( const MapRequest* req, Bool forClient, /*OUT*/Bool* ok );

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
   VG_(am_notify_mprotect), we merely record the given info, and don't
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
extern SysRes VG_(am_mmap_file_fixed_client_flags)
   ( Addr start, SizeT length, UInt prot, UInt flags, Int fd, Off64T offset );
extern SysRes VG_(am_mmap_named_file_fixed_client)
   ( Addr start, SizeT length, UInt prot, Int fd,
     Off64T offset, const HChar *name );
extern SysRes VG_(am_mmap_named_file_fixed_client_flags)
   ( Addr start, SizeT length, UInt prot, UInt flags, Int fd,
     Off64T offset, const HChar *name );

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

/* Similar to VG_(am_mmap_anon_float_client) but also
   marks the segment as containing the client heap. */
extern SysRes VG_(am_mmap_client_heap) ( SizeT length, Int prot );

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

/* Set the 'hasT' bit on the segment containing ADDR indicating that
   translations have or may have been taken from this segment. ADDR is
   expected to belong to a client segment. */
extern void VG_(am_set_segment_hasT)( Addr addr );

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

/* ADDR is the start address of an anonymous client mapping.  This fn extends
   the mapping by DELTA bytes, taking the space from a reservation section
   which must be adjacent.  If DELTA is positive, the segment is
   extended forwards in the address space, and the reservation must be
   the next one along.  If DELTA is negative, the segment is extended
   backwards in the address space and the reservation must be the
   previous one.  DELTA must be page aligned.  abs(DELTA) must not
   exceed the size of the reservation segment minus one page, that is,
   the reservation segment after the operation must be at least one
   page long. The function returns a pointer to the resized segment. */
extern const NSegment *VG_(am_extend_into_adjacent_reservation_client) 
   ( Addr addr, SSizeT delta, /*OUT*/Bool *overflow );

/* --- --- --- resizing/move a mapping --- --- --- */

/* This function grows a client mapping in place into an adjacent free segment.
   ADDR is the client mapping's start address and DELTA, which must be page
   aligned, is the growth amount. The function returns a pointer to the
   resized segment. The function is used in support of mremap. */
extern const NSegment *VG_(am_extend_map_client)( Addr addr, SizeT delta );

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

// VG_DEFAULT_STACK_ACTIVE_SZB is the default size of a Valgrind stack.
// The effectively used size is controlled by the command line options
// --valgrind-stack-size=xxxx (which must be page aligned).
// Note that m_main.c needs an interim stack (just to startup), before
// any command line option can be processed. This interim stack
// (declared in m_main.c) will use the size VG_DEFAULT_STACK_ACTIVE_SZB.
#if defined(VGP_ppc32_linux) \
    || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)	\
    || defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
    || defined(VGP_arm64_linux) || defined(VGP_nanomips_linux)
# define VG_STACK_GUARD_SZB  65536  // 1 or 16 pages
#else
# define VG_STACK_GUARD_SZB  8192   // 2 pages
#endif
# define VG_DEFAULT_STACK_ACTIVE_SZB 1048576 // (4096 * 256) = 1Mb 

typedef struct _VgStack VgStack;


/* Allocate and initialise a VgStack (anonymous valgrind space).
   Protect the stack active area and the guard areas appropriately.
   Returns NULL on failure, else the address of the bottom of the
   stack.  On success, also sets *initial_sp to what the stack pointer
   should be set to. */

extern VgStack* VG_(am_alloc_VgStack)( /*OUT*/Addr* initial_sp );

/* Figure out how many bytes of the stack's active area have not been
   used.  Used for estimating if we are close to overflowing it.  If
   the free area is larger than 'limit', just return 'limit'. */
extern SizeT VG_(am_get_VgStack_unused_szB)( const VgStack* stack,
                                             SizeT limit ); 

/* Returns the Addr of the lowest usable byte of stack. */
extern Addr VG_(am_valgrind_stack_low_addr)( const VgStack* stack);

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

#if defined(VGO_solaris)
extern Bool VG_(am_search_for_new_segment)(Addr *start, SizeT *size,
                                           UInt *prot);
#endif

#if defined(VGO_freebsd)
/* For kern.usrstack syscall on FreeBSD */
extern Word VG_(get_usrstack)(void);
#endif

#endif   // __PUB_CORE_ASPACEMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
