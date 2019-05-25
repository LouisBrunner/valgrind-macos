/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Segment name management                 aspacemgr-segnames.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2015-2017  Florian Krohm

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

/* Segment names are stored in a string table.

   The string table is organised into slots of varying length. Slots are
   adjacent and there are no holes between slots.
   A slot consists of two parts:

   (1) a fixed size overhead of length 4 bytes
   (2) a variable size payload of up to 65535 bytes

   The segment name is stored in the payload area. Therefore:
   a segment name cannot be longer than 65535 bytes including the '\0'
   terminator. This looks like a reasonable limitation.

   Overall slot layout:

       |          4 bytes            |    max 65535 bytes      |
       +-----------------------------+-------------------------+
       |          overhead           |         payload         |
       +-----------------------------+-------------------------+
       ^                             ^
       |                             |
      -4                             +----- seg->fnIdx

   Each slot is uniquely identified by an index which points to the first
   byte of the payload area. It is this value that is stored in seg->fnIdx.
   Note, that this value is at least 4.

   A slot either holds a string or it is free. The status of a slot is
   identified by the leftmost bit in the overhead field, the so called F-bit.
   F-bit == 1 means that slot is free; otherwise it is occupied and holds a
   string.

   Slot containing a string (segment name):

  bits | 1 |      15      |    16    |
       +---+--------------+----------+-------------------------+
       | 0 |   refcount   | slotsize | the string including \0 |
       +---+--------------+----------+-------------------------+
       ^                  ^          ^
       |                  |          |
      -4                 -2          +----- seg->fnIdx

   Segment names are reference counted. 15 bits are available which allows
   for up to 32767 references. If the string is referenced more than 32767
   times, the reference count will be frozen and the slot can never
   become free. I'm not unduly concerned.
   Two bytes are reserved to hold the size of the slot. Well, it's actually
   the size of the payload aread (i.e. the size of the slot minus the
   overhead). Ah well -- the name sticks.
   With two bytes to store the size, the payload area can be at most 65535
   bytes large.

   A free slot looks like this:

  bits | 1 |           31            |    16    |
       +---+-------------------------+----------+--------------+
       | 1 | index of next free slot | slotsize | .. unused .. |
       +---+-------------------------+----------+--------------+
       ^                             ^
       |                             |
      -4                             +----- seg->fnIdx

   Free slots are chained together in a singly linked list. An index of
   zero indicates the end of the chain. Note that zero cannot conflict
   with an index into the string table as the minimum index is at least
   four (see above).

   The typical way to traverse the segment names is:

   for (ix = overhead; (size = get_slotsize(ix)) != 0; ix += size + overhead) {
      if (is_freeslot(ix))
         do this
      else
         do that
   }

   Important detail: there is a sentinel at the end of the list, namely a
   slot with a zero-sized payload area.

   Whenever a new segment name needs to be stashed away, the list of free
   slots is traversed and the first slot which is large enough is being taken
   (first fit). There will be no splitting of slots, as that complicates
   matters and without slot coalescing would lead to memory fragmentation.
   So we leave it as is until a use case comes up that needs something better.
*/

#include "pub_core_basics.h"     // types
#include "priv_aspacemgr.h"

// A few constants.
enum {
   refcount_size = sizeof(UShort),
   slotsize_size = sizeof(UShort),
   overhead = refcount_size + slotsize_size,
   max_refcount  = 0x7fff,      // 2 bytes - F-bit
   max_slotsize  = 0xffff,      // 2 bytes
   max_slotindex = 0x7fffffff,  // 4 bytes - F-bit
   fbit_mask_value = 0x80,
   end_of_chain = 0
};

static const UInt fbit_mask = fbit_mask_value;

/* The old segname implementation allowed for 1000 names on Android and
   6000 names on other platforms. Each name was allowed to be 1000 characters
   long. That was very wasteful. */
#define VG_TABLE_SIZE 1000000

/* String table for segment names */

static HChar segnames[VG_TABLE_SIZE];  /* her majesty, the string table */
static SizeT segnames_used = 0;        /* number of bytes used */
static UInt  num_segnames = 0;         /* number of names in string table */
static UInt  num_slots = 0;            /* number of slots in string table */
static UInt  freeslot_chain = end_of_chain;

static Bool
is_freeslot(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   return (segnames[ix - 4] & fbit_mask) != 0;
}

static void
put_slotindex(UInt ix, UInt slotindex)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   if (slotindex != 0)
      aspacem_assert(slotindex >= overhead && slotindex <= segnames_used);

   slotindex |= fbit_mask << 24;
   segnames[ix - 1] = slotindex & 0xFF;   slotindex >>= 8;
   segnames[ix - 2] = slotindex & 0xFF;   slotindex >>= 8;
   segnames[ix - 3] = slotindex & 0xFF;   slotindex >>= 8;
   segnames[ix - 4] = slotindex & 0xFF;
}

static UInt
get_slotindex(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   aspacem_assert(is_freeslot(ix));

   // Avoid unexpected sign extension
   const UChar *unames = (const UChar *)segnames;

   UInt slotindex = 0;
   slotindex |= unames[ix - 4];   slotindex <<= 8;
   slotindex |= unames[ix - 3];   slotindex <<= 8;
   slotindex |= unames[ix - 2];   slotindex <<= 8;
   slotindex |= unames[ix - 1];

   return slotindex & max_slotindex;   // removes the F-bit
}

static void
put_slotsize(UInt ix, UInt size)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   aspacem_assert(size <= max_slotsize);
   segnames[ix - 1] = size & 0xff;
   segnames[ix - 2] = size >> 8;
}

static UInt
get_slotsize(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);

   // Avoid unexpected sign extension
   const UChar *unames = (const UChar *)segnames;
   if (is_freeslot(ix))
      return (unames[ix] << 8) | unames[ix+1];
   else
      return (unames[ix - 2] << 8) | unames[ix - 1];
}

static void
put_refcount(UInt ix, UInt rc)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   aspacem_assert(rc <= max_refcount);
   // rc <= max_refcount ensures that the F-bit is zero
   segnames[ix - 3] = rc & 0xff;
   segnames[ix - 4] = rc >> 8;
}

static UInt
get_refcount(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   // must not be a free slot
   aspacem_assert(! is_freeslot(ix));

   // Avoid unexpected sign extension
   const UChar *unames = (const UChar *)segnames;
   return (unames[ix - 4] << 8) | unames[ix - 3];
}

static void
inc_refcount(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   UInt rc = get_refcount(ix);
   if (rc != max_refcount)
      put_refcount(ix, rc + 1);
}

static void
dec_refcount(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);
   UInt rc = get_refcount(ix);
   aspacem_assert(rc > 0);
   if (rc != max_refcount) {
      --rc;
      if (rc != 0) {
         put_refcount(ix, rc);
      } else {
         UInt size = get_slotsize(ix);
         /* Chain this slot in the freelist */
         put_slotindex(ix, freeslot_chain);
         put_slotsize(ix + slotsize_size, size);
         freeslot_chain = ix;
         --num_segnames;
         if (0) VG_(am_show_nsegments)(0, "AFTER DECREASE rc -> 0");
      }
   }
}

static void
put_sentinel(UInt ix)
{
   aspacem_assert(ix >= overhead && ix <= segnames_used);

   put_refcount(ix, 0);
   put_slotsize(ix, 0);
}


/* Searches the string table to find an index for the given name.
   If none is found, an index is allocated and the name stored.
   If running ouf of memory, return -1. */
Int
ML_(am_allocate_segname)(const HChar *name)
{
   UInt len, ix, size, next_freeslot;

   aspacem_assert(name);

   if (0) VG_(debugLog)(0, "aspacem", "allocate_segname %s\n", name);

   len = VG_(strlen)(name);

   /* First see if we already have the name. */
   for (ix = overhead; (size = get_slotsize(ix)) != 0; ix += size + overhead) {
      if (is_freeslot(ix)) continue;
      if (VG_(strcmp)(name, segnames + ix) == 0) {
         inc_refcount(ix);
         return ix;
      }
   }

   /* Is there a free slot in the string table from a previously "freed"
      segment name ? */
   Int prev;
   for (prev = -1, ix = freeslot_chain; ix != end_of_chain;
        prev = ix, ix = next_freeslot) {
      next_freeslot = get_slotindex(ix);  // next in chain
      size = get_slotsize(ix);

      if (size >= len + 1) {
         /* Note, if the size of the slot is a lot larger than the length
            of the string we're about to store in it, we could split the
            slot into two. But that complicates matters and as we're not
            doing any coalescing of adjacent free slots this could lead to
            fragmentation. */
         if (prev == -1)
            freeslot_chain = next_freeslot;
         else
            put_slotindex(prev, next_freeslot);
         put_refcount(ix, 0);
         put_slotsize(ix, size);
         VG_(strcpy)(segnames + ix, name);
         ++num_segnames;
         return ix;
      }
   }

   /* We need to add a new name. */

   /* Note, that we need at least two bytes in the payload. The reason is
      that the payload area will be used to store the size of the slot when
      the slot is on the freelist. */
   if (len == 0) len = 1;
   
   /* Is there enough room in the string table? The OVERHEAD is for the
      sentinel following the payload of new slot. */
   SizeT need = len + 1 + overhead;
   if (need > (sizeof segnames) - segnames_used) {
      return -1;
   }

   ++num_segnames;
   ++num_slots;

   /* copy it in */
   ix = segnames_used;
   put_refcount(ix, 0);
   put_slotsize(ix, len + 1);
   VG_(strcpy)(segnames + ix, name);
   segnames_used += need;

   /* Add sentinel at end of segment name list */
   put_sentinel(segnames_used);

   return ix;
}

/* Debugging output */
void
ML_(am_show_segnames)(Int logLevel, const HChar *prefix)
{
   UInt size, ix, i;

   VG_(debugLog)(logLevel, "aspacem", "%u segment names in %u slots\n",
                 num_segnames, num_slots);

   if (freeslot_chain == end_of_chain)
      VG_(debugLog)(logLevel, "aspacem", "freelist is empty\n");
   else
      VG_(debugLog)(logLevel, "aspacem", "freelist begins at %u\n",
                    freeslot_chain);
   for (i = 0, ix = overhead; (size = get_slotsize(ix)) != 0;
        ix += size + overhead, ++i) {
      if (is_freeslot(ix))
         VG_(debugLog)(logLevel, "aspacem",
                       "(%u,%u,0) [free slot: size=%u  next=%u]\n", i, ix,
                       get_slotsize(ix), get_slotindex(ix));
      else
         VG_(debugLog)(logLevel, "aspacem",
                       "(%u,%u,%u) %s\n", i, ix, get_refcount(ix),
                       segnames + ix);
   }
}

/* Returns a sequence number for the fnIdx position in segnames.
   Used in aspacemgr debug output to associate a segment with
   a segment name. */
Int
ML_(am_segname_get_seqnr)(Int fnIdx)
{
   SizeT ix, size;
   Int seqnr = -1;

   if (fnIdx == -1) return -1;   // shortcut

   for (ix = overhead; (size = get_slotsize(ix)) != 0; ix += size + overhead) {
      seqnr++;
      if (ix == fnIdx)
         return seqnr;
   }

   // We should always find the given index; something's busted
   aspacem_assert(0);
   return -1;
}

/* Initialise the string table for segment names. It contains an empty
   string which is not referenced. */
void
ML_(am_segnames_init)(void)
{
   aspacem_assert(sizeof segnames >= overhead);

   segnames_used = overhead;
   put_sentinel(segnames_used);
}

/* Increase reference count of segment name identified by IX. */
void
ML_(am_inc_refcount)(Int ix)
{
   if (ix != -1)
      inc_refcount(ix);
}

/* Decrease reference count of segment name identified by IX. */
void
ML_(am_dec_refcount)(Int ix)
{
   if (ix != -1)
      dec_refcount(ix);
}

Bool
ML_(am_sane_segname)(Int ix)
{
   return ix == -1 || (ix >= overhead && ix < segnames_used);
}

const HChar *
ML_(am_get_segname)(Int ix)
{
   return (ix == -1) ? NULL : segnames + ix;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
