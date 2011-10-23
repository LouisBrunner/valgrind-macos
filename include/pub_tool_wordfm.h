
/*--------------------------------------------------------------------*/
/*--- An AVL tree based finite map for word keys and word values.  ---*/
/*--- Inspired by Haskell's "FiniteMap" library.                   ---*/
/*---                                            pub_tool_wordfm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2011 Julian Seward
      jseward@acm.org

   This code is based on previous work by Nicholas Nethercote
   (coregrind/m_oset.c) which is

   Copyright (C) 2005-2011 Nicholas Nethercote
       njn@valgrind.org

   which in turn was derived partially from:

      AVL C library
      Copyright (C) 2000,2002  Daniel Nagy

      This program is free software; you can redistribute it and/or
      modify it under the terms of the GNU General Public License as
      published by the Free Software Foundation; either version 2 of
      the License, or (at your option) any later version.
      [...]

      (taken from libavl-0.4/debian/copyright)

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

#ifndef __PUB_TOOL_WORDFM_H
#define __PUB_TOOL_WORDFM_H

//------------------------------------------------------------------//
//---                           WordFM                           ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

/* As of r7409 (15 Feb 08), all these word-based abstractions (WordFM,
   WordSet, WordBag) now operate on unsigned words (UWord), whereas
   they previously operated on signed words (Word).  This became a
   problem, when using unboxed comparisons (when kCmp == NULL), with
   the introduction of VG_(initIterAtFM), which allows iteration over
   parts of mappings.  Iterating over a mapping in increasing order of
   signed Word keys is not what callers expect when iterating through
   maps whose keys represent addresses (Addr) since Addr is unsigned,
   and causes logical problems and assertion failures. */

typedef  struct _WordFM  WordFM; /* opaque */

/* Allocate and initialise a WordFM.  If kCmp is non-NULL, elements in
   the set are ordered according to the ordering specified by kCmp,
   which becomes obvious if you use VG_(initIterFM),
   VG_(initIterAtFM), VG_(nextIterFM), VG_(doneIterFM) to iterate over
   sections of the map, or the whole thing.  If kCmp is NULL then the
   ordering used is unsigned word ordering (UWord) on the key
   values. */
WordFM* VG_(newFM) ( void* (*alloc_nofail)( HChar* cc, SizeT ),
                     HChar* cc,
                     void  (*dealloc)(void*),
                     Word  (*kCmp)(UWord,UWord) );

/* Free up the FM.  If kFin is non-NULL, it is applied to keys
   before the FM is deleted; ditto with vFin for vals. */
void VG_(deleteFM) ( WordFM*, void(*kFin)(UWord), void(*vFin)(UWord) );

/* Add (k,v) to fm.  If a binding for k already exists, it is updated
   to map to this new v.  In that case we should really return the
   previous v so that caller can finalise it.  Oh well.  Returns
   True if a binding for k already exists. */
Bool VG_(addToFM) ( WordFM* fm, UWord k, UWord v );

// Delete key from fm, returning associated key and val if found
Bool VG_(delFromFM) ( WordFM* fm,
                      /*OUT*/UWord* oldK, /*OUT*/UWord* oldV, UWord key );

// Look up in fm, assigning found key & val at spec'd addresses
Bool VG_(lookupFM) ( WordFM* fm, 
                     /*OUT*/UWord* keyP, /*OUT*/UWord* valP, UWord key );

// Find the closest key values bracketing the given key, assuming the 
// given key is not present in the map.  minKey and maxKey are the 
// minimum and maximum possible key values.  The resulting bracket
// values are returned in *kMinP and *kMaxP.  It follows that if fm is
// empty then the returned values are simply minKey and maxKey.
//
// For convenience the associated value fields are also returned
// through *vMinP and *vMaxP.  To make that possible in the general
// case, the caller must supply via minVal and maxVal, the value
// fields associated with minKey and maxKey.
//
// If the operation was successful (that is, the given key is not
// present), True is returned.  If the given key is in fact present,
// False is returned, and *kMinP, *vMinP, *kMaxP and *vMaxP are
// undefined.  Any of kMinP, vMinP, kMaxP and vMaxP may be safely
// supplied as NULL.
Bool VG_(findBoundsFM)( WordFM* fm,
                        /*OUT*/UWord* kMinP, /*OUT*/UWord* vMinP,
                        /*OUT*/UWord* kMaxP, /*OUT*/UWord* vMaxP,
                        UWord minKey, UWord minVal,
                        UWord maxKey, UWord maxVal,
                        UWord key );

// How many elements are there in fm?  NOTE: dangerous in the
// sense that this is not an O(1) operation but rather O(N),
// since it involves walking the whole tree.
UWord VG_(sizeFM) ( WordFM* fm );

// Is fm empty?  This at least is an O(1) operation.
// Code is present in m_wordfm.c but commented out due to no
// current usage.  Un-comment (and TEST IT) if required.
//Bool VG_(isEmptyFM)( WordFM* fm );

// set up FM for iteration
void VG_(initIterFM) ( WordFM* fm );

// set up FM for iteration so that the first key subsequently produced
// by VG_(nextIterFM) is the smallest key in the map >= start_at.
// Naturally ">=" is defined by the comparison function supplied to
// VG_(newFM), as documented above.
void VG_(initIterAtFM) ( WordFM* fm, UWord start_at );

// get next key/val pair.  Will assert if fm has been modified
// or looked up in since initIterFM/initIterWithStartFM was called.
Bool VG_(nextIterFM) ( WordFM* fm,
                       /*OUT*/UWord* pKey, /*OUT*/UWord* pVal );

// clear the I'm iterating flag
void VG_(doneIterFM) ( WordFM* fm );

// Deep copy a FM.  If dopyK is NULL, keys are copied verbatim.
// If non-null, dopyK is applied to each key to generate the
// version in the new copy.  In that case, if the argument to dopyK
// is non-NULL but the result is NULL, it is assumed that dopyK
// could not allocate memory, in which case the copy is abandoned
// and NULL is returned.  Ditto with dopyV for values.
WordFM* VG_(dopyFM) ( WordFM* fm,
                      UWord(*dopyK)(UWord), UWord(*dopyV)(UWord) );

// admin: what's the 'common' allocation size (for tree nodes?)
SizeT VG_(getNodeSizeFM)( void );

//------------------------------------------------------------------//
//---                         end WordFM                         ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

//------------------------------------------------------------------//
//---                WordBag (unboxed words only)                ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

typedef  struct _WordBag  WordBag; /* opaque */

/* Allocate and initialise a WordBag */
WordBag* VG_(newBag) ( void* (*alloc_nofail)( HChar* cc, SizeT ),
                       HChar* cc,
                       void  (*dealloc)(void*) );

/* Free up the Bag. */
void VG_(deleteBag) ( WordBag* );

/* Add a word. */
void VG_(addToBag)( WordBag*, UWord );

/* Find out how many times the given word exists in the bag. */
UWord VG_(elemBag) ( WordBag*, UWord );

/* Delete a word from the bag. */
Bool VG_(delFromBag)( WordBag*, UWord );

/* Is the bag empty? */
Bool VG_(isEmptyBag)( WordBag* );

/* Does the bag have exactly one element? */
Bool VG_(isSingletonTotalBag)( WordBag* );

/* Return an arbitrary element from the bag. */
UWord VG_(anyElementOfBag)( WordBag* );

/* How many different / total elements are in the bag? */
UWord VG_(sizeUniqueBag)( WordBag* ); /* fast */
UWord VG_(sizeTotalBag)( WordBag* );  /* warning: slow */

/* Iterating over the elements of a bag. */
void VG_(initIterBag)( WordBag* );
Bool VG_(nextIterBag)( WordBag*, /*OUT*/UWord* pVal, /*OUT*/UWord* pCount );
void VG_(doneIterBag)( WordBag* );

//------------------------------------------------------------------//
//---             end WordBag (unboxed words only)               ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

#endif /* ! __PUB_TOOL_WORDFM_H */

/*--------------------------------------------------------------------*/
/*--- end                                        pub_tool_wordfm.h ---*/
/*--------------------------------------------------------------------*/
