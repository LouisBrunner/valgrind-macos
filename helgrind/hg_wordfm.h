
/*--------------------------------------------------------------------*/
/*--- An AVL tree based finite map for word keys and word values.  ---*/
/*--- Inspired by Haskell's "FiniteMap" library.                   ---*/
/*---                                                  hg_wordfm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2008 Julian Seward
      jseward@acm.org

   This code is based on previous work by Nicholas Nethercote
   (coregrind/m_oset.c) which is

   Copyright (C) 2005-2008 Nicholas Nethercote
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

#ifndef __HG_WORDFM_H
#define __HG_WORDFM_H

//------------------------------------------------------------------//
//---                           WordFM                           ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

typedef  struct _WordFM  WordFM; /* opaque */

/* Allocate and initialise a WordFM.  If kCmp is non-NULL, elements in
   the set are ordered according to the ordering specified by kCmp,
   which becomes obvious if you use VG_(initIterFM),
   VG_(initIterAtFM), VG_(nextIterFM), VG_(doneIterFM) to iterate over
   sections of the map, or the whole thing. */
WordFM* HG_(newFM) ( void* (*alloc_nofail)( SizeT ),
                     void  (*dealloc)(void*),
                     Word  (*kCmp)(UWord,UWord) );

/* Free up the FM.  If kFin is non-NULL, it is applied to keys
   before the FM is deleted; ditto with vFin for vals. */
void HG_(deleteFM) ( WordFM*, void(*kFin)(UWord), void(*vFin)(UWord) );

/* Add (k,v) to fm.  If a binding for k already exists, it is updated
   to map to this new v.  In that case we should really return the
   previous v so that caller can finalise it.  Oh well. */
void HG_(addToFM) ( WordFM* fm, UWord k, UWord v );

// Delete key from fm, returning associated key and val if found
Bool HG_(delFromFM) ( WordFM* fm,
                      /*OUT*/UWord* oldK, /*OUT*/UWord* oldV, UWord key );

// Look up in fm, assigning found key & val at spec'd addresses
Bool HG_(lookupFM) ( WordFM* fm, 
                     /*OUT*/UWord* keyP, /*OUT*/UWord* valP, UWord key );

// How many elements are there in fm?
UWord HG_(sizeFM) ( WordFM* fm );

// set up FM for iteration
void HG_(initIterFM) ( WordFM* fm );

// set up FM for iteration so that the first key subsequently produced
// by HG_(nextIterFM) is the smallest key in the map >= start_at.
// Naturally ">=" is defined by the comparison function supplied to
// HG_(newFM), as documented above.
void HG_(initIterAtFM) ( WordFM* fm, UWord start_at );

// get next key/val pair.  Will assert if fm has been modified
// or looked up in since initIterFM/initIterWithStartFM was called.
Bool HG_(nextIterFM) ( WordFM* fm,
                       /*OUT*/UWord* pKey, /*OUT*/UWord* pVal );

// clear the I'm iterating flag
void HG_(doneIterFM) ( WordFM* fm );

// Deep copy a FM.  If dopyK is NULL, keys are copied verbatim.
// If non-null, dopyK is applied to each key to generate the
// version in the new copy.  In that case, if the argument to dopyK
// is non-NULL but the result is NULL, it is assumed that dopyK
// could not allocate memory, in which case the copy is abandoned
// and NULL is returned.  Ditto with dopyV for values.
WordFM* HG_(dopyFM) ( WordFM* fm,
                      UWord(*dopyK)(UWord), UWord(*dopyV)(UWord) );

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
WordBag* HG_(newBag) ( void* (*alloc_nofail)( SizeT ),
                       void  (*dealloc)(void*) );

/* Free up the Bag. */
void HG_(deleteBag) ( WordBag* );

/* Add a word. */
void HG_(addToBag)( WordBag*, UWord );

/* Find out how many times the given word exists in the bag. */
UWord HG_(elemBag) ( WordBag*, UWord );

/* Delete a word from the bag. */
Bool HG_(delFromBag)( WordBag*, UWord );

/* Is the bag empty? */
Bool HG_(isEmptyBag)( WordBag* );

/* Does the bag have exactly one element? */
Bool HG_(isSingletonTotalBag)( WordBag* );

/* Return an arbitrary element from the bag. */
UWord HG_(anyElementOfBag)( WordBag* );

/* How many different / total elements are in the bag? */
UWord HG_(sizeUniqueBag)( WordBag* ); /* fast */
UWord HG_(sizeTotalBag)( WordBag* );  /* warning: slow */

/* Iterating over the elements of a bag. */
void HG_(initIterBag)( WordBag* );
Bool HG_(nextIterBag)( WordBag*, /*OUT*/UWord* pVal, /*OUT*/UWord* pCount );
void HG_(doneIterBag)( WordBag* );

//------------------------------------------------------------------//
//---             end WordBag (unboxed words only)               ---//
//---                      Public interface                      ---//
//------------------------------------------------------------------//

#endif /* ! __HG_WORDFM_H */

/*--------------------------------------------------------------------*/
/*--- end                                              hg_wordfm.h ---*/
/*--------------------------------------------------------------------*/
