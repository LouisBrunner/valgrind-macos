
/*--------------------------------------------------------------------*/
/*--- Sets of words, with unique set identifiers.                  ---*/
/*---                                                 hg_wordset.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2010 OpenWorks LLP
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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __HG_WORDSET_H
#define __HG_WORDSET_H

//------------------------------------------------------------------//
//---                          WordSet                           ---//
//---                      Public Interface                      ---//
//------------------------------------------------------------------//

typedef  struct _WordSetU  WordSetU;  /* opaque */

typedef  UInt              WordSet;   /* opaque, small int index */

/* Allocate and initialise a WordSetU */
WordSetU* HG_(newWordSetU) ( void* (*alloc_nofail)( HChar*, SizeT ),
                             HChar* cc,
                             void  (*dealloc)(void*),
                             Word  cacheSize );

/* Free up the WordSetU. */
void HG_(deleteWordSetU) ( WordSetU* );

/* Get the number of elements in this WordSetU. */
UWord HG_(cardinalityWSU) ( WordSetU* );

/* Show performance stats for this WordSetU. */
void HG_(ppWSUstats) ( WordSetU* wsu, HChar* name );


/* Element-level operations on WordSets.  Note that the WordSet
   numbers given out are 0, 1, 2, 3, etc, and as it happens 0 always
   represents the empty set. */

WordSet HG_(emptyWS)        ( WordSetU* );
WordSet HG_(addToWS)        ( WordSetU*, WordSet, UWord );
WordSet HG_(delFromWS)      ( WordSetU*, WordSet, UWord );
WordSet HG_(unionWS)        ( WordSetU*, WordSet, WordSet );
WordSet HG_(intersectWS)    ( WordSetU*, WordSet, WordSet );
WordSet HG_(minusWS)        ( WordSetU*, WordSet, WordSet );
Bool    HG_(isEmptyWS)      ( WordSetU*, WordSet );
Bool    HG_(isSingletonWS)  ( WordSetU*, WordSet, UWord );
UWord   HG_(anyElementOfWS) ( WordSetU*, WordSet );
UWord   HG_(cardinalityWS)  ( WordSetU*, WordSet );
Bool    HG_(elemWS)         ( WordSetU*, WordSet, UWord );
WordSet HG_(doubletonWS)    ( WordSetU*, UWord, UWord );
WordSet HG_(singletonWS)    ( WordSetU*, UWord );
WordSet HG_(isSubsetOf)     ( WordSetU*, WordSet, WordSet );

Bool    HG_(plausibleWS)    ( WordSetU*, WordSet );
Bool    HG_(saneWS_SLOW)    ( WordSetU*, WordSet );

void    HG_(ppWS)           ( WordSetU*, WordSet );
void    HG_(getPayloadWS)   ( /*OUT*/UWord** words, /*OUT*/UWord* nWords, 
                             WordSetU*, WordSet );


//------------------------------------------------------------------//
//---                        end WordSet                         ---//
//---                      Public Interface                      ---//
//------------------------------------------------------------------//

#endif /* ! __HG_WORDSET_H */

/*--------------------------------------------------------------------*/
/*--- end                                             hg_wordset.h ---*/
/*--------------------------------------------------------------------*/
