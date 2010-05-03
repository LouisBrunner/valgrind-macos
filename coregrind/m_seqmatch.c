
/*--------------------------------------------------------------------*/
/*--- A simple sequence matching facility.                         ---*/
/*---                                                 m_seqmatch.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2010 OpenWorks Ltd
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

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"    // VG_(strlen)
#include "pub_core_seqmatch.h"    // self

/* ---------------------------------------------------------------------
   A simple sequence matching facility
   ------------------------------------------------------------------ */

/* See detailed comment in include/pub_tool_seqmatch.h about this. */
Bool VG_(generic_match) ( 
        Bool matchAll,
        void* patt,  SizeT szbPatt,  UWord nPatt,  UWord ixPatt,
        void* input, SizeT szbInput, UWord nInput, UWord ixInput,
        Bool (*pIsStar)(void*),
        Bool (*pIsQuery)(void*),
        Bool (*pattEQinp)(void*,void*)
     )
{
   /* This is the spec, written in my favourite formal specification
      language.  It specifies non-greedy matching of '*'s.

      ma ('*':ps) (i:is) = ma ps (i:is) || ma ('*':ps) is
      ma ('*':ps) []     = ma ps []

      ma ('?':ps) (i:is) = ma ps is
      ma ('?':ps) []     = False

      ma (p:ps)   (i:is) = p == i && ma ps is

      ma (p:ps)   []     = False
      ma []       (i:is) = False -- m-all, True for m-prefix
      ma []       []     = True
   */
   Bool  havePatt, haveInput;
   void  *currPatt, *currInput;
  tailcall:
   vg_assert(nPatt >= 0   && nPatt  < 1000000); /* arbitrary */
   vg_assert(nInput >= 0  && nInput < 1000000); /* arbitrary */
   vg_assert(ixPatt >= 0  && ixPatt <= nPatt);
   vg_assert(ixInput >= 0 && ixInput <= nInput);

   havePatt  = ixPatt < nPatt;
   haveInput = ixInput < nInput;

   /* No specific need to set NULL when !have{Patt,Input}, but guards
      against inadvertantly dereferencing an out of range pointer to
      the pattern or input arrays. */
   currPatt  = havePatt  ? ((Char*)patt) + szbPatt * ixPatt    : NULL;
   currInput = haveInput ? ((Char*)input) + szbInput * ixInput : NULL;

   // Deal with the complex case first: wildcards.  Do frugal
   // matching.  When encountering a '*', first skip no characters
   // at all, and see if the rest of the match still works.  Only if
   // that fails do we then skip a character, and retry at the next
   // position.
   //
   // ma ('*':ps) (i:is) = ma ps (i:is) || ma ('*':ps) is
   //
   // If we're out of input, check the rest of the pattern matches
   // the empty input.  This really means can only be be empty or
   // composed entirely of '*'s.
   //
   // ma ('*':ps) []     = ma ps []
   //
   if (havePatt && pIsStar(currPatt)) {
      if (haveInput) {
         // ma ('*':ps) (i:is) = ma ps (i:is) || ma ('*':ps) is
         // we unavoidably have to make a real recursive call for the
         // first half of the OR, since this isn't straight tail-recursion.
         if (VG_(generic_match)( matchAll,
                                 patt, szbPatt, nPatt,  ixPatt+1,
                                 input,szbInput,nInput, ixInput+0,
                                 pIsStar,pIsQuery,pattEQinp) ) {
            return True;
         }
         // but we can tail-recurse for the second call
         ixInput++; goto tailcall;
      } else {
         // ma ('*':ps) []     = ma ps []
         ixPatt++; goto tailcall;
      }
   }

   // simpler cases now.  Deal with '?' wildcards.
   //
   // ma ('?':ps) (i:is) = ma ps is
   // ma ('?':ps) []     = False
   if (havePatt && pIsQuery(currPatt)) {
      if (haveInput) {
         ixPatt++; ixInput++; goto tailcall;
      } else {
         return False;
      }
   }

   // obvious case with literal chars in the pattern
   //
   // ma (p:ps)   (i:is) = p == i && ma ps is
   if (havePatt && haveInput) {
      if (!pattEQinp(currPatt,currInput)) return False;
      ixPatt++; ixInput++; goto tailcall;
   }

   // if we run out of input before we run out of pattern, we must fail
   // ma (p:ps)   []     = False
   if (havePatt && !haveInput) return False;

   // if we run out of pattern before we run out of input, the
   // verdict depends on the matching mode.  If we are trying to
   // match exactly (the pattern must consume the entire input)
   // then the outcome is failure.  However, if we're merely attempting
   // to match some prefix of the input, then we have been successful.
   //
   // ma []       (i:is) = False -- m-all, True for m-prefix
   if (!havePatt && haveInput) {
      return matchAll ? False // match-all
                      : True; // match-prefix
   }

   // finally, if both sequence and input are both completely
   // consumed, then we were successful, regardless of matching mode.
   if (!havePatt && !haveInput) return True;

   // end of cases
   vg_assert(0);
}


/* And a parameterization of the above, to make it do
   string matching.
*/
static Bool charIsStar  ( void* pV ) { return *(Char*)pV == '*'; }
static Bool charIsQuery ( void* pV ) { return *(Char*)pV == '?'; }
static Bool char_p_EQ_i ( void* pV, void* cV ) {
   Char p = *(Char*)pV;
   Char c = *(Char*)cV;
   vg_assert(p != '*' && p != '?');
   return p == c;
}
Bool VG_(string_match) ( const Char* patt, const Char* input )
{
   return VG_(generic_match)(
             True/* match-all */,
             (void*)patt,  sizeof(UChar), VG_(strlen)(patt), 0,
             (void*)input, sizeof(UChar), VG_(strlen)(input), 0,
             charIsStar, charIsQuery, char_p_EQ_i
          );
}


// test cases for the matcher (in match-all mode)
// typedef struct { char* patt; char* input; Bool xres; } Test;
//
//static Test tests[] = 
//  {
//    { ""          ,""   , True },
//    { "a"         ,""   , False },
//    { "a"         ,"b"  , False },
//    { "a"         ,"a"  , True },
//    { "a"         ,"aa" , False },
//    { "*"         ,""   , True },
//    { "**"        ,""   , True },
//    { "*"         ,"abc", True },
//    { "*a"        ,"abc", False },
//    { "*b"        ,"abc", False },
//    { "*bc"       ,"abc", True },
//    { "a*b"       ,"abc", False },
//    { "a*c"       ,"abc", True },
//    { "*c"        ,"abc", True },
//    { "c*c"       ,"abc", False },
//    { "abc*"      ,"abc", True },
//    { "abc**"     ,"abc", True },
//    { "**abc"     ,"abc", True },
//    { "**a*b*c**" ,"abc", True },
//    { "**a*b*d**" ,"abc", False },
//    { "a?b"       ,"abc", False },
//    { "a?c"       ,"abc", True },
//    { "?"         ,""   , False },
//    { "?"         ,"a"  , True },
//    { "?"         ,"ab" , False },
//    { "abcd"      ,"abc", False },
//    { "ab"        ,"abc", False },
//    { NULL        ,NULL , False }
//  };
//
//int main ( void )
//{
//   Test* t;
//   for (t = tests; t->patt; t++) {
//     printf("%10s %6s  %s\n",
//            t->patt, t->input, 
//            match_string_all((UChar*)t->patt,(UChar*)t->input,True) 
//            == t->xres
//               ? "pass" : "FAIL" );
//   }
//   return 0;
//}

/*--------------------------------------------------------------------*/
/*--- end                                             m_seqmatch.c ---*/
/*--------------------------------------------------------------------*/
