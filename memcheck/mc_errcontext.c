
/*--------------------------------------------------------------------*/
/*--- Management of memory error messages.                         ---*/
/*---                                              mc_errcontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors.

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

   The GNU General Public License is contained in the file COPYING.
*/

#include "mc_include.h"

/*------------------------------------------------------------*/
/*--- Comparing and printing errors                        ---*/
/*------------------------------------------------------------*/

void SK_(pp_SkinError) ( Error* err, void (*pp_ExeContext)(void) )
{
   MemCheckError* err_extra = VG_(get_error_extra)(err);

   switch (VG_(get_error_kind)(err)) {
      case CoreMemErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "%s contains unaddressable byte(s)", VG_(get_error_string)(err));
         } else {
            VG_(message)(Vg_UserMsg, 
                "%s contains uninitialised or unaddressable byte(s)",
                VG_(get_error_string)(err));
         }
         pp_ExeContext();
         break;
      
      case ValueErr:
         if (err_extra->size == 0) {
             VG_(message)(
                Vg_UserMsg,
                "Conditional jump or move depends on uninitialised value(s)");
         } else {
             VG_(message)(Vg_UserMsg,
                          "Use of uninitialised value of size %d",
                          err_extra->size);
         }
         pp_ExeContext();
         break;

      case AddrErr:
         switch (err_extra->axskind) {
            case ReadAxs:
               VG_(message)(Vg_UserMsg, "Invalid read of size %d", 
                                        err_extra->size ); 
               break;
            case WriteAxs:
               VG_(message)(Vg_UserMsg, "Invalid write of size %d", 
                                        err_extra->size ); 
               break;
            case ExecAxs:
               VG_(message)(Vg_UserMsg, "Jump to the invalid address "
                                        "stated on the next line");
               break;
            default: 
               VG_(skin_panic)("SK_(pp_SkinError)(axskind)");
         }
         pp_ExeContext();
         MC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case FreeErr:
         VG_(message)(Vg_UserMsg,"Invalid free() / delete / delete[]");
         /* fall through */
      case FreeMismatchErr:
         if (VG_(get_error_kind)(err) == FreeMismatchErr)
            VG_(message)(Vg_UserMsg, 
                         "Mismatched free() / delete / delete []");
         pp_ExeContext();
         MC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case ParamErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "Syscall param %s contains unaddressable byte(s)",
                VG_(get_error_string)(err));
         } else {
            VG_(message)(Vg_UserMsg, 
                "Syscall param %s contains uninitialised or "
                "unaddressable byte(s)",
                VG_(get_error_string)(err));
         }
         pp_ExeContext();
         MC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      case UserErr:
         if (err_extra->isWrite) {
            VG_(message)(Vg_UserMsg, 
               "Unaddressable byte(s) found during client check request");
         } else {
            VG_(message)(Vg_UserMsg, 
               "Uninitialised or "
               "unaddressable byte(s) found during client check request");
         }
         pp_ExeContext();
         MC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      default: 
         VG_(printf)("Error:\n  unknown MemCheck error code %d\n",
                     VG_(get_error_kind)(err));
         VG_(skin_panic)("unknown error code in SK_(pp_SkinError)");
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* Describe an address as best you can, for error messages,
   putting the result in ai. */

static void mc_describe_addr ( Addr a, AddrInfo* ai )
{
   ShadowChunk* sc;
   Bool         ok;
   ThreadId     tid;

   /* Nested functions, yeah.  Need the lexical scoping of 'a'. */ 

   /* Closure for searching thread stacks */
   Bool addr_is_in_bounds(Addr stack_min, Addr stack_max)
   {
      return (stack_min <= a && a <= stack_max);
   }
   /* Closure for searching malloc'd and free'd lists */
   Bool addr_is_in_block(ShadowChunk *sh_ch)
   {
      return VG_(addr_is_in_block) ( a, VG_(get_sc_data)(sh_ch),
                                        VG_(get_sc_size)(sh_ch) );
   }

   /* Perhaps it's a user-def'd block ? */
   ok = MC_(client_perm_maybe_describe)( a, ai );
   if (ok)
      return;
   /* Perhaps it's on a thread's stack? */
   tid = VG_(any_matching_thread_stack)(addr_is_in_bounds);
   if (tid != VG_INVALID_THREADID) {
      ai->akind     = Stack;
      ai->stack_tid = tid;
      return;
   }
   /* Search for a recently freed block which might bracket it. */
   sc = MC_(any_matching_freed_ShadowChunks)(addr_is_in_block);
   if (NULL != sc) {
      ai->akind      = Freed;
      ai->blksize    = VG_(get_sc_size)(sc);
      ai->rwoffset   = (Int)(a) - (Int)(VG_(get_sc_data)(sc));
      ai->lastchange = (ExeContext*)( VG_(get_sc_extra)(sc, 0) );
      return;
   }
   /* Search for a currently malloc'd block which might bracket it. */
   sc = VG_(any_matching_mallocd_ShadowChunks)(addr_is_in_block);
   if (NULL != sc) {
      ai->akind      = Mallocd;
      ai->blksize    = VG_(get_sc_size)(sc);
      ai->rwoffset   = (Int)(a) - (Int)(VG_(get_sc_data)(sc));
      ai->lastchange = (ExeContext*)( VG_(get_sc_extra)(sc, 0) );
      return;
   } 
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}

/* Creates a copy of the `extra' part, updates the copy with address info if
   necessary, and returns the copy. */
void* SK_(dup_extra_and_update)( Error* err )
{
   MemCheckError* new_extra;

   new_extra  = VG_(malloc)(sizeof(MemCheckError));
   *new_extra = *((MemCheckError*)VG_(get_error_extra)(err));

   if (new_extra->addrinfo.akind == Undescribed)
      mc_describe_addr ( VG_(get_error_address)(err), &(new_extra->addrinfo) );

   return new_extra;
}


/* This one called from generated code. */
void MC_(record_value_error) ( Int size )
{
   MemCheckError err_extra;

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.size = size;
   VG_(maybe_record_error)( NULL, ValueErr, /*addr*/0, /*s*/NULL, &err_extra );
}

/* This one called from non-generated code */
void MC_(record_user_error) ( ThreadState* tst, Addr a, Bool isWrite )
{
   MemCheckError err_extra;

   sk_assert(NULL != tst);

   MC_(clear_MemCheckError)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isWrite        = isWrite;
   VG_(maybe_record_error)( tst, UserErr, a, /*s*/NULL, &err_extra );
}

/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

#define STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                      && VG_(strcmp)((s1),(s2))==0)

Bool SK_(recognised_suppression) ( Char* name, Supp* su )
{
   SuppKind skind;
   
   if      (STREQ(name, "Param"))   skind = ParamSupp;
   else if (STREQ(name, "CoreMem")) skind = CoreMemSupp;
   else if (STREQ(name, "Value0"))  skind = Value0Supp; /* backwards compat */ 
   else if (STREQ(name, "Cond"))    skind = Value0Supp;
   else if (STREQ(name, "Value1"))  skind = Value1Supp;
   else if (STREQ(name, "Value2"))  skind = Value2Supp;
   else if (STREQ(name, "Value4"))  skind = Value4Supp;
   else if (STREQ(name, "Value8"))  skind = Value8Supp;
   else if (STREQ(name, "Addr1"))   skind = Addr1Supp;
   else if (STREQ(name, "Addr2"))   skind = Addr2Supp;
   else if (STREQ(name, "Addr4"))   skind = Addr4Supp;
   else if (STREQ(name, "Addr8"))   skind = Addr8Supp;
   else if (STREQ(name, "Free"))    skind = FreeSupp;
   else 
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

#  undef STREQ

/*--------------------------------------------------------------------*/
/*--- end                                          mc_errcontext.c ---*/
/*--------------------------------------------------------------------*/
