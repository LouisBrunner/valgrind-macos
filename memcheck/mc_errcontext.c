
/*--------------------------------------------------------------------*/
/*--- Management of memory error messages.                         ---*/
/*---                                              mc_errcontext.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2004 Julian Seward 
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
/*--- Printing errors                                      ---*/
/*------------------------------------------------------------*/

void SK_(pp_SkinError) ( Error* err )
{
   MAC_Error* err_extra = VG_(get_error_extra)(err);

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
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
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
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
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
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         MAC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
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
         VG_(pp_ExeContext)( VG_(get_error_where)(err) );
         MAC_(pp_AddrInfo)(VG_(get_error_address)(err), &err_extra->addrinfo);
         break;

      default: 
         MAC_(pp_shared_SkinError)(err);
         break;
   }
}

/*------------------------------------------------------------*/
/*--- Recording errors                                     ---*/
/*------------------------------------------------------------*/

/* Creates a copy of the `extra' part, updates the copy with address info if
   necessary, and returns the copy. */
/* This one called from generated code and non-generated code. */
void MC_(record_value_error) ( ThreadId tid, Int size )
{
   MAC_Error err_extra;

   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.size = size;
   VG_(maybe_record_error)( tid, ValueErr, /*addr*/0, /*s*/NULL, &err_extra );
}

/* This called from non-generated code */

void MC_(record_user_error) ( ThreadId tid, Addr a, Bool isWrite )
{
   MAC_Error err_extra;

   sk_assert(VG_INVALID_THREADID != tid);
   MAC_(clear_MAC_Error)( &err_extra );
   err_extra.addrinfo.akind = Undescribed;
   err_extra.isWrite        = isWrite;
   VG_(maybe_record_error)( tid, UserErr, a, /*s*/NULL, &err_extra );
}

/*------------------------------------------------------------*/
/*--- Suppressions                                         ---*/
/*------------------------------------------------------------*/

Bool SK_(recognised_suppression) ( Char* name, Supp* su )
{
   SuppKind skind;

   if (MAC_(shared_recognised_suppression)(name, su))
      return True;

   /* Extra suppressions not used by Addrcheck */
   else if (VG_STREQ(name, "Cond"))    skind = Value0Supp;
   else if (VG_STREQ(name, "Value0"))  skind = Value0Supp;/* backwards compat */
   else if (VG_STREQ(name, "Value1"))  skind = Value1Supp;
   else if (VG_STREQ(name, "Value2"))  skind = Value2Supp;
   else if (VG_STREQ(name, "Value4"))  skind = Value4Supp;
   else if (VG_STREQ(name, "Value8"))  skind = Value8Supp;
   else if (VG_STREQ(name, "Value16")) skind = Value16Supp;
   else 
      return False;

   VG_(set_supp_kind)(su, skind);
   return True;
}

/*--------------------------------------------------------------------*/
/*--- end                                          mc_errcontext.c ---*/
/*--------------------------------------------------------------------*/
