
/*--------------------------------------------------------------------*/
/*--- Error management for Helgrind.                               ---*/
/*---                                                  hg_errors.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2017 OpenWorks Ltd
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __HG_ERRORS_H
#define __HG_ERRORS_H


/* The standard bundle of error management functions that we are
required to present to the core/tool interface at startup. */
Bool  HG_(eq_Error)        ( VgRes not_used, const Error* e1, const Error* e2 );
void  HG_(before_pp_Error) ( const Error* err );
void  HG_(pp_Error)        ( const Error* err );
UInt  HG_(update_extra)    ( const Error* err );
Bool  HG_(recognised_suppression) ( const HChar* name, Supp *su );
Bool  HG_(read_extra_suppression_info) ( Int fd, HChar** bufpp, SizeT* nBufp,
                                         Int* lineno, Supp* su );
Bool  HG_(error_matches_suppression) ( const Error* err, const Supp* su );
const HChar* HG_(get_error_name) ( const Error* err );
SizeT HG_(get_extra_suppression_info) ( const Error* err,
                                        /*OUT*/HChar* buf, Int nBuf );
SizeT HG_(print_extra_suppression_use) ( const Supp* su,
                                         /*OUT*/HChar* buf, Int nBuf );
void  HG_(update_extra_suppression_use) ( const Error* err, const Supp* su );

/* Functions for recording various kinds of errors. */
void HG_(record_error_Race) ( Thread* thr, 
                              Addr data_addr, Int szB, Bool isWrite,
                              Thread* h1_confthr,
                              ExeContext* h1_ct_segstart,
                              ExeContext* h1_ct_mbsegend );
void HG_(record_error_UnlockUnlocked) ( Thread*, Lock* );
void HG_(record_error_UnlockForeign)  ( Thread*, Thread*, Lock* );
void HG_(record_error_UnlockBogus)    ( Thread*, Addr );
void HG_(record_error_PthAPIerror)    ( Thread*, const HChar*, Word,
                                        const HChar* );

/* Function for printing the details about an access */
void HG_(print_access) (StackTrace ips, UInt n_ips,
                        Thr*  thr_a,
                        Addr  ga,
                        SizeT SzB,
                        Bool  isW,
                        WordSetID locksHeldW );

/* see the implementation for meaning of these params */
void HG_(record_error_LockOrder)      ( Thread*, Lock*, Lock*,
                                        ExeContext*, ExeContext*,
                                        ExeContext* );

void HG_(record_error_Misc_w_aux)     ( Thread*, const HChar* errstr,
                                        const HChar* auxstr,
                                        ExeContext* auxctx );
void HG_(record_error_Misc)           ( Thread* thr, const HChar* errstr );

void HG_(record_error_Dubious_w_aux)  ( Thread*, const HChar* errstr,
                                        const HChar* auxstr,
                                        ExeContext* auxctx );
void HG_(record_error_Dubious)        ( Thread* thr, const HChar* errstr );


/* Statistics pertaining to error management. */
extern ULong HG_(stats__LockN_to_P_queries);
extern ULong HG_(stats__LockN_to_P_get_map_size) ( void );
extern ULong HG_(stats__string_table_queries);
extern ULong HG_(stats__string_table_get_map_size) ( void );

#endif /* ! __HG_ERRORS_H */

/*--------------------------------------------------------------------*/
/*--- end                                              hg_errors.h ---*/
/*--------------------------------------------------------------------*/
