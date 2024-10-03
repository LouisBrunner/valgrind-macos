/*--------------------------------------------------------------------*/
/*--- Dummy __DATA,__dyld4.                       dummy_dylddata.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2024 Louis Brunner <louis.brunner.fr@gmail.com>

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

.section __DATA,__fake
.globl __DATA.__fake
__DATA.__fake:
dyld4_ProgramVars_NXArgcPtr:
  .quad 0
dyld4_ProgramVars_NXArgvPtr:
  .quad 0
dyld4_ProgramVars_environPtr:
  .quad 0
dyld4_ProgramVars___prognamePtr:
  .quad 0

.section __DATA,__dyld4
.globl __DATA.__dyld4
__DATA.__dyld4:
; APIs*               apis;
.quad 0 ; (set by dyld, null)
; void*               allImageInfos;
.quad 0 ; (set by dyld, null)
; dyld4::ProgramVars  defaultVars; (5 pointers)
;; const void*      mh;
.quad 0 ; (set by dyld, null)
;; int*             NXArgcPtr;
.quad dyld4_ProgramVars_NXArgcPtr ; (used by dyld, non-null)
;; const char***    NXArgvPtr;
.quad dyld4_ProgramVars_NXArgvPtr ; (used by dyld, non-null)
;; const char***    environPtr;
.quad dyld4_ProgramVars_environPtr ; (used by dyld, non-null)
;; const char**     __prognamePtr;
.quad dyld4_ProgramVars___prognamePtr ; (used by dyld, non-null)
; dyld3::DyldLookFunc dyldLookupFuncAddr;
.quad 0 ; (func pointer, used by dyld, non-null)
; void* (*tlv_get_addrAddr)(dyld3::MachOAnalyzer::TLV_Thunk*);
.quad 0 ; (used by dyld, non-null)

.text
.globl _dyld_stub_binder
_dyld_stub_binder:
  ret
