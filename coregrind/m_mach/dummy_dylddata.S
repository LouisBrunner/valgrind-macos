/*--------------------------------------------------------------------*/
/*--- Dummy __DATA,__dyld4.                       dummy_dylddata.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2024-2025 Louis Brunner <louis.brunner.fr@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "config.h"

#if DARWIN_VERS >= DARWIN_15_04
.section __TPRO_CONST,__dyld_apis
.global __TPRO_CONST.__dyld_apis
__TPRO_CONST.__dyld_apis:
#elif DARWIN_VERS >= DARWIN_15_00
.section __TPRO_CONST,__dyld4
.global __TPRO_CONST.__dyld4
__TPRO_CONST.__dyld4:
#else
.section __DATA,__dyld4
.global __DATA.__dyld4
.balign 4
__DATA.__dyld4:
#endif

; APIs*               apis;
  .quad 0 ; (set by dyld, null)

# dyld used by 15.4 and later replaces these with __DATA_CONST,__helper
#if DARWIN_VERS < DARWIN_15_04
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

.section __DATA,__fake
.global __DATA.__fake
__DATA.__fake:
dyld4_ProgramVars_NXArgcPtr:
  .quad 0
dyld4_ProgramVars_NXArgvPtr:
  .quad 0
dyld4_ProgramVars_environPtr:
  .quad 0
dyld4_ProgramVars___prognamePtr:
  .quad 0
#endif

.text
.global dyld_stub_binder
dyld_stub_binder:
  ret

#if DARWIN_VERS >= DARWIN_15_04
.global  _do_syscall
_do_syscall:
  mov     x16, x0

  mov     x0,  x1
  mov     x1,  x2
  mov     x2,  x3
  mov     x3,  x4
  mov     x4,  x5
  mov     x5,  x6
  mov     x6,  x7

  svc     #0x80

  ret
#endif
