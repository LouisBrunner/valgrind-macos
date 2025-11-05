
/*--------------------------------------------------------------------*/
/*--- Types and macros for writing extensions.                     ---*/
/*---                                        priv_types_n_macros.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) IBM Corp. 2024

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

/* Contributed by Andreas Arnez */

#ifndef __PRIV_TYPES_N_MACROS_H
#define __PRIV_TYPES_N_MACROS_H

#include "pub_core_basics.h" // Addr
#include "pub_core_libcprint.h"
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"

#define PRE_REG_READ(tst, name, reg, offset, len)                              \
   if (VG_(tdict).track_pre_reg_read) {                                        \
      VexGuestArchState* gst = &tst->arch.vex;                                 \
      VG_(tdict).track_pre_reg_read(                                           \
         Vg_CoreSysCall, tst->tid, name,                                       \
         (UChar*)&gst->guest_##reg - (UChar*)gst + (offset), len);             \
   }

#define POST_REG_WRITE(tst, reg, offset, len)                                  \
   if (VG_(tdict).track_post_reg_write) {                                      \
      VexGuestArchState* gst = &tst->arch.vex;                                 \
      VG_(tdict).track_post_reg_write(                                         \
         Vg_CoreSysCall, tst->tid,                                             \
         (UChar*)&gst->guest_##reg - (UChar*)gst + offset, len);               \
   }

#define PRE_MEM_READ(tst, name, addr, len)                                     \
   if (VG_(tdict).track_pre_mem_read) {                                        \
      VG_(tdict).track_pre_mem_read(Vg_CoreSysCall, tst->tid, name, addr,      \
                                    len);                                      \
   }

#define PRE_MEM_WRITE(tst, name, addr, len)                                    \
   if (VG_(tdict).track_pre_mem_write) {                                       \
      VG_(tdict).track_pre_mem_write(Vg_CoreSysCall, tst->tid, name, addr,     \
                                     len);                                     \
   }

#define POST_MEM_WRITE(tst, addr, len)                                         \
   if (VG_(tdict).track_post_mem_write) {                                      \
      VG_(tdict).track_post_mem_write(Vg_CoreSysCall, tst->tid, addr, len);    \
   }

#define REG_READ(tst, reg)                                                     \
   ({                                                                          \
      PRE_REG_READ(tst, #reg, reg, 0, sizeof(tst->arch.vex.guest_##reg));      \
      tst->arch.vex.guest_##reg;                                               \
   })


#endif
