
/*---------------------------------------------------------------*/
/*--- begin                             guest_nanomips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK
      mips-valgrind@rt-rk.com

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

/* Only to be used within the guest-mips directory. */

#ifndef __VEX_GUEST_NANOMIPS_DEFS_H
#define __VEX_GUEST_NANOMIPS_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"  /* DisResult */
#include "common_nanomips_defs.h"

#if defined (_MIPSEL)
   #define MIPS_IEND Iend_LE
#else
   #define MIPS_IEND Iend_BE
#endif

/*---------------------------------------------------------*/
/*---               mips to IR conversion               ---*/
/*---------------------------------------------------------*/

/* Convert one nanoMIPS insn to IR. See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
extern DisResult disInstr_nanoMIPS ( IRSB*        irbb,
                                     const UChar* guest_code,
                                     Long         delta,
                                     Addr         guest_IP,
                                     VexArch      guest_arch,
                                     const VexArchInfo* archinfo,
                                     const VexAbiInfo*  abiinfo,
                                     VexEndness   host_endness,
                                     Bool         sigill_diag );


extern VexGuestLayout nanomipsGuest_layout;

extern HWord nanomips_dirtyhelper_rdhwr ( UInt rd );

#endif

/*---------------------------------------------------------------*/
/*--- end                               guest_nanomips_defs.h ---*/
/*---------------------------------------------------------------*/
