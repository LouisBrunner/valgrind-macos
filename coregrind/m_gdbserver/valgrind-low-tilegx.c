/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2012
   Free Software Foundation, Inc.

   This file is part of VALGRIND.
   It has been inspired from a file from gdbserver in gdb 6.6.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA. */

#include "server.h"
#include "target.h"
#include "regdef.h"
#include "regcache.h"

#include "pub_core_aspacemgr.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h"

#include "valgrind_low.h"

#include "libvex_guest_tilegx.h"
#define  REG_ONE(_n)  { "r"#_n, 64 * (_n), 64 }
#define  REG_ONE_NAME(_n, _name) { _name, 64 * (_n), 64 }

static struct reg regs[] = {
  REG_ONE(0),
  REG_ONE(1),
  REG_ONE(2),
  REG_ONE(3),
  REG_ONE(4),
  REG_ONE(5),
  REG_ONE(6),
  REG_ONE(7),
  REG_ONE(8),
  REG_ONE(9),

  REG_ONE(10),
  REG_ONE(11),
  REG_ONE(12),
  REG_ONE(13),
  REG_ONE(14),
  REG_ONE(15),
  REG_ONE(16),
  REG_ONE(17),
  REG_ONE(18),
  REG_ONE(19),

  REG_ONE(20),
  REG_ONE(21),
  REG_ONE(22),
  REG_ONE(23),
  REG_ONE(24),
  REG_ONE(25),
  REG_ONE(26),
  REG_ONE(27),
  REG_ONE(28),
  REG_ONE(29),

  REG_ONE(30),
  REG_ONE(31),
  REG_ONE(32),
  REG_ONE(33),
  REG_ONE(34),
  REG_ONE(35),
  REG_ONE(36),
  REG_ONE(37),
  REG_ONE(38),
  REG_ONE(39),

  REG_ONE(40),
  REG_ONE(41),
  REG_ONE(42),
  REG_ONE(43),
  REG_ONE(44),
  REG_ONE(45),
  REG_ONE(46),
  REG_ONE(47),
  REG_ONE(48),
  REG_ONE(49),

  REG_ONE(50),
  REG_ONE(51),
  REG_ONE(52),
  REG_ONE(53),

  REG_ONE_NAME(54, "sp"),
  REG_ONE_NAME(55, "lr"),
  REG_ONE(56),
  REG_ONE(57),
  REG_ONE(58),
  REG_ONE(59),

  REG_ONE(60),
  REG_ONE(61),
  REG_ONE(62),
  REG_ONE_NAME(63, "zero"),
  REG_ONE_NAME(64, "pc"),
};

#define num_regs (sizeof (regs) / sizeof (regs[0]))

static const char *expedite_regs[] = { "sp", "pc", 0 };

static
CORE_ADDR get_pc (void)
{
  unsigned long pc;

  collect_register_by_name ("pc", &pc);

  dlog(1, "stop pc is %p\n", (void *) pc);
  return pc;
}

static
void set_pc ( CORE_ADDR newpc )
{
  Bool mod;
  supply_register_by_name ("pc", &newpc, &mod);
  if (mod)
    dlog(1, "set pc to %p\n", C2v (newpc));
  else
    dlog(1, "set pc not changed %p\n", C2v (newpc));
}

/* store registers in the guest state (gdbserver_to_valgrind)
   or fetch register from the guest state (valgrind_to_gdbserver). */
static
void transfer_register ( ThreadId tid, int abs_regno, void * buf,
                         transfer_direction dir, int size, Bool *mod )
{
  ThreadState* tst = VG_(get_ThreadState)(tid);
  int set = abs_regno / num_regs;
  int regno = abs_regno % num_regs;
  *mod = False;

  VexGuestTILEGXState* tilegx = (VexGuestTILEGXState*) get_arch (set, tst);

  switch (regno) {
  case 0: VG_(transfer) (&tilegx->guest_r0, buf, dir, size, mod); break;
  case 1: VG_(transfer) (&tilegx->guest_r1, buf, dir, size, mod); break;
  case 2: VG_(transfer) (&tilegx->guest_r2, buf, dir, size, mod); break;
  case 3: VG_(transfer) (&tilegx->guest_r3, buf, dir, size, mod); break;
  case 4: VG_(transfer) (&tilegx->guest_r4, buf, dir, size, mod); break;
  case 5: VG_(transfer) (&tilegx->guest_r5, buf, dir, size, mod); break;
  case 6: VG_(transfer) (&tilegx->guest_r6, buf, dir, size, mod); break;
  case 7: VG_(transfer) (&tilegx->guest_r7, buf, dir, size, mod); break;
  case 8: VG_(transfer) (&tilegx->guest_r8, buf, dir, size, mod); break;
  case 9: VG_(transfer) (&tilegx->guest_r9, buf, dir, size, mod); break;
  case 10: VG_(transfer) (&tilegx->guest_r10, buf, dir, size, mod); break;
  case 11: VG_(transfer) (&tilegx->guest_r11, buf, dir, size, mod); break;
  case 12: VG_(transfer) (&tilegx->guest_r12, buf, dir, size, mod); break;
  case 13: VG_(transfer) (&tilegx->guest_r13, buf, dir, size, mod); break;
  case 14: VG_(transfer) (&tilegx->guest_r14, buf, dir, size, mod); break;
  case 15: VG_(transfer) (&tilegx->guest_r15, buf, dir, size, mod); break;
  case 16: VG_(transfer) (&tilegx->guest_r16, buf, dir, size, mod); break;
  case 17: VG_(transfer) (&tilegx->guest_r17, buf, dir, size, mod); break;
  case 18: VG_(transfer) (&tilegx->guest_r18, buf, dir, size, mod); break;
  case 19: VG_(transfer) (&tilegx->guest_r19, buf, dir, size, mod); break;
  case 20: VG_(transfer) (&tilegx->guest_r20, buf, dir, size, mod); break;
  case 21: VG_(transfer) (&tilegx->guest_r21, buf, dir, size, mod); break;
  case 22: VG_(transfer) (&tilegx->guest_r22, buf, dir, size, mod); break;
  case 23: VG_(transfer) (&tilegx->guest_r23, buf, dir, size, mod); break;
  case 24: VG_(transfer) (&tilegx->guest_r24, buf, dir, size, mod); break;
  case 25: VG_(transfer) (&tilegx->guest_r25, buf, dir, size, mod); break;
  case 26: VG_(transfer) (&tilegx->guest_r26, buf, dir, size, mod); break;
  case 27: VG_(transfer) (&tilegx->guest_r27, buf, dir, size, mod); break;
  case 28: VG_(transfer) (&tilegx->guest_r28, buf, dir, size, mod); break;
  case 29: VG_(transfer) (&tilegx->guest_r29, buf, dir, size, mod); break;
  case 30: VG_(transfer) (&tilegx->guest_r30, buf, dir, size, mod); break;
  case 31: VG_(transfer) (&tilegx->guest_r31, buf, dir, size, mod); break;
  case 32: VG_(transfer) (&tilegx->guest_r32, buf, dir, size, mod); break;
  case 33: VG_(transfer) (&tilegx->guest_r33, buf, dir, size, mod); break;
  case 34: VG_(transfer) (&tilegx->guest_r34, buf, dir, size, mod); break;
  case 35: VG_(transfer) (&tilegx->guest_r35, buf, dir, size, mod); break;
  case 36: VG_(transfer) (&tilegx->guest_r36, buf, dir, size, mod); break;
  case 37: VG_(transfer) (&tilegx->guest_r37, buf, dir, size, mod); break;
  case 38: VG_(transfer) (&tilegx->guest_r38, buf, dir, size, mod); break;
  case 39: VG_(transfer) (&tilegx->guest_r39, buf, dir, size, mod); break;
  case 40: VG_(transfer) (&tilegx->guest_r40, buf, dir, size, mod); break;
  case 41: VG_(transfer) (&tilegx->guest_r41, buf, dir, size, mod); break;
  case 42: VG_(transfer) (&tilegx->guest_r42, buf, dir, size, mod); break;
  case 43: VG_(transfer) (&tilegx->guest_r43, buf, dir, size, mod); break;
  case 44: VG_(transfer) (&tilegx->guest_r44, buf, dir, size, mod); break;
  case 45: VG_(transfer) (&tilegx->guest_r45, buf, dir, size, mod); break;
  case 46: VG_(transfer) (&tilegx->guest_r46, buf, dir, size, mod); break;
  case 47: VG_(transfer) (&tilegx->guest_r47, buf, dir, size, mod); break;
  case 48: VG_(transfer) (&tilegx->guest_r48, buf, dir, size, mod); break;
  case 49: VG_(transfer) (&tilegx->guest_r49, buf, dir, size, mod); break;
  case 50: VG_(transfer) (&tilegx->guest_r50, buf, dir, size, mod); break;
  case 51: VG_(transfer) (&tilegx->guest_r51, buf, dir, size, mod); break;
  case 52: VG_(transfer) (&tilegx->guest_r52, buf, dir, size, mod); break;
  case 53: VG_(transfer) (&tilegx->guest_r53, buf, dir, size, mod); break;
  case 54: VG_(transfer) (&tilegx->guest_r54, buf, dir, size, mod); break;
  case 55: VG_(transfer) (&tilegx->guest_r55, buf, dir, size, mod); break;
  case 56: VG_(transfer) (&tilegx->guest_r56, buf, dir, size, mod); break;
  case 57: VG_(transfer) (&tilegx->guest_r57, buf, dir, size, mod); break;
  case 58: VG_(transfer) (&tilegx->guest_r58, buf, dir, size, mod); break;
  case 59: VG_(transfer) (&tilegx->guest_r59, buf, dir, size, mod); break;
  case 60: VG_(transfer) (&tilegx->guest_r60, buf, dir, size, mod); break;
  case 61: VG_(transfer) (&tilegx->guest_r61, buf, dir, size, mod); break;
  case 62: VG_(transfer) (&tilegx->guest_r62, buf, dir, size, mod); break;
  case 63: VG_(transfer) (&tilegx->guest_r63, buf, dir, size, mod); break;
  case 64: VG_(transfer) (&tilegx->guest_pc,  buf, dir, size, mod); break;

  default: VG_(printf)("regno: %d\n", regno); vg_assert(0);
  }
}

static
const char* target_xml ( Bool shadow_mode )
{
  if (shadow_mode) {
    return "tilegx-linux-valgrind.xml";
  } else {
    return "tilegx-linux.xml";
  }
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
  VexGuestTILEGXState* tilegx = (VexGuestTILEGXState*)&tst->arch.vex;
  // tilegx dtv location similar to mips
  return (CORE_ADDR**)((CORE_ADDR)tilegx->guest_r53
                        - 0x7000 - sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
  num_regs,
  regs,
  54, //sp = r54, which is register offset 54 in regs
  transfer_register,
  get_pc,
  set_pc,
  "tilegx",
  target_xml,
  target_get_dtv
};

void tilegx_init_architecture ( struct valgrind_target_ops *target )
{
  *target = low_target;
  set_register_cache (regs, num_regs);
  gdbserver_expedite_regs = expedite_regs;
}
