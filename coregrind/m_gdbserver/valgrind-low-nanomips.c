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
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "server.h"
#include "target.h"
#include "regdef.h"
#include "regcache.h"

#include "pub_core_machine.h"
#include "pub_core_debuginfo.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h"

#include "valgrind_low.h"

#include "libvex_guest_mips32.h"

static struct reg regs[] = {
  { "r0", 0, 32 },
  { "r1", 32, 32 },
  { "r2", 64, 32 },
  { "r3", 96, 32 },
  { "r4", 128, 32 },
  { "r5", 160, 32 },
  { "r6", 192, 32 },
  { "r7", 224, 32 },
  { "r8", 256, 32 },
  { "r9", 288, 32 },
  { "r10", 320, 32 },
  { "r11", 352, 32 },
  { "r12", 384, 32 },
  { "r13", 416, 32 },
  { "r14", 448, 32 },
  { "r15", 480, 32 },
  { "r16", 512, 32 },
  { "r17", 544, 32 },
  { "r18", 576, 32 },
  { "r19", 608, 32 },
  { "r20", 640, 32 },
  { "r21", 672, 32 },
  { "r22", 704, 32 },
  { "r23", 736, 32 },
  { "r24", 768, 32 },
  { "r25", 800, 32 },
  { "r26", 832, 32 },
  { "r27", 864, 32 },
  { "r28", 896, 32 },
  { "r29", 928, 32 },
  { "r30", 960, 32 },
  { "r31", 992, 32 },
  { "status", 1024, 32 },
  { "badvaddr", 1120, 32 },
  { "cause", 1152, 32 },
  { "pc", 1184, 32 },
};

#define num_regs (sizeof (regs) / sizeof (regs[0]))

static const char *expedite_regs[] = { "r29", "pc", 0 };

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("pc", &pc);

   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   supply_register_by_name ("pc", &newpc);
}

/* These are the fields of 32 bit mips instructions. */
#define itype_op(x) (x >> 26)
#define itype_rs(x) ((x >> 21) & 0x1f)
#define itype_rt(x) ((x >> 16) & 0x1f)
#define rtype_funct(x) (x & 0x3f)

static inline UInt getUInt(UChar * p) __attribute__((unused));
/* Do a endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUInt(UChar * p)
{
   UInt w = 0;
#if defined (_MIPSEL)
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
#elif defined (_MIPSEB)
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
#endif
   return w;
}

/* store registers in the guest state (gdbserver_to_valgrind)
   or fetch register from the guest state (valgrind_to_gdbserver). */
static
void transfer_register (ThreadId tid, int abs_regno, void * buf,
                        transfer_direction dir, int size, Bool *mod)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   int set = abs_regno / num_regs;
   int regno = abs_regno % num_regs;
   *mod = False;

   VexGuestMIPS32State* mips1 = (VexGuestMIPS32State*) get_arch (set, tst);

   switch (regno) {
   case 0:  VG_(transfer) (&mips1->guest_r0,  buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&mips1->guest_r1,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&mips1->guest_r2,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&mips1->guest_r3,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&mips1->guest_r4,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&mips1->guest_r5,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&mips1->guest_r6,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&mips1->guest_r7,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&mips1->guest_r8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&mips1->guest_r9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&mips1->guest_r10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&mips1->guest_r11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&mips1->guest_r12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&mips1->guest_r13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&mips1->guest_r14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&mips1->guest_r15, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&mips1->guest_r16, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&mips1->guest_r17, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&mips1->guest_r18,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&mips1->guest_r19,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&mips1->guest_r20,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&mips1->guest_r21,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&mips1->guest_r22,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&mips1->guest_r23,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&mips1->guest_r24,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&mips1->guest_r25,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&mips1->guest_r26,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&mips1->guest_r27,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&mips1->guest_r28, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&mips1->guest_r29, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&mips1->guest_r30, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&mips1->guest_r31, buf, dir, size, mod); break;
   case 32: *mod = False; break; // GDBTD???? VEX { "status", 1024, 32 },
   case 33: VG_(transfer) (&mips1->guest_LO, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&mips1->guest_HI, buf, dir, size, mod); break;
   case 35: *mod = False; break; // GDBTD???? VEX { "badvaddr", 1120, 32 },
   case 36: *mod = False; break; // GDBTD???? VEX { "cause", 1152, 32 },
   case 37: VG_(transfer) (&mips1->guest_PC,  buf, dir, size, mod); break;
   case 72: *mod = False; break; // GDBTD???? VEX{ "restart", 2304, 32 },
   default: VG_(printf)("regno: %d\n", regno); vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "mips-linux-valgrind.xml";
   } else {
      return "mips-linux.xml";
   }
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestMIPS32State* mips32 = (VexGuestMIPS32State*)&tst->arch.vex;
   // Top of nanoMIPS tcbhead structure is located 0x7000 bytes before the value
   // of ULR. Dtv is the first of two pointers in tcbhead structure.
   return (CORE_ADDR**)((CORE_ADDR)mips32->guest_ULR
                        - 0x7000 - 2 * sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   29, //sp = r29, which is register offset 29 in regs
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "mips",
   target_xml,
   target_get_dtv
};

void nanomips_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
