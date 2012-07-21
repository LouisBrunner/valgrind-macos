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

#include "pub_core_aspacemgr.h"
#include "pub_tool_machine.h"
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
  { "lo", 1056, 32 },
  { "hi", 1088, 32 },
  { "badvaddr", 1120, 32 },
  { "cause", 1152, 32 },
  { "pc", 1184, 32 },
  { "f0", 1216, 32 },
  { "f1", 1248, 32 },
  { "f2", 1280, 32 },
  { "f3", 1312, 32 },
  { "f4", 1344, 32 },
  { "f5", 1376, 32 },
  { "f6", 1408, 32 },
  { "f7", 1440, 32 },
  { "f8", 1472, 32 },
  { "f9", 1504, 32 },
  { "f10", 1536, 32 },
  { "f11", 1568, 32 },
  { "f12", 1600, 32 },
  { "f13", 1632, 32 },
  { "f14", 1664, 32 },
  { "f15", 1696, 32 },
  { "f16", 1728, 32 },
  { "f17", 1760, 32 },
  { "f18", 1792, 32 },
  { "f19", 1824, 32 },
  { "f20", 1856, 32 },
  { "f21", 1888, 32 },
  { "f22", 1920, 32 },
  { "f23", 1952, 32 },
  { "f24", 1984, 32 },
  { "f25", 2016, 32 },
  { "f26", 2048, 32 },
  { "f27", 2080, 32 },
  { "f28", 2112, 32 },
  { "f29", 2144, 32 },
  { "f30", 2176, 32 },
  { "f31", 2208, 32 },
  { "fcsr", 2240, 32 },
  { "fir", 2272, 32 },
  { "restart", 2304, 32 },
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
   case 38: VG_(transfer) (&mips1->guest_f0,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&mips1->guest_f1,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&mips1->guest_f2,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&mips1->guest_f3,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&mips1->guest_f4,  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&mips1->guest_f5,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&mips1->guest_f6,  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&mips1->guest_f7, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&mips1->guest_f8, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&mips1->guest_f9, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&mips1->guest_f10, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&mips1->guest_f11, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&mips1->guest_f12, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&mips1->guest_f13,  buf, dir, size, mod); break;
   case 52: VG_(transfer) (&mips1->guest_f14,  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&mips1->guest_f15,  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&mips1->guest_f16,  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&mips1->guest_f17,  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&mips1->guest_f18,  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&mips1->guest_f19, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&mips1->guest_f20, buf, dir, size, mod); break;
   case 59: VG_(transfer) (&mips1->guest_f21, buf, dir, size, mod); break;
   case 60: VG_(transfer) (&mips1->guest_f22, buf, dir, size, mod); break;
   case 61: VG_(transfer) (&mips1->guest_f23, buf, dir, size, mod); break;
   case 62: VG_(transfer) (&mips1->guest_f24,  buf, dir, size, mod); break;
   case 63: VG_(transfer) (&mips1->guest_f25,  buf, dir, size, mod); break;
   case 64: VG_(transfer) (&mips1->guest_f26,  buf, dir, size, mod); break;
   case 65: VG_(transfer) (&mips1->guest_f27,  buf, dir, size, mod); break;
   case 66: VG_(transfer) (&mips1->guest_f28,  buf, dir, size, mod); break;
   case 67: VG_(transfer) (&mips1->guest_f29,  buf, dir, size, mod); break;
   case 68: VG_(transfer) (&mips1->guest_f30, buf, dir, size, mod); break;
   case 69: VG_(transfer) (&mips1->guest_f31, buf, dir, size, mod); break;
   case 70: VG_(transfer) (&mips1->guest_FCSR, buf, dir, size, mod); break;
   case 71: VG_(transfer) (&mips1->guest_FIR, buf, dir, size, mod); break;
   case 72: *mod = False; break; // GDBTD???? VEX{ "restart", 2304, 32 },
   default: VG_(printf)("regno: %d\n", regno); vg_assert(0);
   }
}

static
char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "mips-linux-valgrind.xml";
   } else {
      return "mips-linux.xml";
   }  
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   29, //sp = r29, which is register offset 29 in regs
   transfer_register,
   get_pc,
   set_pc,
   "mips",
   target_xml
};

void mips32_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
