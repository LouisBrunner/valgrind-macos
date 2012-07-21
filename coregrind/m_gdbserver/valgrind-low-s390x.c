/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2011
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

#include "libvex_guest_s390x.h"

static struct reg regs[] = {
  { "pswm", 0, 64 },
  { "pswa", 64, 64 },
  { "r0", 128, 64 },
  { "r1", 192, 64 },
  { "r2", 256, 64 },
  { "r3", 320, 64 },
  { "r4", 384, 64 },
  { "r5", 448, 64 },
  { "r6", 512, 64 },
  { "r7", 576, 64 },
  { "r8", 640, 64 },
  { "r9", 704, 64 },
  { "r10", 768, 64 },
  { "r11", 832, 64 },
  { "r12", 896, 64 },
  { "r13", 960, 64 },
  { "r14", 1024, 64 },
  { "r15", 1088, 64 },
  { "acr0", 1152, 32 },
  { "acr1", 1184, 32 },
  { "acr2", 1216, 32 },
  { "acr3", 1248, 32 },
  { "acr4", 1280, 32 },
  { "acr5", 1312, 32 },
  { "acr6", 1344, 32 },
  { "acr7", 1376, 32 },
  { "acr8", 1408, 32 },
  { "acr9", 1440, 32 },
  { "acr10", 1472, 32 },
  { "acr11", 1504, 32 },
  { "acr12", 1536, 32 },
  { "acr13", 1568, 32 },
  { "acr14", 1600, 32 },
  { "acr15", 1632, 32 },
  { "fpc", 1664, 32 },
  { "f0", 1696, 64 },
  { "f1", 1760, 64 },
  { "f2", 1824, 64 },
  { "f3", 1888, 64 },
  { "f4", 1952, 64 },
  { "f5", 2016, 64 },
  { "f6", 2080, 64 },
  { "f7", 2144, 64 },
  { "f8", 2208, 64 },
  { "f9", 2272, 64 },
  { "f10", 2336, 64 },
  { "f11", 2400, 64 },
  { "f12", 2464, 64 },
  { "f13", 2528, 64 },
  { "f14", 2592, 64 },
  { "f15", 2656, 64 },
  { "orig_r2", 2720, 64 },
};
static const char *expedite_regs[] = { "r14", "r15", "pswa", 0 };
#define num_regs (sizeof (regs) / sizeof (regs[0]))

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("pswa", &pc);
   
   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   Bool mod;
   supply_register_by_name ("pswa", &newpc, &mod);
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

   VexGuestS390XState* s390x = (VexGuestS390XState*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  *mod = False; break; //GDBTD??? { "pswm", 0, 64 },  
   case 1:  VG_(transfer) (&s390x->guest_IA,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&s390x->guest_r0,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&s390x->guest_r1,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&s390x->guest_r2,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&s390x->guest_r3,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&s390x->guest_r4,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&s390x->guest_r5,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&s390x->guest_r6,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&s390x->guest_r7,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&s390x->guest_r8,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&s390x->guest_r9,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&s390x->guest_r10, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&s390x->guest_r11, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&s390x->guest_r12, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&s390x->guest_r13, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&s390x->guest_r14, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&s390x->guest_r15, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&s390x->guest_a0,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&s390x->guest_a1,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&s390x->guest_a2,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&s390x->guest_a3,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&s390x->guest_a4,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&s390x->guest_a5,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&s390x->guest_a6,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&s390x->guest_a7,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&s390x->guest_a8,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&s390x->guest_a9,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&s390x->guest_a10, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&s390x->guest_a11, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&s390x->guest_a12, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&s390x->guest_a13, buf, dir, size, mod); break;
   case 32: VG_(transfer) (&s390x->guest_a14, buf, dir, size, mod); break;
   case 33: VG_(transfer) (&s390x->guest_a15, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&s390x->guest_fpc, buf, dir, size, mod); break;
   case 35: VG_(transfer) (&s390x->guest_f0,  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&s390x->guest_f1,  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&s390x->guest_f2,  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&s390x->guest_f3,  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&s390x->guest_f4,  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&s390x->guest_f5,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&s390x->guest_f6,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&s390x->guest_f7,  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&s390x->guest_f8,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&s390x->guest_f9,  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&s390x->guest_f10, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&s390x->guest_f11, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&s390x->guest_f12, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&s390x->guest_f13, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&s390x->guest_f14, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&s390x->guest_f15, buf, dir, size, mod); break;
   case 51:  *mod = False; break; //GDBTD??? { "orig_r2", 0, 64 },  
   default: vg_assert(0);
   }
}

static
char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "s390x-generic-valgrind.xml";
   } else {
      return "s390x-generic.xml";
   }  
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   17, //sp = r15, which is register offset 17 in regs
   transfer_register,
   get_pc,
   set_pc,
   "s390x",
   target_xml
};

void s390x_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
