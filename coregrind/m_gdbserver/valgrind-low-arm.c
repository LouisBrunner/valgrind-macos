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
#include "pub_core_debuginfo.h"

#include "valgrind_low.h"

#include "libvex_guest_arm.h"

struct reg regs[] = {
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
  { "sp", 416, 32 },
  { "lr", 448, 32 },
  { "pc", 480, 32 },
  { "", 512, 0 }, // It seems these entries are needed 
  { "", 512, 0 }, // as previous versions of arm <-> gdb placed 
  { "", 512, 0 }, // some floating point registers here. So, cpsr 
  { "", 512, 0 }, // must be register 25.
  { "", 512, 0 },
  { "", 512, 0 },
  { "", 512, 0 },
  { "", 512, 0 },
  { "", 512, 0 },
  { "cpsr", 512, 32 },
  { "d0", 544, 64 },
  { "d1", 608, 64 },
  { "d2", 672, 64 },
  { "d3", 736, 64 },
  { "d4", 800, 64 },
  { "d5", 864, 64 },
  { "d6", 928, 64 },
  { "d7", 992, 64 },
  { "d8", 1056, 64 },
  { "d9", 1120, 64 },
  { "d10", 1184, 64 },
  { "d11", 1248, 64 },
  { "d12", 1312, 64 },
  { "d13", 1376, 64 },
  { "d14", 1440, 64 },
  { "d15", 1504, 64 },
  { "d16", 1568, 64 },
  { "d17", 1632, 64 },
  { "d18", 1696, 64 },
  { "d19", 1760, 64 },
  { "d20", 1824, 64 },
  { "d21", 1888, 64 },
  { "d22", 1952, 64 },
  { "d23", 2016, 64 },
  { "d24", 2080, 64 },
  { "d25", 2144, 64 },
  { "d26", 2208, 64 },
  { "d27", 2272, 64 },
  { "d28", 2336, 64 },
  { "d29", 2400, 64 },
  { "d30", 2464, 64 },
  { "d31", 2528, 64 },
  { "fpscr", 2592, 32 }
};
static const char *expedite_regs[] = { "r11", "sp", "pc", 0 };
#define num_regs (sizeof (regs) / sizeof (regs[0]))

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

Addr thumb_pc (Addr pc)
{
   // If the thumb bit (bit 0) is already set, we trust it.
   if (pc & 1) {
      dlog (1, "%p = thumb (bit0 is set)\n", C2v (pc));
      return pc;
   }

   // Here, bit 0 is not set.
   // For a pc aligned on 4 bytes, we have to use the debug
   // info to determine the thumb-ness.
   // else (aligned on 2 bytes), we trust this is a thumb
   // address and we set the thumb bit.

   if (pc & 2) {
      dlog (1, "bit0 not set, bit1 set => %p = thumb\n", C2v (pc));
      return pc | 1;
   }

   // pc aligned on 4 bytes. We need to use debug info.
   {
      Char fnname[200]; // ??? max size
      Addr entrypoint;
      Addr ptoc; // unused but needed.
      // If this is a thumb instruction, we need to ask
      // the debug info with the bit0 set
      // (why can't debug info do that for us ???)
      // (why if this is a 4 bytes thumb instruction ???)
      if (VG_(get_fnname_raw) (pc | 1, fnname, 200)) {
         if (VG_(lookup_symbol_SLOW)( "*", fnname, &entrypoint, &ptoc )) {
            dlog (1, "fnname %s lookupsym %p => %p %s.\n",
                  fnname, C2v(entrypoint), C2v(pc),
                  (entrypoint & 1 ? "thumb" : "arm"));
            if (entrypoint & 1)
               return pc | 1;
            else
               return pc;
            
         } else {
            dlog (1, "%p fnname %s lookupsym failed?. Assume arm\n",
                  C2v (pc), fnname);
            return pc;
         }
      } else {
         // Can't find function name. We assume this is arm
         dlog (1, "%p unknown fnname?. Assume arm\n", C2v (pc));
         return pc;
      }
   }
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

   VexGuestARMState* arm = (VexGuestARMState*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&arm->guest_R0,   buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&arm->guest_R1,   buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&arm->guest_R2,   buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&arm->guest_R3,   buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&arm->guest_R4,   buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&arm->guest_R5,   buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&arm->guest_R6,   buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&arm->guest_R7,   buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&arm->guest_R8,   buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&arm->guest_R9,   buf, dir, size, mod); break;
   case 10: VG_(transfer) (&arm->guest_R10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&arm->guest_R11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&arm->guest_R12,  buf, dir, size, mod); break;
   case 13: VG_(transfer) (&arm->guest_R13,  buf, dir, size, mod); break;
   case 14: VG_(transfer) (&arm->guest_R14,  buf, dir, size, mod); break;
   case 15: { 
      VG_(transfer) (&arm->guest_R15T, buf, dir, size, mod);
      if (dir == gdbserver_to_valgrind && *mod) {
         // If gdb is changing the PC, we have to set the thumb bit
         // if needed.
         arm->guest_R15T = thumb_pc(arm->guest_R15T);
      }
      break;
   }
   case 16:
   case 17:
   case 18:
   case 19:
   case 20: /* 9 "empty registers". See struct reg regs above. */
   case 21:
   case 22:
   case 23:
   case 24: *mod = False; break;
   case 25: {
      UInt cpsr = LibVEX_GuestARM_get_cpsr (arm);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&cpsr, buf, dir, size, mod); 
      } else {
#      if 0
         UInt newcpsr;
         VG_(transfer) (&newcpsr, buf, dir, size, mod);
         *mod = newcpsr != cpsr;
         // GDBTD ???? see FIXME in guest_arm_helpers.c
         LibVEX_GuestARM_put_flags (newcpsr, arm);
#      else
         *mod = False;
#      endif
      }
      break;
   }
   case 26: VG_(transfer) (&arm->guest_D0,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&arm->guest_D1,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&arm->guest_D2,  buf, dir, size, mod); break;
   case 29: VG_(transfer) (&arm->guest_D3,  buf, dir, size, mod); break;
   case 30: VG_(transfer) (&arm->guest_D4,  buf, dir, size, mod); break;
   case 31: VG_(transfer) (&arm->guest_D5,  buf, dir, size, mod); break;
   case 32: VG_(transfer) (&arm->guest_D6,  buf, dir, size, mod); break;
   case 33: VG_(transfer) (&arm->guest_D7,  buf, dir, size, mod); break;
   case 34: VG_(transfer) (&arm->guest_D8,  buf, dir, size, mod); break;
   case 35: VG_(transfer) (&arm->guest_D9,  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&arm->guest_D10, buf, dir, size, mod); break;
   case 37: VG_(transfer) (&arm->guest_D11, buf, dir, size, mod); break;
   case 38: VG_(transfer) (&arm->guest_D12, buf, dir, size, mod); break;
   case 39: VG_(transfer) (&arm->guest_D13, buf, dir, size, mod); break;
   case 40: VG_(transfer) (&arm->guest_D14, buf, dir, size, mod); break;
   case 41: VG_(transfer) (&arm->guest_D15, buf, dir, size, mod); break;
   case 42: VG_(transfer) (&arm->guest_D16, buf, dir, size, mod); break;
   case 43: VG_(transfer) (&arm->guest_D17, buf, dir, size, mod); break;
   case 44: VG_(transfer) (&arm->guest_D18, buf, dir, size, mod); break;
   case 45: VG_(transfer) (&arm->guest_D19, buf, dir, size, mod); break;
   case 46: VG_(transfer) (&arm->guest_D20, buf, dir, size, mod); break;
   case 47: VG_(transfer) (&arm->guest_D21, buf, dir, size, mod); break;
   case 48: VG_(transfer) (&arm->guest_D22, buf, dir, size, mod); break;
   case 49: VG_(transfer) (&arm->guest_D23, buf, dir, size, mod); break;
   case 50: VG_(transfer) (&arm->guest_D24, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&arm->guest_D25, buf, dir, size, mod); break;
   case 52: VG_(transfer) (&arm->guest_D26, buf, dir, size, mod); break;
   case 53: VG_(transfer) (&arm->guest_D27, buf, dir, size, mod); break;
   case 54: VG_(transfer) (&arm->guest_D28, buf, dir, size, mod); break;
   case 55: VG_(transfer) (&arm->guest_D29, buf, dir, size, mod); break;
   case 56: VG_(transfer) (&arm->guest_D30, buf, dir, size, mod); break;
   case 57: VG_(transfer) (&arm->guest_D31, buf, dir, size, mod); break;
   case 58: VG_(transfer) (&arm->guest_FPSCR, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   13, //SP
   transfer_register,
   get_pc,
   set_pc,
   "arm",
   "arm-with-vfpv3.xml",
   "arm-with-vfpv3-valgrind.xml"
};

void arm_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
