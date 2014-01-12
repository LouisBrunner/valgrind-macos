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

#include "libvex_guest_arm64.h"

//ZZ static struct reg regs[] = {
//ZZ   { "r0", 0, 32 },
//ZZ   { "r1", 32, 32 },
//ZZ   { "r2", 64, 32 },
//ZZ   { "r3", 96, 32 },
//ZZ   { "r4", 128, 32 },
//ZZ   { "r5", 160, 32 },
//ZZ   { "r6", 192, 32 },
//ZZ   { "r7", 224, 32 },
//ZZ   { "r8", 256, 32 },
//ZZ   { "r9", 288, 32 },
//ZZ   { "r10", 320, 32 },
//ZZ   { "r11", 352, 32 },
//ZZ   { "r12", 384, 32 },
//ZZ   { "sp", 416, 32 },
//ZZ   { "lr", 448, 32 },
//ZZ   { "pc", 480, 32 },
//ZZ   { "", 512, 0 }, // It seems these entries are needed 
//ZZ   { "", 512, 0 }, // as previous versions of arm <-> gdb placed 
//ZZ   { "", 512, 0 }, // some floating point registers here. So, cpsr 
//ZZ   { "", 512, 0 }, // must be register 25.
//ZZ   { "", 512, 0 },
//ZZ   { "", 512, 0 },
//ZZ   { "", 512, 0 },
//ZZ   { "", 512, 0 },
//ZZ   { "", 512, 0 },
//ZZ   { "cpsr", 512, 32 },
//ZZ   { "d0", 544, 64 },
//ZZ   { "d1", 608, 64 },
//ZZ   { "d2", 672, 64 },
//ZZ   { "d3", 736, 64 },
//ZZ   { "d4", 800, 64 },
//ZZ   { "d5", 864, 64 },
//ZZ   { "d6", 928, 64 },
//ZZ   { "d7", 992, 64 },
//ZZ   { "d8", 1056, 64 },
//ZZ   { "d9", 1120, 64 },
//ZZ   { "d10", 1184, 64 },
//ZZ   { "d11", 1248, 64 },
//ZZ   { "d12", 1312, 64 },
//ZZ   { "d13", 1376, 64 },
//ZZ   { "d14", 1440, 64 },
//ZZ   { "d15", 1504, 64 },
//ZZ   { "d16", 1568, 64 },
//ZZ   { "d17", 1632, 64 },
//ZZ   { "d18", 1696, 64 },
//ZZ   { "d19", 1760, 64 },
//ZZ   { "d20", 1824, 64 },
//ZZ   { "d21", 1888, 64 },
//ZZ   { "d22", 1952, 64 },
//ZZ   { "d23", 2016, 64 },
//ZZ   { "d24", 2080, 64 },
//ZZ   { "d25", 2144, 64 },
//ZZ   { "d26", 2208, 64 },
//ZZ   { "d27", 2272, 64 },
//ZZ   { "d28", 2336, 64 },
//ZZ   { "d29", 2400, 64 },
//ZZ   { "d30", 2464, 64 },
//ZZ   { "d31", 2528, 64 },
//ZZ   { "fpscr", 2592, 32 }
//ZZ };
//ZZ static const char *expedite_regs[] = { "r11", "sp", "pc", 0 };
//ZZ #define num_regs (sizeof (regs) / sizeof (regs[0]))
//ZZ 
//ZZ static
//ZZ CORE_ADDR get_pc (void)
//ZZ {
//ZZ    unsigned long pc;
//ZZ 
//ZZ    collect_register_by_name ("pc", &pc);
//ZZ    
//ZZ    dlog(1, "stop pc is %p\n", (void *) pc);
//ZZ    return pc;
//ZZ }
//ZZ 
//ZZ static
//ZZ void set_pc (CORE_ADDR newpc)
//ZZ {
//ZZ    Bool mod;
//ZZ    supply_register_by_name ("pc", &newpc, &mod);
//ZZ    if (mod)
//ZZ       dlog(1, "set pc to %p\n", C2v (newpc));
//ZZ    else
//ZZ       dlog(1, "set pc not changed %p\n", C2v (newpc));
//ZZ }
//ZZ 
//ZZ Addr thumb_pc (Addr pc)
//ZZ {
//ZZ    // If the thumb bit (bit 0) is already set, we trust it.
//ZZ    if (pc & 1) {
//ZZ       dlog (1, "%p = thumb (bit0 is set)\n", C2v (pc));
//ZZ       return pc;
//ZZ    }
//ZZ 
//ZZ    // Here, bit 0 is not set.
//ZZ    // For a pc aligned on 4 bytes, we have to use the debug
//ZZ    // info to determine the thumb-ness.
//ZZ    // else (aligned on 2 bytes), we trust this is a thumb
//ZZ    // address and we set the thumb bit.
//ZZ 
//ZZ    if (pc & 2) {
//ZZ       dlog (1, "bit0 not set, bit1 set => %p = thumb\n", C2v (pc));
//ZZ       return pc | 1;
//ZZ    }
//ZZ 
//ZZ    // pc aligned on 4 bytes. We need to use debug info.
//ZZ    {
//ZZ       HChar fnname[200]; // ??? max size
//ZZ       Addr entrypoint;
//ZZ       Addr ptoc; // unused but needed.
//ZZ       // If this is a thumb instruction, we need to ask
//ZZ       // the debug info with the bit0 set
//ZZ       // (why can't debug info do that for us ???)
//ZZ       // (why if this is a 4 bytes thumb instruction ???)
//ZZ       if (VG_(get_fnname_raw) (pc | 1, fnname, 200)) {
//ZZ          if (VG_(lookup_symbol_SLOW)( "*", fnname, &entrypoint, &ptoc )) {
//ZZ             dlog (1, "fnname %s lookupsym %p => %p %s.\n",
//ZZ                   fnname, C2v(entrypoint), C2v(pc),
//ZZ                   (entrypoint & 1 ? "thumb" : "arm"));
//ZZ             if (entrypoint & 1)
//ZZ                return pc | 1;
//ZZ             else
//ZZ                return pc;
//ZZ             
//ZZ          } else {
//ZZ             dlog (1, "%p fnname %s lookupsym failed?. Assume arm\n",
//ZZ                   C2v (pc), fnname);
//ZZ             return pc;
//ZZ          }
//ZZ       } else {
//ZZ          // Can't find function name. We assume this is arm
//ZZ          dlog (1, "%p unknown fnname?. Assume arm\n", C2v (pc));
//ZZ          return pc;
//ZZ       }
//ZZ    }
//ZZ }
//ZZ 
//ZZ /* store registers in the guest state (gdbserver_to_valgrind)
//ZZ    or fetch register from the guest state (valgrind_to_gdbserver). */
//ZZ static
//ZZ void transfer_register (ThreadId tid, int abs_regno, void * buf,
//ZZ                         transfer_direction dir, int size, Bool *mod)
//ZZ {
//ZZ    ThreadState* tst = VG_(get_ThreadState)(tid);
//ZZ    int set = abs_regno / num_regs;
//ZZ    int regno = abs_regno % num_regs;
//ZZ    *mod = False;
//ZZ 
//ZZ    VexGuestARMState* arm = (VexGuestARMState*) get_arch (set, tst);
//ZZ 
//ZZ    switch (regno) { 
//ZZ    // numbers here have to match the order of regs above
//ZZ    // Attention: gdb order does not match valgrind order.
//ZZ    case 0:  VG_(transfer) (&arm->guest_R0,   buf, dir, size, mod); break;
//ZZ    case 1:  VG_(transfer) (&arm->guest_R1,   buf, dir, size, mod); break;
//ZZ    case 2:  VG_(transfer) (&arm->guest_R2,   buf, dir, size, mod); break;
//ZZ    case 3:  VG_(transfer) (&arm->guest_R3,   buf, dir, size, mod); break;
//ZZ    case 4:  VG_(transfer) (&arm->guest_R4,   buf, dir, size, mod); break;
//ZZ    case 5:  VG_(transfer) (&arm->guest_R5,   buf, dir, size, mod); break;
//ZZ    case 6:  VG_(transfer) (&arm->guest_R6,   buf, dir, size, mod); break;
//ZZ    case 7:  VG_(transfer) (&arm->guest_R7,   buf, dir, size, mod); break;
//ZZ    case 8:  VG_(transfer) (&arm->guest_R8,   buf, dir, size, mod); break;
//ZZ    case 9:  VG_(transfer) (&arm->guest_R9,   buf, dir, size, mod); break;
//ZZ    case 10: VG_(transfer) (&arm->guest_R10,  buf, dir, size, mod); break;
//ZZ    case 11: VG_(transfer) (&arm->guest_R11,  buf, dir, size, mod); break;
//ZZ    case 12: VG_(transfer) (&arm->guest_R12,  buf, dir, size, mod); break;
//ZZ    case 13: VG_(transfer) (&arm->guest_R13,  buf, dir, size, mod); break;
//ZZ    case 14: VG_(transfer) (&arm->guest_R14,  buf, dir, size, mod); break;
//ZZ    case 15: { 
//ZZ       VG_(transfer) (&arm->guest_R15T, buf, dir, size, mod);
//ZZ       if (dir == gdbserver_to_valgrind && *mod) {
//ZZ          // If gdb is changing the PC, we have to set the thumb bit
//ZZ          // if needed.
//ZZ          arm->guest_R15T = thumb_pc(arm->guest_R15T);
//ZZ       }
//ZZ       break;
//ZZ    }
//ZZ    case 16:
//ZZ    case 17:
//ZZ    case 18:
//ZZ    case 19:
//ZZ    case 20: /* 9 "empty registers". See struct reg regs above. */
//ZZ    case 21:
//ZZ    case 22:
//ZZ    case 23:
//ZZ    case 24: *mod = False; break;
//ZZ    case 25: {
//ZZ       UInt cpsr = LibVEX_GuestARM_get_cpsr (arm);
//ZZ       if (dir == valgrind_to_gdbserver) {
//ZZ          VG_(transfer) (&cpsr, buf, dir, size, mod); 
//ZZ       } else {
//ZZ #      if 0
//ZZ          UInt newcpsr;
//ZZ          VG_(transfer) (&newcpsr, buf, dir, size, mod);
//ZZ          *mod = newcpsr != cpsr;
//ZZ          // GDBTD ???? see FIXME in guest_arm_helpers.c
//ZZ          LibVEX_GuestARM_put_flags (newcpsr, arm);
//ZZ #      else
//ZZ          *mod = False;
//ZZ #      endif
//ZZ       }
//ZZ       break;
//ZZ    }
//ZZ    case 26: VG_(transfer) (&arm->guest_D0,  buf, dir, size, mod); break;
//ZZ    case 27: VG_(transfer) (&arm->guest_D1,  buf, dir, size, mod); break;
//ZZ    case 28: VG_(transfer) (&arm->guest_D2,  buf, dir, size, mod); break;
//ZZ    case 29: VG_(transfer) (&arm->guest_D3,  buf, dir, size, mod); break;
//ZZ    case 30: VG_(transfer) (&arm->guest_D4,  buf, dir, size, mod); break;
//ZZ    case 31: VG_(transfer) (&arm->guest_D5,  buf, dir, size, mod); break;
//ZZ    case 32: VG_(transfer) (&arm->guest_D6,  buf, dir, size, mod); break;
//ZZ    case 33: VG_(transfer) (&arm->guest_D7,  buf, dir, size, mod); break;
//ZZ    case 34: VG_(transfer) (&arm->guest_D8,  buf, dir, size, mod); break;
//ZZ    case 35: VG_(transfer) (&arm->guest_D9,  buf, dir, size, mod); break;
//ZZ    case 36: VG_(transfer) (&arm->guest_D10, buf, dir, size, mod); break;
//ZZ    case 37: VG_(transfer) (&arm->guest_D11, buf, dir, size, mod); break;
//ZZ    case 38: VG_(transfer) (&arm->guest_D12, buf, dir, size, mod); break;
//ZZ    case 39: VG_(transfer) (&arm->guest_D13, buf, dir, size, mod); break;
//ZZ    case 40: VG_(transfer) (&arm->guest_D14, buf, dir, size, mod); break;
//ZZ    case 41: VG_(transfer) (&arm->guest_D15, buf, dir, size, mod); break;
//ZZ    case 42: VG_(transfer) (&arm->guest_D16, buf, dir, size, mod); break;
//ZZ    case 43: VG_(transfer) (&arm->guest_D17, buf, dir, size, mod); break;
//ZZ    case 44: VG_(transfer) (&arm->guest_D18, buf, dir, size, mod); break;
//ZZ    case 45: VG_(transfer) (&arm->guest_D19, buf, dir, size, mod); break;
//ZZ    case 46: VG_(transfer) (&arm->guest_D20, buf, dir, size, mod); break;
//ZZ    case 47: VG_(transfer) (&arm->guest_D21, buf, dir, size, mod); break;
//ZZ    case 48: VG_(transfer) (&arm->guest_D22, buf, dir, size, mod); break;
//ZZ    case 49: VG_(transfer) (&arm->guest_D23, buf, dir, size, mod); break;
//ZZ    case 50: VG_(transfer) (&arm->guest_D24, buf, dir, size, mod); break;
//ZZ    case 51: VG_(transfer) (&arm->guest_D25, buf, dir, size, mod); break;
//ZZ    case 52: VG_(transfer) (&arm->guest_D26, buf, dir, size, mod); break;
//ZZ    case 53: VG_(transfer) (&arm->guest_D27, buf, dir, size, mod); break;
//ZZ    case 54: VG_(transfer) (&arm->guest_D28, buf, dir, size, mod); break;
//ZZ    case 55: VG_(transfer) (&arm->guest_D29, buf, dir, size, mod); break;
//ZZ    case 56: VG_(transfer) (&arm->guest_D30, buf, dir, size, mod); break;
//ZZ    case 57: VG_(transfer) (&arm->guest_D31, buf, dir, size, mod); break;
//ZZ    case 58: VG_(transfer) (&arm->guest_FPSCR, buf, dir, size, mod); break;
//ZZ    default: vg_assert(0);
//ZZ    }
//ZZ }
//ZZ 
//ZZ static
//ZZ const char* target_xml (Bool shadow_mode)
//ZZ {
//ZZ    if (shadow_mode) {
//ZZ       return "arm-with-vfpv3-valgrind.xml";
//ZZ    } else {
//ZZ       return "arm-with-vfpv3.xml";
//ZZ    }  
//ZZ }
//ZZ 
//ZZ static struct valgrind_target_ops low_target = {
//ZZ    num_regs,
//ZZ    regs,
//ZZ    13, //SP
//ZZ    transfer_register,
//ZZ    get_pc,
//ZZ    set_pc,
//ZZ    "arm",
//ZZ    target_xml
//ZZ };

void arm64_init_architecture (struct valgrind_target_ops *target)
{
  vg_assert(0); // IMPLEMENT ME
  //ZZ    *target = low_target;
  //ZZ    set_register_cache (regs, num_regs);
  //ZZ    gdbserver_expedite_regs = expedite_regs;
}
