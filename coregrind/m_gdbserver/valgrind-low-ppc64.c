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

#include "pub_core_machine.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h" 

#include "valgrind_low.h"

#include "libvex_guest_ppc64.h"

/* The PPC64 register layout with vs register support (Power 7 and beyond)
   consists of 64 VSR registers of size 128-bits.  The 32 floating point
   registers fp map to the upper 64-bits of vsr[0] to vsr[31].  The 32
   vr[0] to vr[31] registers of size 128-bits map to vsr[31] to vsr[63].  The
   lower 64-bits of the vsr[0] to vsr[31] registers are in the pseudo
   registers vs[0]h to vs[31]h registers.  These pseudo registers get printed
   by GDB but there are no instructions that directly access these registers.
   When GDB prints the vsr[0] to vsr[31] registers it combines the contents
   of the floating point registers fp[0] to fp[31] and its corresponding
   vs[0]h to vs[31]h registers to display the VSR contents.  The vsr[32]
   to vsr[63] contents are the same as the the vr[0] to vr[31] contents.

   GDB also prints fp[32] to fp[63].  These are simply the upper 64 bits of
   vsr[32] to vsr[63] however, these are not "real" floating point registers
   as none of the floating point instructions can access these registers.

    Register map.
                  MSB          IBM bit numbering           LSB
                   0                 63 64                127
         vsr[0]   |        fp[0]       |       vs[0]h        |
         vsr[1]   |        fp[1]       |       vs[1]h        |
         vsr[2]   |        fp[2]       |       vs[2]h        |
          ...
         vsr[31]  |        fp[31]      |       vs[31]h       |
         vsr[32]  |                  vr[0]                   |
         vsr[33]  |                  vr[1]                   |
          ...
         vsr[63]  |                  vr[31]                  |

    Note, not shown above are the fake fp[32] to fp[63] that GDB prints

   Valgrind has two shadow registers for each real register denoted with
   the suffix s1 and s2.   When printing the contents of the shadow registers,
   GDB does not explicitly print the shadow registers vsr[0] to vsr[63]. GDB
   prints the shadow register contents of the 32 floating point registers as
   fp[0]s1 to fp[31]s1 and fp[0]s2 to fp[31]s2.  The shadow register contents
   of vs[0]hs1 to vs[31]hs1 and vs[0]hs2 to vs[31]hs2 are also printed.  The
   user needs to construct the vsr[i]s1 shadow register contents by looking
   at fp[i]s1 for the upper 64-bits and vs[i]hs1 for the lower 64-bits.  The
   vsr[i]s2 shadow register contents are constructed similarly.

   GDB prints the 128-bit shadow register contents of the 32 vr registers as
   vr[0]s1 to vr[31]s1 and vr[0]s2 to vr[31]s2. These are also the value of the
   VSR shadow registers vsr[32]s1 to vsr[63]s1 and vsr[32]s2 to vsr[63]s2. */

static struct reg regs[] = {
  { "r0", 0, 64 },
  { "r1", 64, 64 },
  { "r2", 128, 64 },
  { "r3", 192, 64 },
  { "r4", 256, 64 },
  { "r5", 320, 64 },
  { "r6", 384, 64 },
  { "r7", 448, 64 },
  { "r8", 512, 64 },
  { "r9", 576, 64 },
  { "r10", 640, 64 },
  { "r11", 704, 64 },
  { "r12", 768, 64 },
  { "r13", 832, 64 },
  { "r14", 896, 64 },
  { "r15", 960, 64 },
  { "r16", 1024, 64 },
  { "r17", 1088, 64 },
  { "r18", 1152, 64 },
  { "r19", 1216, 64 },
  { "r20", 1280, 64 },
  { "r21", 1344, 64 },
  { "r22", 1408, 64 },
  { "r23", 1472, 64 },
  { "r24", 1536, 64 },
  { "r25", 1600, 64 },
  { "r26", 1664, 64 },
  { "r27", 1728, 64 },
  { "r28", 1792, 64 },
  { "r29", 1856, 64 },
  { "r30", 1920, 64 },
  { "r31", 1984, 64 },
  { "f0", 2048, 64 },
  { "f1", 2112, 64 },
  { "f2", 2176, 64 },
  { "f3", 2240, 64 },
  { "f4", 2304, 64 },
  { "f5", 2368, 64 },
  { "f6", 2432, 64 },
  { "f7", 2496, 64 },
  { "f8", 2560, 64 },
  { "f9", 2624, 64 },
  { "f10", 2688, 64 },
  { "f11", 2752, 64 },
  { "f12", 2816, 64 },
  { "f13", 2880, 64 },
  { "f14", 2944, 64 },
  { "f15", 3008, 64 },
  { "f16", 3072, 64 },
  { "f17", 3136, 64 },
  { "f18", 3200, 64 },
  { "f19", 3264, 64 },
  { "f20", 3328, 64 },
  { "f21", 3392, 64 },
  { "f22", 3456, 64 },
  { "f23", 3520, 64 },
  { "f24", 3584, 64 },
  { "f25", 3648, 64 },
  { "f26", 3712, 64 },
  { "f27", 3776, 64 },
  { "f28", 3840, 64 },
  { "f29", 3904, 64 },
  { "f30", 3968, 64 },
  { "f31", 4032, 64 },
  { "pc", 4096, 64 },
  { "msr", 4160, 64 },
  { "cr", 4224, 32 },
  { "lr", 4256, 64 },
  { "ctr", 4320, 64 },
  { "xer", 4384, 32 },
  { "fpscr", 4416, 32 },
  { "orig_r3", 4448, 64 },
  { "trap", 4512, 64 },
  { "vr0", 4576, 128 },
  { "vr1", 4704, 128 },
  { "vr2", 4832, 128 },
  { "vr3", 4960, 128 },
  { "vr4", 5088, 128 },
  { "vr5", 5216, 128 },
  { "vr6", 5344, 128 },
  { "vr7", 5472, 128 },
  { "vr8", 5600, 128 },
  { "vr9", 5728, 128 },
  { "vr10", 5856, 128 },
  { "vr11", 5984, 128 },
  { "vr12", 6112, 128 },
  { "vr13", 6240, 128 },
  { "vr14", 6368, 128 },
  { "vr15", 6496, 128 },
  { "vr16", 6624, 128 },
  { "vr17", 6752, 128 },
  { "vr18", 6880, 128 },
  { "vr19", 7008, 128 },
  { "vr20", 7136, 128 },
  { "vr21", 7264, 128 },
  { "vr22", 7392, 128 },
  { "vr23", 7520, 128 },
  { "vr24", 7648, 128 },
  { "vr25", 7776, 128 },
  { "vr26", 7904, 128 },
  { "vr27", 8032, 128 },
  { "vr28", 8160, 128 },
  { "vr29", 8288, 128 },
  { "vr30", 8416, 128 },
  { "vr31", 8544, 128 },
  { "vscr", 8672, 32 },
  { "vrsave", 8704, 32 },
  { "vs0h", 8736, 64 },
  { "vs1h", 8800, 64 },
  { "vs2h", 8864, 64 },
  { "vs3h", 8928, 64 },
  { "vs4h", 8992, 64 },
  { "vs5h", 9056, 64 },
  { "vs6h", 9120, 64 },
  { "vs7h", 9184, 64 },
  { "vs8h", 9248, 64 },
  { "vs9h", 9312, 64 },
  { "vs10h", 9376, 64 },
  { "vs11h", 9440, 64 },
  { "vs12h", 9504, 64 },
  { "vs13h", 9568, 64 },
  { "vs14h", 9632, 64 },
  { "vs15h", 9696, 64 },
  { "vs16h", 9760, 64 },
  { "vs17h", 9824, 64 },
  { "vs18h", 9888, 64 },
  { "vs19h", 9952, 64 },
  { "vs20h", 10016, 64 },
  { "vs21h", 10080, 64 },
  { "vs22h", 10144, 64 },
  { "vs23h", 10208, 64 },
  { "vs24h", 10272, 64 },
  { "vs25h", 10336, 64 },
  { "vs26h", 10400, 64 },
  { "vs27h", 10464, 64 },
  { "vs28h", 10528, 64 },
  { "vs29h", 10592, 64 },
  { "vs30h", 10656, 64 },
  { "vs31h", 10720, 64 },
};

static const char *expedite_regs[] = { "r1", "pc", 0 };
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
   supply_register_by_name ("pc", &newpc);
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
   int low_offset, high_offset;

   *mod = False;

   VexGuestPPC64State* ppc64 = (VexGuestPPC64State*) get_arch (set, tst);


#if defined (VG_LITTLEENDIAN)
   /* Fetch the 64-bits for the VR registers (VSR[32] to VSR[63] stored as
    * Little Endian. The 128-bit value is stored as an array of four 32-bit
    * values.  The lower 32-bits are in element 0 in Little Endian format.
    */
   low_offset  = 0;

   /* Fetch the upper 64-bits for the floating point register stored as
    * Little Endian.  The 128-bit value is stored as an array of four 32-bit
    * values.  The upper 32-bits are in element 3 in Little Endian format.
    */
   high_offset = 2;
#elif defined (VG_BIGENDIAN)
   /* Fetch the 64-bits for the VR registers (VSR[32] to VSR[63] stored as
    * Little Endian. The 128-bit value is stored as an array of four 32-bit
    * values.  The lower 32-bits are in element 3 in Big Endian format.
    */
   low_offset  = 2;

   /* Fetch the upper 64-bits for the floating point register stored as
    * Little Endian.  The 128-bit value is stored as an array of four 32-bit
    * values.  The upper 32-bits are in element 0 in Big Endian format.
    */
   high_offset = 0;
#else
#     error "Unknown endianness"
#endif

   switch (regno) {
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&ppc64->guest_GPR0,  buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&ppc64->guest_GPR1,  buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&ppc64->guest_GPR2,  buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&ppc64->guest_GPR3,  buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&ppc64->guest_GPR4,  buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&ppc64->guest_GPR5,  buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&ppc64->guest_GPR6,  buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&ppc64->guest_GPR7,  buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&ppc64->guest_GPR8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&ppc64->guest_GPR9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&ppc64->guest_GPR10, buf, dir, size, mod); break;
   case 11: VG_(transfer) (&ppc64->guest_GPR11, buf, dir, size, mod); break;
   case 12: VG_(transfer) (&ppc64->guest_GPR12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&ppc64->guest_GPR13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&ppc64->guest_GPR14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&ppc64->guest_GPR15, buf, dir, size, mod); break;
   case 16: VG_(transfer) (&ppc64->guest_GPR16, buf, dir, size, mod); break;
   case 17: VG_(transfer) (&ppc64->guest_GPR17, buf, dir, size, mod); break;
   case 18: VG_(transfer) (&ppc64->guest_GPR18, buf, dir, size, mod); break;
   case 19: VG_(transfer) (&ppc64->guest_GPR19, buf, dir, size, mod); break;
   case 20: VG_(transfer) (&ppc64->guest_GPR20, buf, dir, size, mod); break;
   case 21: VG_(transfer) (&ppc64->guest_GPR21, buf, dir, size, mod); break;
   case 22: VG_(transfer) (&ppc64->guest_GPR22, buf, dir, size, mod); break;
   case 23: VG_(transfer) (&ppc64->guest_GPR23, buf, dir, size, mod); break;
   case 24: VG_(transfer) (&ppc64->guest_GPR24, buf, dir, size, mod); break;
   case 25: VG_(transfer) (&ppc64->guest_GPR25, buf, dir, size, mod); break;
   case 26: VG_(transfer) (&ppc64->guest_GPR26, buf, dir, size, mod); break;
   case 27: VG_(transfer) (&ppc64->guest_GPR27, buf, dir, size, mod); break;
   case 28: VG_(transfer) (&ppc64->guest_GPR28, buf, dir, size, mod); break;
   case 29: VG_(transfer) (&ppc64->guest_GPR29, buf, dir, size, mod); break;
   case 30: VG_(transfer) (&ppc64->guest_GPR30, buf, dir, size, mod); break;
   case 31: VG_(transfer) (&ppc64->guest_GPR31, buf, dir, size, mod); break;

   case 32: VG_(transfer) (&ppc64->guest_VSR0[high_offset],  buf, dir, size, mod); break;
   case 33: VG_(transfer) (&ppc64->guest_VSR1[high_offset],  buf, dir, size, mod); break;
   case 34: VG_(transfer) (&ppc64->guest_VSR2[high_offset],  buf, dir, size, mod); break;
   case 35: VG_(transfer) (&ppc64->guest_VSR3[high_offset],  buf, dir, size, mod); break;
   case 36: VG_(transfer) (&ppc64->guest_VSR4[high_offset],  buf, dir, size, mod); break;
   case 37: VG_(transfer) (&ppc64->guest_VSR5[high_offset],  buf, dir, size, mod); break;
   case 38: VG_(transfer) (&ppc64->guest_VSR6[high_offset],  buf, dir, size, mod); break;
   case 39: VG_(transfer) (&ppc64->guest_VSR7[high_offset],  buf, dir, size, mod); break;
   case 40: VG_(transfer) (&ppc64->guest_VSR8[high_offset],  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&ppc64->guest_VSR9[high_offset],  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&ppc64->guest_VSR10[high_offset],  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&ppc64->guest_VSR11[high_offset],  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&ppc64->guest_VSR12[high_offset],  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&ppc64->guest_VSR13[high_offset],  buf, dir, size, mod); break;
   case 46: VG_(transfer) (&ppc64->guest_VSR14[high_offset],  buf, dir, size, mod); break;
   case 47: VG_(transfer) (&ppc64->guest_VSR15[high_offset],  buf, dir, size, mod); break;
   case 48: VG_(transfer) (&ppc64->guest_VSR16[high_offset],  buf, dir, size, mod); break;
   case 49: VG_(transfer) (&ppc64->guest_VSR17[high_offset],  buf, dir, size, mod); break;
   case 50: VG_(transfer) (&ppc64->guest_VSR18[high_offset],  buf, dir, size, mod); break;
   case 51: VG_(transfer) (&ppc64->guest_VSR19[high_offset],  buf, dir, size, mod); break;
   case 52: VG_(transfer) (&ppc64->guest_VSR20[high_offset],  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&ppc64->guest_VSR21[high_offset],  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&ppc64->guest_VSR22[high_offset],  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&ppc64->guest_VSR23[high_offset],  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&ppc64->guest_VSR24[high_offset],  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&ppc64->guest_VSR25[high_offset],  buf, dir, size, mod); break;
   case 58: VG_(transfer) (&ppc64->guest_VSR26[high_offset],  buf, dir, size, mod); break;
   case 59: VG_(transfer) (&ppc64->guest_VSR27[high_offset],  buf, dir, size, mod); break;
   case 60: VG_(transfer) (&ppc64->guest_VSR28[high_offset],  buf, dir, size, mod); break;
   case 61: VG_(transfer) (&ppc64->guest_VSR29[high_offset],  buf, dir, size, mod); break;
   case 62: VG_(transfer) (&ppc64->guest_VSR30[high_offset],  buf, dir, size, mod); break;
   case 63: VG_(transfer) (&ppc64->guest_VSR31[high_offset],  buf, dir, size, mod); break;

   case 64: VG_(transfer) (&ppc64->guest_CIA,   buf, dir, size, mod); break;
   case 65: *mod = False; break; // VEX does not model Machine State Register
   case 66: {
      UInt cr = LibVEX_GuestPPC64_get_CR (ppc64);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&cr, buf, dir, size, mod);
      } else {
         UInt newcr;
         VG_(transfer) (&newcr, buf, dir, size, mod);
         *mod = newcr != cr;
         LibVEX_GuestPPC64_put_CR (newcr, ppc64);
      }
      break;
   }
   case 67: VG_(transfer) (&ppc64->guest_LR,    buf, dir, size, mod); break;
   case 68: VG_(transfer) (&ppc64->guest_CTR,   buf, dir, size, mod); break;
   case 69: {
      UInt xer = LibVEX_GuestPPC64_get_XER (ppc64);
      if (dir == valgrind_to_gdbserver) {
         VG_(transfer) (&xer, buf, dir, size, mod); 
      } else {
         UInt newxer;
         VG_(transfer) (&newxer, buf, dir, size, mod);
         *mod = newxer != xer;
         LibVEX_GuestPPC64_put_XER (newxer, ppc64);
      }
      break;
   }
   case 70:  VG_(transfer) (&ppc64->guest_FPROUND, buf, dir, size, mod); break;
   case 71:  *mod = False; break; // GDBTD???? VEX { "orig_r3", 4448, 64 },
   case 72:  *mod = False; break; // GDBTD???? VEX { "trap", 4512, 64 },

   case 73:  VG_(transfer) (&ppc64->guest_VSR32,  buf, dir, size, mod);  break;
   case 74:  VG_(transfer) (&ppc64->guest_VSR33,  buf, dir, size, mod);  break;
   case 75:  VG_(transfer) (&ppc64->guest_VSR34,  buf, dir, size, mod);  break;
   case 76:  VG_(transfer) (&ppc64->guest_VSR35,  buf, dir, size, mod);  break;
   case 77:  VG_(transfer) (&ppc64->guest_VSR36,  buf, dir, size, mod);  break;
   case 78:  VG_(transfer) (&ppc64->guest_VSR37,  buf, dir, size, mod);  break;
   case 79:  VG_(transfer) (&ppc64->guest_VSR38,  buf, dir, size, mod);  break;
   case 80:  VG_(transfer) (&ppc64->guest_VSR39,  buf, dir, size, mod);  break;
   case 81:  VG_(transfer) (&ppc64->guest_VSR40,  buf, dir, size, mod);  break;
   case 82:  VG_(transfer) (&ppc64->guest_VSR40,  buf, dir, size, mod);  break;
   case 83:  VG_(transfer) (&ppc64->guest_VSR42,  buf, dir, size, mod); break;
   case 84:  VG_(transfer) (&ppc64->guest_VSR43,  buf, dir, size, mod); break;
   case 85:  VG_(transfer) (&ppc64->guest_VSR44,  buf, dir, size, mod); break;
   case 86:  VG_(transfer) (&ppc64->guest_VSR45,  buf, dir, size, mod); break;
   case 87:  VG_(transfer) (&ppc64->guest_VSR46,  buf, dir, size, mod); break;
   case 88:  VG_(transfer) (&ppc64->guest_VSR47,  buf, dir, size, mod); break;
   case 89:  VG_(transfer) (&ppc64->guest_VSR48,  buf, dir, size, mod); break;
   case 90:  VG_(transfer) (&ppc64->guest_VSR49,  buf, dir, size, mod); break;
   case 91:  VG_(transfer) (&ppc64->guest_VSR50,  buf, dir, size, mod); break;
   case 92:  VG_(transfer) (&ppc64->guest_VSR51,  buf, dir, size, mod); break;
   case 93:  VG_(transfer) (&ppc64->guest_VSR52,  buf, dir, size, mod); break;
   case 94:  VG_(transfer) (&ppc64->guest_VSR53,  buf, dir, size, mod); break;
   case 95:  VG_(transfer) (&ppc64->guest_VSR54,  buf, dir, size, mod); break;
   case 96:  VG_(transfer) (&ppc64->guest_VSR55,  buf, dir, size, mod); break;
   case 97:  VG_(transfer) (&ppc64->guest_VSR56,  buf, dir, size, mod); break;
   case 98:  VG_(transfer) (&ppc64->guest_VSR57,  buf, dir, size, mod); break;
   case 99:  VG_(transfer) (&ppc64->guest_VSR58,  buf, dir, size, mod); break;
   case 100: VG_(transfer) (&ppc64->guest_VSR59,  buf, dir, size, mod); break;
   case 101: VG_(transfer) (&ppc64->guest_VSR60,  buf, dir, size, mod); break;
   case 102: VG_(transfer) (&ppc64->guest_VSR61,  buf, dir, size, mod); break;
   case 103: VG_(transfer) (&ppc64->guest_VSR62,  buf, dir, size, mod); break;
   case 104: VG_(transfer) (&ppc64->guest_VSR63,  buf, dir, size, mod); break;
   case 105: VG_(transfer) (&ppc64->guest_VSCR, buf, dir, size, mod);   break;
   case 106: VG_(transfer) (&ppc64->guest_VRSAVE, buf, dir, size, mod); break;

   /* Fetch the lower 64-bits of the VSR registers.  GDB will combine the
    * lower 64-bits of the VSR with the upper 64-bits it got fetching the
    * corresponding floating point register to display the full 128-bit
    * VSR value.
    */
   case 107:  VG_(transfer) (&ppc64->guest_VSR0[low_offset], buf, dir, size, mod); break;
   case 108:  VG_(transfer) (&ppc64->guest_VSR1[low_offset], buf, dir, size, mod); break;
   case 109:  VG_(transfer) (&ppc64->guest_VSR2[low_offset], buf, dir, size, mod); break;
   case 110:  VG_(transfer) (&ppc64->guest_VSR3[low_offset], buf, dir, size, mod); break;
   case 111:  VG_(transfer) (&ppc64->guest_VSR4[low_offset], buf, dir, size, mod); break;
   case 112:  VG_(transfer) (&ppc64->guest_VSR5[low_offset], buf, dir, size, mod); break;
   case 113:  VG_(transfer) (&ppc64->guest_VSR6[low_offset], buf, dir, size, mod); break;
   case 114:  VG_(transfer) (&ppc64->guest_VSR7[low_offset], buf, dir, size, mod); break;
   case 115:  VG_(transfer) (&ppc64->guest_VSR8[low_offset], buf, dir, size, mod); break;
   case 116:  VG_(transfer) (&ppc64->guest_VSR9[low_offset], buf, dir, size, mod); break;
   case 117:  VG_(transfer) (&ppc64->guest_VSR10[low_offset], buf, dir, size, mod); break;
   case 118:  VG_(transfer) (&ppc64->guest_VSR11[low_offset], buf, dir, size, mod); break;
   case 119:  VG_(transfer) (&ppc64->guest_VSR12[low_offset], buf, dir, size, mod); break;
   case 120:  VG_(transfer) (&ppc64->guest_VSR13[low_offset], buf, dir, size, mod); break;
   case 121:  VG_(transfer) (&ppc64->guest_VSR14[low_offset], buf, dir, size, mod); break;
   case 122:  VG_(transfer) (&ppc64->guest_VSR15[low_offset], buf, dir, size, mod); break;
   case 123:  VG_(transfer) (&ppc64->guest_VSR16[low_offset], buf, dir, size, mod); break;
   case 124:  VG_(transfer) (&ppc64->guest_VSR17[low_offset], buf, dir, size, mod); break;
   case 125:  VG_(transfer) (&ppc64->guest_VSR18[low_offset], buf, dir, size, mod); break;
   case 126:  VG_(transfer) (&ppc64->guest_VSR19[low_offset], buf, dir, size, mod); break;
   case 127:  VG_(transfer) (&ppc64->guest_VSR20[low_offset], buf, dir, size, mod); break;
   case 128:  VG_(transfer) (&ppc64->guest_VSR21[low_offset], buf, dir, size, mod); break;
   case 129:  VG_(transfer) (&ppc64->guest_VSR22[low_offset], buf, dir, size, mod); break;
   case 130:  VG_(transfer) (&ppc64->guest_VSR23[low_offset], buf, dir, size, mod); break;
   case 131:  VG_(transfer) (&ppc64->guest_VSR24[low_offset], buf, dir, size, mod); break;
   case 132:  VG_(transfer) (&ppc64->guest_VSR25[low_offset], buf, dir, size, mod); break;
   case 133:  VG_(transfer) (&ppc64->guest_VSR26[low_offset], buf, dir, size, mod); break;
   case 134:  VG_(transfer) (&ppc64->guest_VSR27[low_offset], buf, dir, size, mod); break;
   case 135:  VG_(transfer) (&ppc64->guest_VSR28[low_offset], buf, dir, size, mod); break;
   case 136:  VG_(transfer) (&ppc64->guest_VSR29[low_offset], buf, dir, size, mod); break;
   case 137:  VG_(transfer) (&ppc64->guest_VSR30[low_offset], buf, dir, size, mod); break;
   case 138:  VG_(transfer) (&ppc64->guest_VSR31[low_offset], buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   /* NOTE, the current powerpc-altivec64l*.xml files includes the vsx
    * registers.  Power 6 and earlier power processors do not support the
    * vsx registers.  GDB has a bug in that it is only checking for ptrace
    * support rather then checking the actual HW feature.  Hence GDB on
    * power 6 prints vsx registers that do not exist.  Valgrind GDB support
    * also has to include the vsx register definitions to be consistent with
    * GDB.
    */
   if (shadow_mode) {
      return "powerpc-altivec64l-valgrind.xml";
   } else {
      return "powerpc-altivec64l.xml";
   }  
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestPPC64State* ppc64 = (VexGuestPPC64State*)&tst->arch.vex;
   // ppc64 dtv is located just before the tcb, which is 0x7000 before 
   // the thread id (r13)
   return (CORE_ADDR**)((CORE_ADDR)ppc64->guest_GPR13 
                        - 0x7000 - sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   1, //r1
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "ppc64",
   target_xml,
   target_get_dtv
};

void ppc64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
