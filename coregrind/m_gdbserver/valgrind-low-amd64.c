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

#include "libvex_guest_amd64.h"
/* GDBTD: ??? have a cleaner way to get the f80 <> f64 conversion functions */
/* below include needed for conversion f80 <> f64 */
#include "../../VEX/priv/guest_generic_x87.h" 

/* below loosely inspired from file generated with gdb regdat.sh */

struct reg regs[] = {
  { "rax", 0, 64 },
  { "rbx", 64, 64 },
  { "rcx", 128, 64 },
  { "rdx", 192, 64 },
  { "rsi", 256, 64 },
  { "rdi", 320, 64 },
  { "rbp", 384, 64 },
  { "rsp", 448, 64 },
  { "r8", 512, 64 },
  { "r9", 576, 64 },
  { "r10", 640, 64 },
  { "r11", 704, 64 },
  { "r12", 768, 64 },
  { "r13", 832, 64 },
  { "r14", 896, 64 },
  { "r15", 960, 64 },
  { "rip", 1024, 64 },
  { "eflags", 1088, 32 },
  { "cs", 1120, 32 },
  { "ss", 1152, 32 },
  { "ds", 1184, 32 },
  { "es", 1216, 32 },
  { "fs", 1248, 32 },
  { "gs", 1280, 32 },
  { "st0", 1312, 80 },
  { "st1", 1392, 80 },
  { "st2", 1472, 80 },
  { "st3", 1552, 80 },
  { "st4", 1632, 80 },
  { "st5", 1712, 80 },
  { "st6", 1792, 80 },
  { "st7", 1872, 80 },
  { "fctrl", 1952, 32 },
  { "fstat", 1984, 32 },
  { "ftag", 2016, 32 },
  { "fiseg", 2048, 32 },
  { "fioff", 2080, 32 },
  { "foseg", 2112, 32 },
  { "fooff", 2144, 32 },
  { "fop", 2176, 32 },
  { "xmm0", 2208, 128 },
  { "xmm1", 2336, 128 },
  { "xmm2", 2464, 128 },
  { "xmm3", 2592, 128 },
  { "xmm4", 2720, 128 },
  { "xmm5", 2848, 128 },
  { "xmm6", 2976, 128 },
  { "xmm7", 3104, 128 },
  { "xmm8", 3232, 128 },
  { "xmm9", 3360, 128 },
  { "xmm10", 3488, 128 },
  { "xmm11", 3616, 128 },
  { "xmm12", 3744, 128 },
  { "xmm13", 3872, 128 },
  { "xmm14", 4000, 128 },
  { "xmm15", 4128, 128 },
  { "mxcsr", 4256, 32  },
#if defined(VGO_linux)
  { "orig_rax", 4288, 64 }
#endif
};
static const char *expedite_regs[] = { "rbp", "rsp", "rip", 0 };
#define num_regs (sizeof (regs) / sizeof (regs[0]))

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("rip", &pc);
   
   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   Bool mod;
   supply_register_by_name ("rip", &newpc, &mod);
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

   VexGuestAMD64State* amd64 = (VexGuestAMD64State*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above.
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&amd64->guest_RAX, buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&amd64->guest_RBX, buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&amd64->guest_RCX, buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&amd64->guest_RDX, buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&amd64->guest_RSI, buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&amd64->guest_RDI, buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&amd64->guest_RBP, buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&amd64->guest_RSP, buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&amd64->guest_R8,  buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&amd64->guest_R9,  buf, dir, size, mod); break;
   case 10: VG_(transfer) (&amd64->guest_R10, buf, dir, size, mod); break;
   case 11: VG_(transfer) (&amd64->guest_R11, buf, dir, size, mod); break;
   case 12: VG_(transfer) (&amd64->guest_R12, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&amd64->guest_R13, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&amd64->guest_R14, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&amd64->guest_R15, buf, dir, size, mod); break;
   case 16: 
      VG_(transfer) (&amd64->guest_RIP, buf, dir, size, mod);
      if (*mod && VG_(debugLog_getLevel)() > 2) {
         char bufimage [2*sizeof(amd64->guest_IP_AT_SYSCALL) + 1];
         heximage (bufimage, 
                   (char *) &amd64->guest_IP_AT_SYSCALL, 
                   sizeof(amd64->guest_IP_AT_SYSCALL));
         dlog(3, "guest_IP_AT_SYSCALL %s\n", bufimage);
      }
      break;
   case 17: 
      if (dir == valgrind_to_gdbserver) {
         ULong rflags;
         /* we can only retrieve the real flags (set 0)
            retrieving shadow flags is not ok */
         if (set == 0)
            rflags = LibVEX_GuestAMD64_get_rflags (amd64);
         else
            rflags = 0;
         VG_(transfer) (&rflags, buf, dir, size, mod); 
      } else {
         *mod = False; //GDBTD? how do we store rflags in libvex_guest_amd64.h ???
      }
      break; 
   case 18: *mod = False; break; //GDBTD VG_(transfer) (&amd64->guest_CS, buf, dir, size, mod);
   case 19: *mod = False; break; //GDBTD VG_(transfer) (&amd64->guest_SS, buf, dir, size, mod);
   case 20: *mod = False; break; //GDBTD VG_(transfer) (&amd64->guest_DS, buf, dir, size, mod);
   case 21: *mod = False; break; //GDBTD VG_(transfer) (&amd64->guest_ES, buf, dir, size, mod);
   case 22: *mod = False; break; //GDBTD VG_(transfer) (&amd64->guest_FS, buf, dir, size, mod);
   case 23: VG_(transfer) (&amd64->guest_GS_0x60, buf, dir, size, mod); break;
   case 24:
   case 25:
   case 26:
   case 27: /* register 24 to 31 are float registers 80 bits but 64 bits in valgrind */
   case 28:
   case 29:
   case 30:
   case 31: 
      if (dir == valgrind_to_gdbserver) {
         UChar fpreg80[10];
         convert_f64le_to_f80le ((UChar *)&amd64->guest_FPREG[regno-16],
                                 fpreg80);
         VG_(transfer) (&fpreg80, buf, dir, sizeof(fpreg80), mod);
      } else {
         ULong fpreg64;
         convert_f80le_to_f64le (buf, (UChar *)&fpreg64); 
         VG_(transfer) (&amd64->guest_FPREG[regno-16], &fpreg64,
                        dir, sizeof(fpreg64), mod);
      }
      break;
   case 32: 
      if (dir == valgrind_to_gdbserver) {
         // vex only models the rounding bits (see libvex_guest_amd64.h)
         UWord value = 0x037f;
         value |= amd64->guest_FPROUND << 10;
         VG_(transfer)(&value, buf, dir, size, mod);
      } else {
         *mod = False; // GDBTD???? VEX equivalent fcrtl
      }
      break;
   case 33: 
      if (dir == valgrind_to_gdbserver) {
         UWord value = amd64->guest_FC3210;
         value |= (amd64->guest_FTOP & 7) << 11;
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False; // GDBTD???? VEX equivalent fstat
      }
      break;
   case 34: 
      if (dir == valgrind_to_gdbserver) {
         // vex doesn't model these precisely
         UWord value = 
            ((amd64->guest_FPTAG[0] ? 0 : 3) << 0)  | 
            ((amd64->guest_FPTAG[1] ? 0 : 3) << 2)  | 
            ((amd64->guest_FPTAG[2] ? 0 : 3) << 4)  | 
            ((amd64->guest_FPTAG[3] ? 0 : 3) << 6)  | 
            ((amd64->guest_FPTAG[4] ? 0 : 3) << 8)  | 
            ((amd64->guest_FPTAG[5] ? 0 : 3) << 10) | 
            ((amd64->guest_FPTAG[6] ? 0 : 3) << 12) | 
            ((amd64->guest_FPTAG[7] ? 0 : 3) << 14);
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False; // GDBTD???? VEX equivalent ftag
      }
      break;
   case 35: *mod = False; break; // GDBTD ??? equivalent of fiseg
   case 36: *mod = False; break; // GDBTD ??? equivalent of fioff
   case 37: *mod = False; break; // GDBTD ??? equivalent of foseg
   case 38: *mod = False; break; // GDBTD ??? equivalent of fooff
   case 39: *mod = False; break; // GDBTD ??? equivalent of fop
   case 40: VG_(transfer) (&amd64->guest_XMM0,  buf, dir, size, mod); break;
   case 41: VG_(transfer) (&amd64->guest_XMM1,  buf, dir, size, mod); break;
   case 42: VG_(transfer) (&amd64->guest_XMM2,  buf, dir, size, mod); break;
   case 43: VG_(transfer) (&amd64->guest_XMM3,  buf, dir, size, mod); break;
   case 44: VG_(transfer) (&amd64->guest_XMM4,  buf, dir, size, mod); break;
   case 45: VG_(transfer) (&amd64->guest_XMM5,  buf, dir, size, mod); break;
   case 46: VG_(transfer) (&amd64->guest_XMM6,  buf, dir, size, mod); break;
   case 47: VG_(transfer) (&amd64->guest_XMM7,  buf, dir, size, mod); break;
   case 48: VG_(transfer) (&amd64->guest_XMM8,  buf, dir, size, mod); break;
   case 49: VG_(transfer) (&amd64->guest_XMM9,  buf, dir, size, mod); break;
   case 50: VG_(transfer) (&amd64->guest_XMM10, buf, dir, size, mod); break;
   case 51: VG_(transfer) (&amd64->guest_XMM11, buf, dir, size, mod); break;
   case 52: VG_(transfer) (&amd64->guest_XMM12, buf, dir, size, mod); break;
   case 53: VG_(transfer) (&amd64->guest_XMM13, buf, dir, size, mod); break;
   case 54: VG_(transfer) (&amd64->guest_XMM14, buf, dir, size, mod); break;
   case 55: VG_(transfer) (&amd64->guest_XMM15, buf, dir, size, mod); break;
   case 56: 
      if (dir == valgrind_to_gdbserver) {
         // vex only models the rounding bits (see libvex_guest_x86.h)
         UWord value = 0x1f80;
         value |= amd64->guest_SSEROUND << 13;
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False;  // GDBTD???? VEX equivalent mxcsr
      }
      break;
   case 57: *mod = False; break; // GDBTD???? VEX equivalent { "orig_rax"},
   default: vg_assert(0);
   }
}

static struct valgrind_target_ops low_target = {
   num_regs,
   regs,
   7, //RSP
   transfer_register,
   get_pc,
   set_pc,
   "amd64",
   NULL, // target_xml not needed.
#if defined(VGO_linux)
   "amd64-linux-valgrind.xml"
#else
   "amd64-coresse-valgrind.xml"
#endif
};

void amd64_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
