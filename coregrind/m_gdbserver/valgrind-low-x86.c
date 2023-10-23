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

#include "libvex_guest_x86.h"
/* GDBTD: ??? have a cleaner way to get the f80 <> f64 conversion functions */
/* below include needed for conversion f80 <> f64 */
#include "../../VEX/priv/guest_generic_x87.h"


/* below loosely inspired from  file generated with gdb regdat.sh  */

static struct reg regs[] = {
   { "eax", 0, 32 },
   { "ecx", 32, 32 },
   { "edx", 64, 32 },
   { "ebx", 96, 32 },
   { "esp", 128, 32 },
   { "ebp", 160, 32 },
   { "esi", 192, 32 },
   { "edi", 224, 32 },
   { "eip", 256, 32 },
   { "eflags", 288, 32 },
   { "cs", 320, 32 },
   { "ss", 352, 32 },
   { "ds", 384, 32 },
   { "es", 416, 32 },
   { "fs", 448, 32 },
   { "gs", 480, 32 },
   { "st0", 512, 80 },
   { "st1", 592, 80 },
   { "st2", 672, 80 },
   { "st3", 752, 80 },
   { "st4", 832, 80 },
   { "st5", 912, 80 },
   { "st6", 992, 80 },
   { "st7", 1072, 80 },
   { "fctrl", 1152, 32 },
   { "fstat", 1184, 32 },
   { "ftag", 1216, 32 },
   { "fiseg", 1248, 32 },
   { "fioff", 1280, 32 },
   { "foseg", 1312, 32 },
   { "fooff", 1344, 32 },
   { "fop", 1376, 32 },
   { "xmm0", 1408, 128 },
   { "xmm1", 1536, 128 },
   { "xmm2", 1664, 128 },
   { "xmm3", 1792, 128 },
   { "xmm4", 1920, 128 },
   { "xmm5", 2048, 128 },
   { "xmm6", 2176, 128 },
   { "xmm7", 2304, 128 },
   { "mxcsr", 2432, 32 },
#if defined(VGO_linux)
   { "orig_eax", 2464, 32 }
#endif
};
static const char *expedite_regs[] = { "ebp", "esp", "eip", 0 };
#define num_regs (sizeof (regs) / sizeof (regs[0]))

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("eip", &pc);
   
   dlog(1, "stop pc is %p\n", (void *) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   supply_register_by_name ("eip", &newpc);
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

   VexGuestX86State* x86 = (VexGuestX86State*) get_arch (set, tst);

   switch (regno) { 
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&x86->guest_EAX, buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&x86->guest_ECX, buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&x86->guest_EDX, buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&x86->guest_EBX, buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&x86->guest_ESP, buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&x86->guest_EBP, buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&x86->guest_ESI, buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&x86->guest_EDI, buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&x86->guest_EIP, buf, dir, size, mod); break;
   case 9:  
      if (dir == valgrind_to_gdbserver) {
         UInt eflags;
         /* we can only retrieve the real flags (set 0)
            retrieving shadow flags is not ok */
         if (set == 0)
            eflags = LibVEX_GuestX86_get_eflags (x86);
         else
            eflags = 0;
         VG_(transfer) (&eflags, buf, dir, size, mod); break;
      } else {
         *mod = False; //GDBTD? how do we store eflags in libvex_guest_x86.h ???
      }
      break;
   case 10: VG_(transfer) (&x86->guest_CS, buf, dir, size, mod); break;
   case 11: VG_(transfer) (&x86->guest_SS, buf, dir, size, mod); break;
   case 12: VG_(transfer) (&x86->guest_DS, buf, dir, size, mod); break;
   case 13: VG_(transfer) (&x86->guest_ES, buf, dir, size, mod); break;
   case 14: VG_(transfer) (&x86->guest_FS, buf, dir, size, mod); break;
   case 15: VG_(transfer) (&x86->guest_GS, buf, dir, size, mod); break;
   case 16:
   case 17:
   case 18:
   case 19: /* register 16 to 23 are float registers 80 bits but 64 bits in valgrind */
   case 20:
   case 21:
   case 22:
   case 23: {
      if (dir == valgrind_to_gdbserver) {
         UChar fpreg80[10];
         convert_f64le_to_f80le ((UChar *)&x86->guest_FPREG[regno-16],
                                 fpreg80);
         VG_(transfer) (&fpreg80, buf, dir, sizeof(fpreg80), mod);
      } else {
         ULong fpreg64;
         convert_f80le_to_f64le (buf, (UChar *)&fpreg64); 
         VG_(transfer) (&x86->guest_FPREG[regno-16], &fpreg64, 
                        dir, sizeof(fpreg64), mod);
      }
      break;
   }
   case 24: 
      if (dir == valgrind_to_gdbserver) {
         // vex only models the rounding bits (see libvex_guest_x86.h)
         UWord value = 0x037f;
         value |= x86->guest_FPROUND << 10;
         VG_(transfer)(&value, buf, dir, size, mod);
      } else {
         *mod = False; // GDBTD???? VEX { "fctrl", 1152, 32 },
      }
      break; 
   case 25:
      if (dir == valgrind_to_gdbserver) {
         UWord value = x86->guest_FC3210;
         value |= (x86->guest_FTOP & 7) << 11;
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False; // GDBTD???? VEX { "fstat", 1184, 32 },
      }
      break;
   case 26: 
      if (dir == valgrind_to_gdbserver) {
         // vex doesn't model these precisely
         UWord value = 
            ((x86->guest_FPTAG[0] ? 0 : 3) << 0)  | 
            ((x86->guest_FPTAG[1] ? 0 : 3) << 2)  | 
            ((x86->guest_FPTAG[2] ? 0 : 3) << 4)  | 
            ((x86->guest_FPTAG[3] ? 0 : 3) << 6)  | 
            ((x86->guest_FPTAG[4] ? 0 : 3) << 8)  | 
            ((x86->guest_FPTAG[5] ? 0 : 3) << 10) | 
            ((x86->guest_FPTAG[6] ? 0 : 3) << 12) | 
            ((x86->guest_FPTAG[7] ? 0 : 3) << 14);
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False;  // GDBTD???? VEX { "ftag", 1216, 32 },
      }
      break;
   case 27: *mod = False; break; // GDBTD???? VEX { "fiseg", 1248, 32 },
   case 28: *mod = False; break; // GDBTD???? VEX { "fioff", 1280, 32 },
   case 29: *mod = False; break; // GDBTD???? VEX { "foseg", 1312, 32 },
   case 30: *mod = False; break; // GDBTD???? VEX { "fooff", 1344, 32 },
   case 31: *mod = False; break; // GDBTD???? VEX { "fop", 1376, 32 },
   case 32: VG_(transfer) (&x86->guest_XMM0, buf, dir, size, mod); break;
   case 33: VG_(transfer) (&x86->guest_XMM1, buf, dir, size, mod); break;
   case 34: VG_(transfer) (&x86->guest_XMM2, buf, dir, size, mod); break;
   case 35: VG_(transfer) (&x86->guest_XMM3, buf, dir, size, mod); break;
   case 36: VG_(transfer) (&x86->guest_XMM4, buf, dir, size, mod); break;
   case 37: VG_(transfer) (&x86->guest_XMM5, buf, dir, size, mod); break;
   case 38: VG_(transfer) (&x86->guest_XMM6, buf, dir, size, mod); break;
   case 39: VG_(transfer) (&x86->guest_XMM7, buf, dir, size, mod); break;
   case 40: 
      if (dir == valgrind_to_gdbserver) {
         // vex only models the rounding bits (see libvex_guest_x86.h)
         UWord value = 0x1f80;
         value |= x86->guest_SSEROUND << 13;
         VG_(transfer)(&value, buf, dir, size, mod); 
      } else {
         *mod = False; // GDBTD???? VEX { "mxcsr", 2432, 32 },
      }
      break;
   case 41: *mod = False; break; // GDBTD???? VEX { "orig_eax", 2464, 32 },
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
#if defined(VGO_linux)
   return "i386-linux-valgrind.xml";
#else
   return "i386-coresse-valgrind.xml";
#endif
   } else {
      return NULL;
   }  
}

static CORE_ADDR** target_get_dtv (ThreadState *tst)
{
   VexGuestX86State* x86 = (VexGuestX86State*)&tst->arch.vex;
   // FIXME: should make the below formally visible from VEX.
   extern ULong x86g_use_seg_selector ( HWord ldt, HWord gdt,
                                        UInt seg_selector, UInt virtual_addr );

   ULong dtv_loc_g = x86g_use_seg_selector (x86->guest_LDT,
                                            x86->guest_GDT,
                                            x86->guest_GS,
                                            0x4);
   if (dtv_loc_g == 1ULL << 32) {
      dlog(0, "Error getting x86 dtv\n");
      return NULL;
   } else {
      CORE_ADDR dtv_loc = dtv_loc_g;
      return (CORE_ADDR**)dtv_loc;
   }
}

static struct valgrind_target_ops low_target = {
   num_regs,
   4, //ESP
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "i386",
   target_xml,
   target_get_dtv
};

void x86_init_architecture (struct valgrind_target_ops *target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
