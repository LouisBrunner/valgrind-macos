/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024-2025  Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_ir.h"        // emptyIRSB
#include "libvex.h"           // LibVEX_Init
#include "guest_s390_defs.h"  // disInstr_S390
#include "host_s390_defs.h"   // s390_host_hwcaps
#include "main_globals.h"     // vex_traceflags

/* Some VEX header defines this. Need to get rid of it before
   including standard headers. */
#undef NULL
#include <stddef.h>           // NULL
#include <stdlib.h>           // free
#include <string.h>           // strlen
#include <ctype.h>            // isspace
#include "main.h"             // fatal
#include "vex.h"

static IRSB *dis_irsb;
static char *last_vex_string;


/* This function is called from vfatal, vpanic, or due to a failed
   assertion in VEX. vex_printf was called just before. */
__attribute__((noreturn))
static void
vex_exit(void)
{
   if (last_vex_string)
      fatal("VEX: %s\n", last_vex_string);
   else
      fatal("vex_exit was called\n");
}


/* This function is called from VEX whenever it wants to print a string.
   We've arranged for the disassembled instruction to be printed. So we
   intercept it here and stash it away.
   However, this function may also be called when something unexpected
   occurs in VEX.
   Nb: strange function prototype.
       nbytes == strlen(string) at all times. */
static void
vex_put_string(const char *string, unsigned long nbytes)
{
   static unsigned buf_size = 0;
   static char *buf = NULL;
   unsigned need = strlen(string) + 1;

   if (need > buf_size) {
      free(buf);
      buf = mallock(need);
   }

   /* Copy the string and remove any trailing white space. */
   strcpy(buf, string);

   for (int i = strlen(buf) - 1; i >= 0; --i) {
      if (! isspace(string[i]))
         break;
      buf[i] = '\0';
   }

   last_vex_string = buf;
}


/* Initialise the disassembly machinery. */
void
vex_init(void)
{
   if (vex_initdone) return;

   VexControl vcon;

   LibVEX_default_VexControl(&vcon);
   LibVEX_Init(vex_exit, vex_put_string, 0, &vcon);

   /* Enable disassembly. */
   vex_traceflags = VEX_TRACE_FE;

   /* Pretend all hardware extensions are available to avoid running
      into an emulation failure */
   s390_host_hwcaps = VEX_HWCAPS_S390X_ALL;

   dis_irsb = emptyIRSB();
}


/* Reset the VEX memory allocator. Otherwise, we'll run out of memory
   with a suggestion to recompile valgrind. Yuck. */
static void
vex_reset(void)
{
   if (vex_initdone) {
      vexSetAllocModeTEMP_and_clear();
      dis_irsb = emptyIRSB();
   }

   /* Otherwise we won't make it through s390_irgen_EXRL. */
   last_execute_target = 42;
}


/* Disassemble a single insn.
   The returned string will be overwritten the next time vex_disasm
   is called. The function may return NULL indicating that something
   inside VEX went wrong. */
const char *
vex_disasm(const unsigned char *codebuf, int *spec_exc)
{
   DisResult res;

   /* Work around VEX running out of memory. */
   vex_reset();

   res = disInstr_S390(dis_irsb, codebuf, /* delta */0, /* guest_IA */0,
                       VexArchS390X, NULL, NULL, VexEndnessBE, 0);

   /* Check for specification exception. Cf. macro s390_insn_assert
      in guest_s390_toIR.c */
   if (res.whatNext == Dis_StopHere &&
       res.jk_StopHere == Ijk_NoDecode) {
      *spec_exc = 1;
   }

   return last_vex_string;
}
