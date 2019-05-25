/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Reading of ARM(32) EXIDX unwind information      readexidx.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Mozilla Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* libunwind - a platform-independent unwind library
   Copyright 2011 Linaro Limited

This file is part of libunwind.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  */


// Copyright (c) 2010 Google Inc.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


// Derived originally from libunwind, with very extensive modifications.
/* Contributed by Julian Seward <jseward@acm.org> */


// This file translates EXIDX unwind information into the same format
// that Valgrind uses for CFI information.  Hence Valgrind's CFI
// unwinding abilities also become usable for EXIDX.
//
// See: "Exception Handling ABI for the ARM Architecture", ARM IHI 0038A
// http://infocenter.arm.com/help/topic/com.arm.doc.ihi0038a/IHI0038A_ehabi.pdf

// EXIDX data is presented in two parts:
//
// * an index table.  This contains two words per routine,
//   the first of which identifies the routine, and the second
//   of which is a reference to the unwind bytecode.  If the
//   bytecode is very compact -- 3 bytes or less -- it can be
//   stored directly in the second word.
//
// * an area containing the unwind bytecodes.
//
// General flow is: ML_(read_exidx) iterates over all
// of the index table entries (pairs).  For each entry, it:
//
// * calls ExtabEntryExtract to copy the bytecode out into 
// an intermediate buffer.

// * uses ExtabEntryDecode to parse the intermediate
//   buffer.  Each bytecode instruction is bundled into a
//   arm_ex_to_module::extab_data structure, and handed to ..
//
// * .. TranslateCmd, which generates the pseudo-CFI
//   records that Valgrind stores.

// This file is derived from the following files in the Mozilla tree
// toolkit/crashreporter/google-breakpad:
//   src/common/arm_ex_to_module.cc
//   src/common/arm_ex_reader.cc


#if defined(VGA_arm)

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_options.h"

#include "priv_storage.h"
#include "priv_readexidx.h"


static void complain ( const HChar* str )
{
   if (!VG_(clo_xml) && VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg,
                   "  Warning: whilst reading EXIDX: %s\n", str);
}


/*------------------------------------------------------------*/
/*--- MemoryRange                                          ---*/
/*------------------------------------------------------------*/

typedef  struct { Addr start; SizeT len; }  MemoryRange;

/* Initialise |mr| for [start .. start+len).  Zero ranges are allowed,
   but wraparounds are not.  Returns True on success. */
static Bool MemoryRange__init ( /*OUT*/MemoryRange* mr,
                                const void* startV, SizeT len )
{
   VG_(memset)(mr, 0, sizeof(*mr));
   /* This relies on Addr being unsigned. */
   Addr start = (Addr)startV;
   if (len > 0 && start + len - 1 < start) {
      return False;
   }
   mr->start = start;
   mr->len   = len;
   return True;
}

static Bool MemoryRange__covers ( MemoryRange* mr,
                                  const void* startV, SizeT len )
{
   vg_assert(len > 0);
   if (mr->len == 0) {
      return False;
   }
   Addr start = (Addr)startV;
   return start >= mr->start && start + len - 1 <= mr->start + mr->len - 1;
}


/*------------------------------------------------------------*/
/*--- (Pass 1 of 3) The EXIDX extractor                    ---*/
/*------------------------------------------------------------*/

#define ARM_EXIDX_CANT_UNWIND 0x00000001
#define ARM_EXIDX_COMPACT     0x80000000
#define ARM_EXTBL_OP_FINISH   0xb0
#define ARM_EXIDX_TABLE_LIMIT (255*4)

/* These are in the ARM-defined format, so their layout is important. */
typedef
   struct { UInt addr; UInt data; }
   ExidxEntry;


typedef
   enum {
      ExSuccess=1,      // success
      ExInBufOverflow,  // out-of-range while reading .exidx
      ExOutBufOverflow, // output buffer is too small
      ExCantUnwind,     // this function is marked CANT_UNWIND
      ExCantRepresent,  // entry valid, but we can't represent it
      ExInvalid         // entry is invalid
   }
   ExExtractResult;


/* Helper function for fishing bits out of the EXIDX representation. */
static const void* Prel31ToAddr(const void* addr)
{
   UInt offset32 = *(const UInt*)addr;
   // sign extend offset32[30:0] to 64 bits -- copy bit 30 to positions
   // 63:31 inclusive.
   ULong offset64 = offset32;
   if (offset64 & (1ULL << 30))
      offset64 |= 0xFFFFFFFF80000000ULL;
   else
      offset64 &= 0x000000007FFFFFFFULL;
   return ((const UChar*)addr) + (UWord)offset64;
}


// Extract unwind bytecode for the function denoted by |entry| into |buf|,
// and return the number of bytes of |buf| written, along with a code
// indicating the outcome.
static
ExExtractResult ExtabEntryExtract ( MemoryRange* mr_exidx,
                                    MemoryRange* mr_extab,
                                    const ExidxEntry* entry,
                                    UChar* buf, SizeT buf_size,
                                    /*OUT*/SizeT* buf_used)
{
   Bool ok;
   MemoryRange mr_out;
   ok = MemoryRange__init(&mr_out, buf, buf_size);
   if (!ok) return ExOutBufOverflow;

   *buf_used = 0;

#  define PUT_BUF_U8(_byte) \
      do { if (!MemoryRange__covers(&mr_out, &buf[*buf_used], 1)) \
              return ExOutBufOverflow; \
           buf[(*buf_used)++] = (_byte); } while (0)

#  define GET_EX_U32(_lval, _addr, _mr) \
      do { if (!MemoryRange__covers((_mr), (const void*)(_addr), 4)) \
              return ExInBufOverflow; \
           (_lval) = *(const UInt*)(_addr); } while (0)

#  define GET_EXIDX_U32(_lval, _addr) \
      GET_EX_U32(_lval, _addr, mr_exidx)

#  define GET_EXTAB_U32(_lval, _addr) \
      GET_EX_U32(_lval, _addr, mr_extab)

   UInt data;
   GET_EXIDX_U32(data, &entry->data);

   // A function can be marked CANT_UNWIND if (eg) it is known to be
   // at the bottom of the stack.
   if (data == ARM_EXIDX_CANT_UNWIND)
      return ExCantUnwind;

   UInt  pers;          // personality number
   UInt  extra;         // number of extra data words required
   UInt  extra_allowed; // number of extra data words allowed
   const UInt* extbl_data;    // the handler entry, if not inlined

   if (data & ARM_EXIDX_COMPACT) {
      // The handler table entry has been inlined into the index table entry.
      // In this case it can only be an ARM-defined compact model, since
      // bit 31 is 1.  Only personalities 0, 1 and 2 are defined for the
      // ARM compact model, but 1 and 2 are "Long format" and may require
      // extra data words.  Hence the allowable personalities here are:
      //   personality 0, in which case 'extra' has no meaning
      //   personality 1, with zero extra words
      //   personality 2, with zero extra words
      extbl_data = NULL;
      pers  = (data >> 24) & 0x0F;
      extra = (data >> 16) & 0xFF;
      extra_allowed = 0;
   }
   else {
      // The index table entry is a pointer to the handler entry.  Note
      // that Prel31ToAddr will read the given address, but we already
      // range-checked above.
      extbl_data = Prel31ToAddr(&entry->data);
      GET_EXTAB_U32(data, extbl_data);
      if (!(data & ARM_EXIDX_COMPACT)) {
         // This denotes a "generic model" handler.  That will involve
         // executing arbitrary machine code, which is something we
         // can't represent here; hence reject it.
         return ExCantRepresent;
      }
      // So we have a compact model representation.  Again, 3 possible
      // personalities, but this time up to 255 allowable extra words.
      pers  = (data >> 24) & 0x0F;
      extra = (data >> 16) & 0xFF;
      extra_allowed = 255;
      extbl_data++;
   }

   // Now look at the handler table entry.  The first word is |data|
   // and subsequent words start at |*extbl_data|.  The number of
   // extra words to use is |extra|, provided that the personality
   // allows extra words.  Even if it does, none may be available --
   // extra_allowed is the maximum number of extra words allowed. */
   if (pers == 0) {
      // "Su16" in the documentation -- 3 unwinding insn bytes
      // |extra| has no meaning here; instead that byte is an unwind-info byte
      PUT_BUF_U8(data >> 16);
      PUT_BUF_U8(data >> 8);
      PUT_BUF_U8(data);
   }
   else if ((pers == 1 || pers == 2) && extra <= extra_allowed) {
      // "Lu16" or "Lu32" respectively -- 2 unwinding insn bytes,
      // and up to 255 extra words.
      PUT_BUF_U8(data >> 8);
      PUT_BUF_U8(data);
      UInt j;
      for (j = 0; j < extra; j++) {
         GET_EXTAB_U32(data, extbl_data);
         extbl_data++;
         PUT_BUF_U8(data >> 24);
         PUT_BUF_U8(data >> 16);
         PUT_BUF_U8(data >> 8);
         PUT_BUF_U8(data >> 0);
      }
   }
   else {
      // The entry is invalid.
      return ExInvalid;
   }

   // Make sure the entry is terminated with "FINISH"
   if (*buf_used > 0 && buf[(*buf_used) - 1] != ARM_EXTBL_OP_FINISH)
      PUT_BUF_U8(ARM_EXTBL_OP_FINISH);

   return ExSuccess;

#  undef GET_EXTAB_U32
#  undef GET_EXIDX_U32
#  undef GET_U32
#  undef PUT_BUF_U8
}


/*------------------------------------------------------------*/
/*--- (Pass 2 of 3) The EXIDX decoder                      ---*/
/*------------------------------------------------------------*/

/* This (ExtabData) is an intermediate structure, used to carry
   information from the decoder (pass 2) to the summariser (pass 3).
   I don't think its layout is important. */
typedef 
   enum {
      ARM_EXIDX_CMD_FINISH=0x100,
      ARM_EXIDX_CMD_SUB_FROM_VSP,
      ARM_EXIDX_CMD_ADD_TO_VSP,
      ARM_EXIDX_CMD_REG_POP,
      ARM_EXIDX_CMD_REG_TO_SP,
      ARM_EXIDX_CMD_VFP_POP,
      ARM_EXIDX_CMD_WREG_POP,
      ARM_EXIDX_CMD_WCGR_POP,
      ARM_EXIDX_CMD_RESERVED,
      ARM_EXIDX_CMD_REFUSED
   }
   ExtabCmd;

static const HChar* showExtabCmd ( ExtabCmd cmd ) {
   switch (cmd) {
      case ARM_EXIDX_CMD_FINISH:       return "FINISH";
      case ARM_EXIDX_CMD_SUB_FROM_VSP: return "SUB_FROM_VSP";
      case ARM_EXIDX_CMD_ADD_TO_VSP:   return "ADD_TO_VSP";
      case ARM_EXIDX_CMD_REG_POP:      return "REG_POP";
      case ARM_EXIDX_CMD_REG_TO_SP:    return "REG_TO_SP";
      case ARM_EXIDX_CMD_VFP_POP:      return "VFP_POP";
      case ARM_EXIDX_CMD_WREG_POP:     return "WREG_POP";
      case ARM_EXIDX_CMD_WCGR_POP:     return "WCGR_POP";
      case ARM_EXIDX_CMD_RESERVED:     return "RESERVED";
      case ARM_EXIDX_CMD_REFUSED:      return "REFUSED";
      default:                         return "???";
   }
}


typedef
   struct { ExtabCmd cmd; UInt data; }
   ExtabData;

static void ppExtabData ( const ExtabData* etd ) {
   VG_(printf)("ExtabData{%-12s 0x%08x}", showExtabCmd(etd->cmd), etd->data);
}


enum extab_cmd_flags {
   ARM_EXIDX_VFP_SHIFT_16 = 1 << 16,
   ARM_EXIDX_VFP_FSTMD = 1 << 17, // distinguishes FSTMxxD from FSTMxxX
};


/* Forwards */
typedef  struct _SummState  SummState;
static Int TranslateCmd(/*MOD*/SummState* state, const ExtabData* edata);


// Take the unwind information extracted by ExtabEntryExtract
// and parse it into frame-unwind instructions.  These are as
// specified in "Table 4, ARM-defined frame-unwinding instructions"
// in the specification document detailed in comments at the top
// of this file.
//
// This reads from |buf[0, +data_size)|.  It checks for overruns of
// the input buffer and returns a negative value if that happens, or
// for any other failure cases.  It returns zero in case of success.
// Whilst reading the input, it dumps the result in |*state|.
static
Int ExtabEntryDecode(/*OUT*/SummState* state, const UChar* buf, SizeT buf_size)
{
   if (buf == NULL || buf_size == 0)
      return -3;

   MemoryRange mr_in;
   Bool ok = MemoryRange__init(&mr_in, buf, buf_size);
   if (!ok)
      return -2;

#  define GET_BUF_U8(_lval) \
      do { if (!MemoryRange__covers(&mr_in, buf, 1)) \
              return -4; \
           (_lval) = *(buf++); } while (0)

   const UChar* end = buf + buf_size;

   while (buf < end) {
      ExtabData edata;
      VG_(bzero_inline)(&edata, sizeof(edata));

      UChar op;
      GET_BUF_U8(op);
      if ((op & 0xc0) == 0x00) {
         // vsp = vsp + (xxxxxx << 2) + 4
         edata.cmd  = ARM_EXIDX_CMD_ADD_TO_VSP;
         edata.data = (((Int)op & 0x3f) << 2) + 4;
      }
      else if ((op & 0xc0) == 0x40) {
         // vsp = vsp - (xxxxxx << 2) - 4
         edata.cmd  = ARM_EXIDX_CMD_SUB_FROM_VSP;
         edata.data = (((Int)op & 0x3f) << 2) + 4;
      }
      else if ((op & 0xf0) == 0x80) {
         UChar op2;
         GET_BUF_U8(op2);
         if (op == 0x80 && op2 == 0x00) {
            // Refuse to unwind
            edata.cmd = ARM_EXIDX_CMD_REFUSED;
         } else {
            // Pop up to 12 integer registers under masks {r15-r12},{r11-r4}
            edata.cmd  = ARM_EXIDX_CMD_REG_POP;
            edata.data = ((op & 0xf) << 8) | op2;
            edata.data = edata.data << 4;
         }
      }
      else if ((op & 0xf0) == 0x90) {
         if (op == 0x9d || op == 0x9f) {
            // 9d: Reserved as prefix for ARM register to register moves
            // 9f: Reserved as prefix for Intel Wireless MMX reg to reg moves
            edata.cmd = ARM_EXIDX_CMD_RESERVED;
         } else {
            // Set vsp = r[nnnn]
            edata.cmd  = ARM_EXIDX_CMD_REG_TO_SP;
            edata.data = op & 0x0f;
         }
      }
      else if ((op & 0xf0) == 0xa0) {
         // Pop r4 to r[4+nnn],          or
         // Pop r4 to r[4+nnn] and r14
         Int nnn    = (op & 0x07);
         edata.data = (1 << (nnn + 1)) - 1;
         edata.data = edata.data << 4;
         if (op & 0x08) edata.data |= 1 << 14;
         edata.cmd = ARM_EXIDX_CMD_REG_POP;
      }
      else if (op == ARM_EXTBL_OP_FINISH) {
         // Finish
         edata.cmd = ARM_EXIDX_CMD_FINISH;
         buf = end;
      }
      else if (op == 0xb1) {
         UChar op2;
         GET_BUF_U8(op2);
         if (op2 == 0 || (op2 & 0xf0)) {
            // Spare
            edata.cmd = ARM_EXIDX_CMD_RESERVED;
         } else {
            // Pop integer registers under mask {r3,r2,r1,r0}
            edata.cmd = ARM_EXIDX_CMD_REG_POP;
            edata.data = op2 & 0x0f;
         }
      }
      else if (op == 0xb2) {
         // vsp = vsp + 0x204 + (uleb128 << 2)
         ULong offset = 0;
         UChar byte, shift = 0;
         do {
            GET_BUF_U8(byte);
            offset |= (byte & 0x7f) << shift;
            shift += 7;
         } while ((byte & 0x80) && buf < end);
         edata.data = offset * 4 + 0x204;
         edata.cmd  = ARM_EXIDX_CMD_ADD_TO_VSP;
      }
      else if (op == 0xb3 || op == 0xc8 || op == 0xc9) {
         // b3: Pop VFP regs D[ssss]    to D[ssss+cccc],    FSTMFDX-ishly
         // c8: Pop VFP regs D[16+ssss] to D[16+ssss+cccc], FSTMFDD-ishly
         // c9: Pop VFP regs D[ssss]    to D[ssss+cccc],    FSTMFDD-ishly
         edata.cmd = ARM_EXIDX_CMD_VFP_POP;
         GET_BUF_U8(edata.data);
         if (op == 0xc8) edata.data |= ARM_EXIDX_VFP_SHIFT_16;
         if (op != 0xb3) edata.data |= ARM_EXIDX_VFP_FSTMD;
      }
      else if ((op & 0xf8) == 0xb8 || (op & 0xf8) == 0xd0) {
         // b8: Pop VFP regs D[8] to D[8+nnn], FSTMFDX-ishly
         // d0: Pop VFP regs D[8] to D[8+nnn], FSTMFDD-ishly
         edata.cmd  = ARM_EXIDX_CMD_VFP_POP;
         edata.data = 0x80 | (op & 0x07);
         if ((op & 0xf8) == 0xd0) edata.data |= ARM_EXIDX_VFP_FSTMD;
      }
      else if (op >= 0xc0 && op <= 0xc5) {
         // Intel Wireless MMX pop wR[10]-wr[10+nnn], nnn != 6,7
         edata.cmd  = ARM_EXIDX_CMD_WREG_POP;
         edata.data = 0xa0 | (op & 0x07);
      }
      else if (op == 0xc6) {
         // Intel Wireless MMX pop wR[ssss] to wR[ssss+cccc]
         edata.cmd = ARM_EXIDX_CMD_WREG_POP;
         GET_BUF_U8(edata.data);
      }
      else if (op == 0xc7) {
         UChar op2;
         GET_BUF_U8(op2);
         if (op2 == 0 || (op2 & 0xf0)) {
            // Spare
            edata.cmd = ARM_EXIDX_CMD_RESERVED;
         } else {
            // Intel Wireless MMX pop wCGR registers under mask {wCGR3,2,1,0}
            edata.cmd = ARM_EXIDX_CMD_WCGR_POP;
            edata.data = op2 & 0x0f;
         }
      }
      else {
         // Spare
         edata.cmd = ARM_EXIDX_CMD_RESERVED;
      }

      if (0)
         VG_(printf)("  edata:  cmd %08x  data %08x\n",
                     (UInt)edata.cmd, edata.data);

      Int ret = TranslateCmd ( state, &edata );
      if (ret < 0) return ret;
   }
   return 0;

# undef GET_BUF_U8
}


/*------------------------------------------------------------*/
/*--- (Pass 3 of 3) The EXIDX summariser                   ---*/
/*------------------------------------------------------------*/

/* In this translation into DiCfSI_m, we're going to have the CFA play
   the role of the VSP.  That means that the VSP can be exactly any of
   the CFA expressions, viz: {r7,r11,r12,r13) +/- offset.

   All of this would be a lot simpler if the DiCfSI_m representation
   was just a bit more expressive and orthogonal.  But it isn't.

   The central difficulty is that, although we can track changes
   to the offset of VSP (via vsp_off), we can't deal with assignments
   of an entirely new expression to it, because the existing 
   rules in |cfi| will almost certainly refer to the CFA, and so
   changing it will make them invalid.  Hence, below:

   * for the case ARM_EXIDX_CMD_REG_TO_SP we simply disallow 
     assignment, and hence give up, if any rule refers to CFA

   * for the case ARM_EXIDX_CMD_REG_POP, the SP (hence, VSP) is
     updated by the pop, give up.

   This is an ugly hack to work around not having a better (LUL-like)
   expression representation.  That said, these restrictions don't
   appear to be a big problem in practice.
*/

struct _SummState {
   // The DiCfSI_m under construction
   DiCfSI_m   cfi;
   Int        vsp_off;
   // For generating CFI register expressions, if needed.
   DebugInfo* di;
};


/* Generate a trivial CfiExpr, for the ARM(32) integer register
   numbered |gprNo|.  First ensure this DebugInfo has a cfsi_expr
   array in which to park it.  Returns -1 if |gprNo| cannot be
   represented, otherwise returns a value >= 0. */
static
Int gen_CfiExpr_CfiReg_ARM_GPR ( /*MB_MOD*/DebugInfo* di, UInt gprNo )
{
   CfiReg creg = Creg_INVALID;
   switch (gprNo) {
      case 13: creg = Creg_ARM_R13; break;
      case 12: creg = Creg_ARM_R12; break;
      case 15: creg = Creg_ARM_R15; break;
      case 14: creg = Creg_ARM_R14; break;
      case 7:  creg = Creg_ARM_R7;  break;
      default: break;
   }
   if (creg == Creg_INVALID) {
      return -1;
   }
   if (!di->cfsi_exprs) {
      di->cfsi_exprs = VG_(newXA)( ML_(dinfo_zalloc), "di.gCCAG",
                                   ML_(dinfo_free), sizeof(CfiExpr) );
   }
   Int res = ML_(CfiExpr_CfiReg)( di->cfsi_exprs, creg );
   vg_assert(res >= 0);
   return res;
}


/* Given a DiCfSI_m, find the _how/_off pair for the given ARM(32) GPR
   number inside |cfsi_m|, or return NULL for both if that register
   number is not represented. */
static
void maybeFindExprForRegno( /*OUT*/UChar** howPP, /*OUT*/Int** offPP,
                            DiCfSI_m* cfsi_m, Int regNo )
{
   switch (regNo) {
      case 15: *howPP = &cfsi_m->ra_how;  *offPP = &cfsi_m->ra_off;  return;
      case 14: *howPP = &cfsi_m->r14_how; *offPP = &cfsi_m->r14_off; return;
      case 13: *howPP = &cfsi_m->r13_how; *offPP = &cfsi_m->r13_off; return;
      case 12: *howPP = &cfsi_m->r12_how; *offPP = &cfsi_m->r12_off; return;
      case 11: *howPP = &cfsi_m->r11_how; *offPP = &cfsi_m->r11_off; return;
      case 7:  *howPP = &cfsi_m->r7_how;  *offPP = &cfsi_m->r7_off;  return;
      default: break;
   }
   *howPP = NULL; *offPP = NULL;
}


/* Set cfi.cfa_{how,off} so as to be a copy of the expression denoted
   by (how,off), if it is possible to do so.  Returns True on
   success. */
static
Bool setCFAfromCFIR( /*MOD*/DiCfSI_m* cfi, XArray*/*CfiExpr*/ cfsi_exprs,
                     UChar how, Int off )
{
   switch (how) {
      case CFIR_EXPR:
         if (!cfsi_exprs) return False;
         CfiExpr* e = (CfiExpr*)VG_(indexXA)(cfsi_exprs, off);
         if (e->tag != Cex_CfiReg) return False;
         if (e->Cex.CfiReg.reg == Creg_ARM_R7) {
            cfi->cfa_how = CFIC_ARM_R7REL;
            cfi->cfa_off = 0;
            return True;
         }
         ML_(ppCfiExpr)(cfsi_exprs, off);
         vg_assert(0);
      default:
         break;
   }
   VG_(printf)("setCFAfromCFIR: FAIL: how %d off %d\n", how, off);
   vg_assert(0);
   return False;
}


#define ARM_EXBUF_START(x) (((x) >> 4) & 0x0f)
#define ARM_EXBUF_COUNT(x) ((x) & 0x0f)
#define ARM_EXBUF_END(x)   (ARM_EXBUF_START(x) + ARM_EXBUF_COUNT(x))


static Bool mentionsCFA ( DiCfSI_m* cfi )
{
#  define MENTIONS_CFA(_how) ((_how) == CFIR_CFAREL || (_how) == CFIR_MEMCFAREL)
   if (MENTIONS_CFA(cfi->ra_how))  return True;
   if (MENTIONS_CFA(cfi->r14_how)) return True;
   if (MENTIONS_CFA(cfi->r13_how)) return True;
   if (MENTIONS_CFA(cfi->r12_how)) return True;
   if (MENTIONS_CFA(cfi->r11_how)) return True;
   if (MENTIONS_CFA(cfi->r7_how))  return True;
   return False;
#  undef MENTIONS_CFA
}


// Translate command from extab_data to command for Module.
static
Int TranslateCmd(/*MOD*/SummState* state, const ExtabData* edata)
{
   /* Stay sane: check that the CFA has the expected form. */
   vg_assert(state);
   switch (state->cfi.cfa_how) {
      case CFIC_ARM_R13REL: case CFIC_ARM_R12REL:
      case CFIC_ARM_R11REL: case CFIC_ARM_R7REL: break;
      default: vg_assert(0);
   }

   if (0) {
      VG_(printf)("  TranslateCmd: ");
      ppExtabData(edata);
      VG_(printf)("\n");
   }

   Int ret = 0;
   switch (edata->cmd) {
      case ARM_EXIDX_CMD_FINISH:
         /* Copy LR to PC if there isn't currently a rule for PC in force. */
         if (state->cfi.ra_how == CFIR_UNKNOWN) {
            if (state->cfi.r14_how == CFIR_UNKNOWN) {
               state->cfi.ra_how = CFIR_EXPR;
               state->cfi.ra_off = gen_CfiExpr_CfiReg_ARM_GPR(state->di, 14);
               vg_assert(state->cfi.ra_off >= 0);
            } else {
               state->cfi.ra_how = state->cfi.r14_how;
               state->cfi.ra_off = state->cfi.r14_off;
            }
         }
         break;
      case ARM_EXIDX_CMD_SUB_FROM_VSP:
         state->vsp_off -= (Int)(edata->data);
         break;
      case ARM_EXIDX_CMD_ADD_TO_VSP:
         state->vsp_off += (Int)(edata->data);
         break;
      case ARM_EXIDX_CMD_REG_POP: {
         UInt i;
         for (i = 0; i < 16; i++) {
            if (edata->data & (1 << i)) {
               // See if we're summarising for int register |i|.  If so,
               // describe how to pull it off the stack.  The cast of |i| is
               // a bit of a kludge but works because DW_REG_ARM_Rn has the
               // value |n|, for 0 <= |n| <= 15 -- that is, for the ARM 
               // general-purpose registers.
               UChar* rX_howP = NULL;
               Int*   rX_offP = NULL;
               maybeFindExprForRegno(&rX_howP, &rX_offP, &state->cfi, i);
               if (rX_howP) {
                  vg_assert(rX_offP);
                  /* rX_howP and rX_offP point at one of the rX fields
                     in |state->cfi|.  Hence the following assignments
                     are really updating |state->cfi|. */
                  *rX_howP = CFIR_MEMCFAREL;
                  *rX_offP = state->vsp_off;
               } else {
                  /* We're not tracking this register, so ignore it. */
                  vg_assert(!rX_offP);
               }
               state->vsp_off += 4;
            }
         }
         /* Set cfa in case the SP got popped. */
         if (edata->data & (1 << 13)) {
            //  vsp = curr_rules_.mR13expr;
            //state->cfi.cfa_how = 
            //state->cfi.cfa_off = 
            //state->vsp_off = 0;
            // If this happens, it would make the existing CFA references
            // in the summary invalid.  So give up instead.
            goto cant_summarise;
         }
         break;
         }
      case ARM_EXIDX_CMD_REG_TO_SP: {
         /* We're generating a new value for the CFA/VSP here.  Hence,
            if the summary already refers to the CFA at all, we can't
            go any further, and have to abandon summarisation. */
         if (mentionsCFA(&state->cfi))
            goto cant_summarise;
         vg_assert(edata->data < 16);
         Int reg_no = edata->data;
         // Same comment as above, re the casting of |reg_no|, applies.
         UChar* rX_howP = NULL;
         Int*   rX_offP = NULL;
         maybeFindExprForRegno(&rX_howP, &rX_offP, &state->cfi, reg_no);
         if (rX_howP) {
            vg_assert(rX_offP);
            if (*rX_howP == CFIR_UNKNOWN) {
               //curr_rules_.mR13expr = LExpr(LExpr::NODEREF, reg_no, 0);
               Int expr_ix = gen_CfiExpr_CfiReg_ARM_GPR(state->di, reg_no);
               if (expr_ix >= 0) {
                  state->cfi.r13_how = CFIR_EXPR;
                  state->cfi.r13_off = expr_ix;
               } else {
                  goto cant_summarise;
               }
            } else {
               //curr_rules_.mR13expr = *reg_exprP;
               state->cfi.r13_how = *rX_howP;
               state->cfi.r13_off = *rX_offP;
            }
            //vsp = curr_rules_.mR13expr;
            Bool ok = setCFAfromCFIR( &state->cfi, state->di->cfsi_exprs,
                                      state->cfi.r13_how, state->cfi.r13_off );
            if (!ok) goto cant_summarise;
            state->vsp_off = 0;
         } else {
            vg_assert(!rX_offP);
         }
         break;
      }
      case ARM_EXIDX_CMD_VFP_POP: {
         /* Don't recover VFP registers, but be sure to adjust the stack
            pointer. */
         UInt i;
         for (i = ARM_EXBUF_START(edata->data);
              i <= ARM_EXBUF_END(edata->data); i++) {
            state->vsp_off += 8;
         }
         if (!(edata->data & ARM_EXIDX_VFP_FSTMD)) {
            state->vsp_off += 4;
         }
         break;
      }
      case ARM_EXIDX_CMD_WREG_POP: {
         UInt i;
         for (i = ARM_EXBUF_START(edata->data);
              i <= ARM_EXBUF_END(edata->data); i++) {
            state->vsp_off += 8;
         }
         break;
      }
      case ARM_EXIDX_CMD_WCGR_POP: {
         UInt i;
         // Pop wCGR registers under mask {wCGR3,2,1,0}, hence "i < 4"
         for (i = 0; i < 4; i++) {
            if (edata->data & (1 << i)) {
               state->vsp_off += 4;
            }
         }
         break;
      }
      case ARM_EXIDX_CMD_REFUSED:
      case ARM_EXIDX_CMD_RESERVED:
         ret = -1;
         break;
   }
   return ret;

 cant_summarise:
   return -10;
}


/* Initialise the EXIDX summariser, by writing initial values in |state|. */
static
void AddStackFrame ( /*OUT*/SummState* state,
                     DebugInfo* di )
{
   VG_(bzero_inline)(state, sizeof(*state));
   state->vsp_off = 0;
   state->di      = di;
   /* Initialise the DiCfSI_m that we are building. */
   state->cfi.cfa_how = CFIC_ARM_R13REL;
   state->cfi.cfa_off = 0;
   state->cfi.ra_how  = CFIR_UNKNOWN;
   state->cfi.r14_how = CFIR_UNKNOWN;
   state->cfi.r13_how = CFIR_UNKNOWN;
   state->cfi.r12_how = CFIR_UNKNOWN;
   state->cfi.r11_how = CFIR_UNKNOWN;
   state->cfi.r7_how  = CFIR_UNKNOWN;
}

static
void SubmitStackFrame( /*MOD*/DebugInfo* di,
                       SummState* state, Addr avma, SizeT len )
{
   // JRS: I'm really not sure what this means, or if it is necessary
   // return address always winds up in pc
   //stack_frame_entry_->initial_rules[ustr__ZDra()] // ".ra"
   //  = stack_frame_entry_->initial_rules[ustr__pc()];
   // maybe don't need to do anything here?

   // the final value of vsp is the new value of sp.
   switch (state->cfi.cfa_how) {
      case CFIC_ARM_R13REL: case CFIC_ARM_R12REL:
      case CFIC_ARM_R11REL: case CFIC_ARM_R7REL: break;
      default: vg_assert(0);
   }
   state->cfi.r13_how = CFIR_CFAREL;
   state->cfi.r13_off = state->vsp_off;

   // Finally, add the completed RuleSet to the SecMap
   if (len > 0) {

      // Futz with the rules for r4 .. r11 in the same way as happens
      // with the CFI summariser:
      /* Mark callee-saved registers (r4 .. r11) as unchanged, if there is
       no other information about them.  FIXME: do this just once, at
       the point where the ruleset is committed. */
      if (state->cfi.r7_how == CFIR_UNKNOWN) {
         state->cfi.r7_how = CFIR_SAME;
         state->cfi.r7_off = 0;
      }
      if (state->cfi.r11_how == CFIR_UNKNOWN) {
         state->cfi.r11_how = CFIR_SAME;
         state->cfi.r11_off = 0;
      }
      if (state->cfi.r12_how == CFIR_UNKNOWN) {
         state->cfi.r12_how = CFIR_SAME;
         state->cfi.r12_off = 0;
      }
      if (state->cfi.r14_how == CFIR_UNKNOWN) {
         state->cfi.r14_how = CFIR_SAME;
         state->cfi.r14_off = 0;
      }

      // And add them
      ML_(addDiCfSI)(di, avma, len, &state->cfi);
      if (di->trace_cfi)
         ML_(ppDiCfSI)(di->cfsi_exprs, avma, len, &state->cfi);
   }
}


/*------------------------------------------------------------*/
/*--- Top level                                            ---*/
/*------------------------------------------------------------*/

void ML_(read_exidx) ( /*MOD*/DebugInfo* di,
                       UChar*   exidx_img, SizeT exidx_size,
                       UChar*   extab_img, SizeT extab_size,
                       Addr     text_last_svma,
                       PtrdiffT text_bias )
{
   if (di->trace_cfi)
      VG_(printf)("BEGIN ML_(read_exidx) exidx_img=[%p, +%lu) "
                  "extab_img=[%p, +%lu) text_last_svma=%lx text_bias=%lx\n",
                  exidx_img, exidx_size, extab_img, extab_size,
                  text_last_svma, (UWord)text_bias);
   Bool ok;
   MemoryRange mr_exidx, mr_extab;
   ok =       MemoryRange__init(&mr_exidx, exidx_img, exidx_size);
   ok = ok && MemoryRange__init(&mr_extab, extab_img, extab_size);
   if (!ok) {
      complain(".exidx or .extab image area wraparound");
      return;
   }

   const ExidxEntry* start_img = (const ExidxEntry*)exidx_img;
   const ExidxEntry* end_img   = (const ExidxEntry*)(exidx_img + exidx_size);

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "  Reading EXIDX entries: %lu available\n",
                   exidx_size / sizeof(ExidxEntry) );

   // Iterate over each of the EXIDX entries (pairs of 32-bit words).
   // These occupy the entire .exidx section.
   UWord n_attempted = 0, n_successful = 0;

   const ExidxEntry* entry_img;
   for (entry_img = start_img; entry_img < end_img; ++entry_img) {

      n_attempted++;
      // Figure out the code address range that this table entry_img is
      // associated with.
      Addr avma = (Addr)Prel31ToAddr(&entry_img->addr);
      if (di->trace_cfi)
         VG_(printf)("XXX1 entry: entry->addr 0x%x, avma 0x%lx\n",
                     entry_img->addr, avma);

      Addr next_avma;
      if (entry_img < end_img - 1) {
         next_avma = (Addr)Prel31ToAddr(&(entry_img+1)->addr);
      } else {
         // This is the last EXIDX entry in the sequence, so we don't
         // have an address for the start of the next function, to limit
         // this one.  Instead use the address of the last byte of the
         // text section associated with this .exidx section, that we
         // have been given.  So as to avoid junking up the CFI unwind
         // tables with absurdly large address ranges in the case where
         // text_last_svma_ is wrong, only use the value if it is nonzero
         // and within one page of |svma|.  Otherwise assume a length of 1.
         //
         // In some cases, gcc has been observed to finish the exidx
         // section with an entry of length 1 marked CANT_UNWIND,
         // presumably exactly for the purpose of giving a definite
         // length for the last real entry, without having to look at
         // text segment boundaries.
         Addr text_last_avma = text_last_svma + text_bias;

         Bool plausible;
         Addr maybe_next_avma = text_last_avma + 1;
         if (maybe_next_avma > avma && maybe_next_avma - avma <= 4096) {
            next_avma = maybe_next_avma;
            plausible = True;
         } else {
            next_avma = avma + 1;
            plausible = False;
         }

         if (!plausible && avma != text_last_avma + 1) {
            HChar buf[100];
            VG_(snprintf)(buf, sizeof(buf),
                          "Implausible EXIDX last entry size %lu"
                          "; using 1 instead.", text_last_avma - avma);
            buf[sizeof(buf)-1] = 0;
            complain(buf);
         }
      }

      // Extract the unwind info into |buf|.  This might fail for
      // various reasons.  It involves reading both the .exidx and
      // .extab sections.  All accesses to those sections are
      // bounds-checked.
      if (di->trace_cfi)
         VG_(printf)("XXX1 entry is for AVMA 0x%lx 0x%lx\n",
                     avma, next_avma-1);
      UChar buf[ARM_EXIDX_TABLE_LIMIT];
      SizeT buf_used = 0;
      ExExtractResult res
         = ExtabEntryExtract(&mr_exidx, &mr_extab,
                             entry_img, buf, sizeof(buf), &buf_used);
      if (res != ExSuccess) {
         // Couldn't extract the unwind info, for some reason.  Move on.
         switch (res) {
            case ExInBufOverflow:
               complain("ExtabEntryExtract: .exidx/.extab section overrun");
               break;
            case ExOutBufOverflow:
               complain("ExtabEntryExtract: bytecode buffer overflow");
               break;
            case ExCantUnwind:
               // Some functions are marked CantUnwind by the compiler.
               // Don't record these as attempted, since that's just 
               // confusing, and failure to summarise them is not the fault
               // of this code.
               n_attempted--;
               if (0)
                  complain("ExtabEntryExtract: function is marked CANT_UNWIND");
               break;
            case ExCantRepresent:
               complain("ExtabEntryExtract: bytecode can't be represented");
               break;
            case ExInvalid:
               complain("ExtabEntryExtract: index table entry is invalid");
               break;
            default: {
               HChar mbuf[100];
               VG_(snprintf)(mbuf, sizeof(mbuf),
                             "ExtabEntryExtract: unknown error: %d", (Int)res);
               buf[sizeof(mbuf)-1] = 0;
               complain(mbuf);
               break;
            }
         }
         continue;
      }

      // Finally, work through the unwind instructions in |buf| and
      // create CFI entries that Valgrind can use.  This can also fail.
      // First, initialise the summariser's running state, into which
      // ExtabEntryDecode will write the CFI entries.

      SummState state;
      AddStackFrame( &state, di );
      Int ret = ExtabEntryDecode( &state, buf, buf_used );
      if (ret < 0) {
         /* Failed summarisation.  Ignore and move on. */
         HChar mbuf[100];
         VG_(snprintf)(mbuf, sizeof(mbuf),
                       "ExtabEntryDecode: failed with error code: %d", ret);
         mbuf[sizeof(mbuf)-1] = 0;
         complain(mbuf);
      } else {
         /* Successful summarisation.  Add it to the collection. */
         SubmitStackFrame( di, &state, avma, next_avma - avma );
         n_successful++;
      }

   } /* iterating over .exidx */

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg,
                   "  Reading EXIDX entries: %lu attempted, %lu successful\n",
                   n_attempted, n_successful);
}

#endif /* defined(VGA_arm) */

/*--------------------------------------------------------------------*/
/*--- end                                              readexidx.c ---*/
/*--------------------------------------------------------------------*/
