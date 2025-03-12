/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024-2025  Florian Krohm

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

#include <stdio.h>        // printf
#include <string.h>       // strlen
#include <stdlib.h>       // free
#include <assert.h>       // assert
#include <ctype.h>        // isdigit
#include "main.h"         // error

/* Each line in the initialiser is an opcode specification which
   consists of two parts: opcode name and opcode operands.
   Name and operands are separated by white space. Operands are
   separated by ','.

   Operands should not be confused with fields. E.g. the "bc"
   opcode has 2 operands (m1 and d12(x2,b2)) but 4 fields (m,d,x,b).

   The following naming conventions are observed:

   - 'r[0-9]+' denotes a GPR (4-bit wide unsigned value)
   - 'b[0-9]+' denotes a GPR used as base register
   - 'x[0-9]+' denotes a GPR used as index register
   - 'v[0-9]+' denotes a VR (5-bit wide unsigned value)
   - 'm[0-9]+' denotes a 4-bit mask (an unsigned value)
   - 'l'       denotes an 8-bit length (an unsigned value)
   - 'l[0-9]+' denotes an 8-bit length (an unsigned value)
   - 'i[0-9]+' denotes an integer
     You must specify #bits and signedness like so:
       i2:8u  meaning: 8-bit wide, unsigned integer contents
       i3:12s meaning: 12-bit wide, signed integer contents
   - 'ri[0-9]+' denotes an integer that is used for PC-relative
     addressing. You must specify #bits and signedness.
   - 'd12' denotes a 12-bit wide displacement (an unsigned integer)
   - 'd20' denotes a 20-bit wide displacement (a signed integer)

   It is also possible to restrict the values that may be assigned to an
   operand (typically a mask). This is heavily used by vector opcodes.

   Example:
     m4:{ 0..4 }       values 0,1,2,3,4 are allowed
     m4:{ 0,1,2,3,4 }  same as above
     m5:{ 0,1,3..7 }   values 0,1,3,4,5,6,7 are allowed

   If you need to specify #bits, signedness and allowed values you
   need to provide those in the order: signedness, #bits, allowed
   values. E.g. i2:s8{-10..42}
*/
static const char *opcodes[] = {
   /* Unsorted list of opcodes (other than vector ops)  with
      extended mnemonics. See also:
      Appendix J, Principles of Operation */
   "bic    m1,d20(x2,b2)",
   "bcr    m1,r2",
   "bc     m1,d12(x2,b2)",
   "bras   r1,ri2:s16",
   "brasl  r1,ri2:s32",
   "brc    m1,ri2:s16",
   "brcl   m1,ri2:s32",
   "brct   r1,ri2:s16",
   "brctg  r1,ri2:s16",
   "brcth  r1,ri2:s32",
   "brxh   r1,r3,ri2:s16",
   "brxhg  r1,r3,ri2:s16",
   "brxle  r1,r3,ri2:s16",
   "brxlg  r1,r3,ri2:s16",
   "crb    r1,r2,m3,d12(b4)",
   "cgrb   r1,r2,m3,d12(b4)",
   "crj    r1,r2,m3,ri4:s16",
   "cgrj   r1,r2,m3,ri4:s16",
   "crt    r1,r2,m3",
   "cgrt   r1,r2,m3",
   "cib    r1,i2:s8,m3,d12(b4)",
   "cgib   r1,i2:s8,m3,d12(b4)",
   "cij    r1,i2:s8,m3,ri4:s16",
   "cgij   r1,i2:s8,m3,ri4:s16",
   "cit    r1,i2:s16,m3",
   "cgit   r1,i2:s16,m3",
   "clrb   r1,r2,m3,d12(b4)",
   "clgrb  r1,r2,m3,d12(b4)",
   "clrj   r1,r2,m3,ri4:s16",
   "clgrj  r1,r2,m3,ri4:s16",
   "clrt   r1,r2,m3",
   "clgrt  r1,r2,m3",
   "clt    r1,m3,d20(b2)",
   "clgt   r1,m3,d20(b2)",
   "clib   r1,i2:u8,m3,d12(b4)",
   "clgib  r1,i2:u8,m3,d12(b4)",
   "clij   r1,i2:u8,m3,ri4:s16",
   "clgij  r1,i2:u8,m3,ri4:s16",
   "clfit  r1,i2:u16,m3",
   "clgit  r1,i2:u16,m3",
   "iilf   r1,i2:u32",
   "lochhi r1,i2:s16,m3",
   "lochi  r1,i2:s16,m3",
   "locghi r1,i2:s16,m3",
   "locfhr r1,r2,m3",
   "locfh  r1,d20(b2),m3",
   "llilf  r1,i2:u32",
   "locr   r1,r2,m3",
   "locgr  r1,r2,m3",
   "loc    r1,d20(b2),m3",
   "locg   r1,d20(b2),m3",
   "nork   r1,r2,r3",
   "nogrk  r1,r2,r3",
   "rnsbg  r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  un/signed  i3/4/5 ? t-bit ? z-bit?
   "rxsbg  r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "risbg  r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "risbgn r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "risbhg r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "risblg r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "rosbg  r1,r2,i3:u8,i4:u8,i5:u8",  // FIXME  ditto
   "selr   r1,r2,r3,m4",
   "selgr  r1,r2,r3,m4",
   "selfhr r1,r2,r3,m4",
   "stocfh r1,d20(b2),m3",
   "stoc   r1,d20(b2),m3",
   "stocg  r1,d20(b2),m3",

   /* Misc. other opcodes */
   "cksm  r1,r2",   // make sure, it gets disassmbled (BZ 495817)
   "clcl  r1,r2",   // make sure, it gets disassmbled (BZ 495817)
   "mvcl  r1,r2",   // make sure, it gets disassmbled (BZ 495817)

   // If a set of allowed values is specified for an operand this
   // implies that any other value would cause a specification exception.
   // UNLESS otherwise noted.

   // Chapter 21: Vector Overview and Support Instructions
   "vbperm  v1,v2,v3",
   "vgef    v1,d12(v2,b2),m3:{0,1,2,3}",
   "vgeg    v1,d12(v2,b2),m3:{0,1}",
   "vgbm    v1,i2:u16",
   "vgm     v1,i2:u8,i3:u8,m4:{0..3}",
   "vl      v1,d12(x2,b2),m3:{0,3,4}",    // no spec. exc
   "vlr     v1,v2",
   "vlrep   v1,d12(x2,b2),m3:{0..3}",
   "vlebrh  v1,d12(x2,b2),m3:{0..7}",
   "vlebrf  v1,d12(x2,b2),m3:{0..3}",
   "vlebrg  v1,d12(x2,b2),m3:{0,1}",
   "vlbrrep v1,d12(x2,b2),m3:{1..3}",
   "vllebrz v1,d12(x2,b2),m3:{1..3,6}",
   "vlbr    v1,d12(x2,b2),m3:{1..4}",
   "vleb    v1,d12(x2,b2),m3",
   "vleh    v1,d12(x2,b2),m3:{0..7}",
   "vlef    v1,d12(x2,b2),m3:{0..3}",
   "vleg    v1,d12(x2,b2),m3:{0,1}",
   "vleib   v1,i2:s16,m3",
   "vleih   v1,i2:s16,m3:{0..7}",
   "vleif   v1,i2:s16,m3:{0..3}",
   "vleig   v1,i2:s16,m3:{0,1}",
   "vler    v1,d12(x2,b2),m3:{0..3}",
   "vlgv    r1,v3,d12(b2),m4:{0..3}",
   "vllez   v1,d12(x2,b2),m3:{0..3,6}",
   //   "vlm     v1,v3,d12(b2),m4",  // cannot express constraint
   "vlrlr   v1,r3,d12(b2)",
   "vlrl    v1,d12(b2),i3:u8{0..15}",
   "vlbb    v1,d12(x2,b2),m3:{0..6}",
   "vlvg    v1,r3,d12(b2),m4:{0..3}",
   "vlvgp   v1,r2,r3",
   "vll     v1,r3,d12(b2)",
   "vmrh    v1,v2,v3,m4:{0..3}",
   "vmrl    v1,v2,v3,m4:{0..3}",
   "vpk     v1,v2,v3,m4:{1..3}",
   "vpks    v1,v2,v3,m4:{1..3},m5:{0,1}", // no spec. exception for m5
   "vpkls   v1,v2,v3,m4:{1..3},m5:{0,1}", // no spec. exception for m5
   "vperm   v1,v2,v3,v4",
   "vpdi    v1,v2,v3,m4",
   "vrep    v1,v3,i2:u16,m4:{0..3}",
   "vrepi   v1,i2:s16,m3:{0..3}",
   "vscef   v1,d12(v2,b2),m3:{0..3}",
   "vsceg   v1,d12(v2,b2),m3:{0,1}",
   "vsel    v1,v2,v3,v4",
   "vseg    v1,v2,m3:{0..2}",
   "vst     v1,d12(x2,b2),m3",
   "vstebrh v1,d12(x2,b2),m3:{0..7}",
   "vstebrf v1,d12(x2,b2),m3:{0..3}",
   "vstebrg v1,d12(x2,b2),m3:{0,1}",
   "vstbr   v1,d12(x2,b2),m3:{1..4}",
   "vsteb   v1,d12(x2,b2),m3",
   "vsteh   v1,d12(x2,b2),m3:{0..7}",
   "vstef   v1,d12(x2,b2),m3:{0..3}",
   "vsteg   v1,d12(x2,b2),m3:{0,1}",
   "vster   v1,d12(x2,b2),m3:{1..3}",
   //   "vstm    v1,v3,d12(b2),m4",  // cannot express constraint
   "vstrlr  v1,r3,d12(b2)",
   "vstrl   v1,d12(b2),i3:u8{0..15}",
   "vstl    v1,r3,d12(b2)",
   "vuph    v1,v2,m3:{0..2}",
   "vuplh   v1,v2,m3:{0..2}",
   "vupl    v1,v2,m3:{0..2}",
   "vupll   v1,v2,m3:{0..2}",

   // Chapter 22: Vector Integer Instructions
   "va      v1,v2,v3,m4:{0..4}",
   "vacc    v1,v2,v3,m4:{0..4}",
   "vac     v1,v2,v3,v4,m5:{4}",
   "vaccc   v1,v2,v3,v4,m5:{4}",
   "vn      v1,v2,v3",
   "vnc     v1,v2,v3",
   "vavg    v1,v2,v3,m4:{0..3}",
   "vavgl   v1,v2,v3,m4:{0..3}",
   "vcksm   v1,v2,v3",
   "vec     v1,v2,m3:{0..3}",
   "vecl    v1,v2,m3:{0..3}",
   "vceq    v1,v2,v3,m4:{0..3},m5:{0,1}",  // no spec. exception for m5
   "vch     v1,v2,v3,m4:{0..3},m5:{0,1}",  // no spec. exception for m5
   "vchl    v1,v2,v3,m4:{0..3},m5:{0,1}",  // no spec. exception for m5
   "vclz    v1,v2,m3:{0..3}",
   "vctz    v1,v2,m3:{0..3}",
   "vx      v1,v2,v3",
   "vgfm    v1,v2,v3,m4:{0..3}",
   "vgfma   v1,v2,v3,v4,m5:{0..3}",
   "vlc     v1,v2,m3:{0..3}",
   "vlp     v1,v2,m3:{0..3}",
   "vmx     v1,v2,v3,m4:{0..3}",
   "vmxl    v1,v2,v3,m4:{0..3}",
   "vmn     v1,v2,v3,m4:{0..3}",
   "vmnl    v1,v2,v3,m4:{0..3}",
   "vmal    v1,v2,v3,v4,m5:{0..2}",
   "vmah    v1,v2,v3,v4,m5:{0..2}",
   "vmalh   v1,v2,v3,v4,m5:{0..2}",
   "vmae    v1,v2,v3,v4,m5:{0..2}",
   "vmale   v1,v2,v3,v4,m5:{0..2}",
   "vmao    v1,v2,v3,v4,m5:{0..2}",
   "vmalo   v1,v2,v3,v4,m5:{0..2}",
   "vmh     v1,v2,v3,m4:{0..2}",
   "vmlh    v1,v2,v3,m4:{0..2}",
   "vml     v1,v2,v3,m4:{0..2}",
   "vme     v1,v2,v3,m4:{0..2}",
   "vmle    v1,v2,v3,m4:{0..2}",
   "vmo     v1,v2,v3,m4:{0..2}",
   "vmlo    v1,v2,v3,m4:{0..2}",
   "vmsl    v1,v2,v3,v4,m5:{3},m6:{0,4,8,12}",   // no spec. exception for m6
   "vnn     v1,v2,v3",
   "vno     v1,v2,v3",
   "vnx     v1,v2,v3",
   "vo      v1,v2,v3",
   "voc     v1,v2,v3",
   "vpopct  v1,v2,m3:{0..3}",
   "verllv  v1,v2,v3,m4:{0..3}",
   "verll   v1,v3,d12(b2),m4:{0..3}",
   "verim   v1,v2,v3,i4:u8,m5:{0..3}",
   "veslv   v1,v2,v3,m4:{0..3}",
   "vesl    v1,v3,d12(b2),m4:{0..3}",
   "vesrav  v1,v2,v3,m4:{0..3}",
   "vesra   v1,v3,d12(b2),m4:{0..3}",
   "vesrlv  v1,v2,v3,m4:{0..3}",
   "vesrl   v1,v3,d12(b2),m4:{0..3}",
   "vsl     v1,v2,v3",
   "vslb    v1,v2,v3",
   "vsld    v1,v2,v3,i4:u8{0..7}",   // spec exc.
   "vsldb   v1,v2,v3,i4:u8{0..15}",  // otherwise unpredictable
   "vsra    v1,v2,v3",
   "vsrab   v1,v2,v3",
   "vsrd    v1,v2,v3,i4:u8{0..7}",
   "vsrl    v1,v2,v3",
   "vsrlb   v1,v2,v3",
   "vs      v1,v2,v3,m4:{0..4}",
   "vscbi   v1,v2,v3,m4:{0..4}",
   "vsbi    v1,v2,v3,v4,m5:{4}",
   "vsbcbi  v1,v2,v3,v4,m5:{4}",
   "vsumg   v1,v2,v3,m4:{1,2}",
   "vsumq   v1,v2,v3,m4:{2,3}",
   "vsum    v1,v2,v3,m4:{0,1}",
   "vtm     v1,v2",

   // Chapter 23: Vector String Instructions
   "vfae    v1,v2,v3,m4:{0..2},m5",
   "vfee    v1,v2,v3,m4:{0..2},m5:{0..3}",
   "vfene   v1,v2,v3,m4:{0..2},m5:{0..3}",
   "vistr   v1,v2,m3:{0..2},m5:{0,1}",
   "vstrc   v1,v2,v3,v4,m5:{0..2},m6",
   "vstrs   v1,v2,v3,v4,m5:{0..2},m6:{0,2}",

   // Chapter 24: Vector Floating-Point Instructions
   "vfa    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "wfc    v1,v2,m3:{2,3,4},m4:{0}",
   "wfk    v1,v2,m3:{2,3,4},m4:{0}",
   "vfce   v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vfch   v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vfche  v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vcfps  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcdg
   "vcdg   v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcfps
   "vcfpl  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcdlg
   "vcdlg  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcfpl
   "vcsfp  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcgd
   "vcgd   v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcsfp
   "vclfp  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vclgd
   "vclgd  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vclfp
   "vfd    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vfi    v1,v2,m3:{2,3,4},m4:{0,4,8,12},m5:{0,1,3..7}",
   "vfll   v1,v2,m3:{2,3},m4:{0,8}",
   "vflr   v1,v2,m3:{3,4},m4:{0,4,8,12},m5:{0,1,3..7}",
   "vfmax  v1,v2,v3,m4:{2,3,4},m5:{0,8},m6:{0..4,8..12}",
   "vfmin  v1,v2,v3,m4:{2,3,4},m5:{0,8},m6:{0..4,8..12}",
   "vfm    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vfma   v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfms   v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfnma  v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfnms  v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfpso  v1,v2,m3:{2,3,4},m4:{0,8},m5:{0..2}",
   "vfsq   v1,v2,m3:{2,3,4},m4:{0,8}",
   "vfs    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vftci  v1,v2,i3:u12,m4:{2,3,4},m5:{0,8}",

   // Chapter 25: Vector Decimal Instructions

   //  not implemented

   // Chapter 26: Specialized-Function-Assist Instructions

   //   "kdsa   r1,r2",    // cannot express constraint
   //   "dfltcc r1,r2,r3", // cannot express constraint
   //   "nnpa",            // cannot express constraint
   //   "sortl  r1,r2",    // not implemented
   "vclfnh  v1,v2,m3,m4",      // FIXME: m3:{2}  m4:{0}  no spec but IEEE exc.
   "vclfnl  v1,v2,m3,m4",      // FIXME: m3:{2}  m4:{0}  no spec but IEEE exc.
   "vcrnf   v1,v2,v3,m4,m5",   // FIXME: m4:{0}  m5:{2}  no spec but IEEE exc.
   "vcfn    v1,v2,m3,m4",      // FIXME: m3:{0}  m4:{1}  no spec but IEEE exc.
   "vcnf    v1,v2,m3,m4",      // FIXME: m3:{0}  m4:{1}  no spec but IEEE exc.
};

unsigned num_opcodes = sizeof opcodes / sizeof *opcodes;


static const char *
skip_digits(const char *p)
{
   while (isdigit(*p))
      ++p;
   return p;
}


/* Parse an integer. If not found return initial value of P. */
static const char *
parse_int(const char *p, int is_unsigned, long long *val)
{
   if (sscanf(p, "%lld", val) != 1) {
      error("integer expected\n");
      return p;
   }
   if (is_unsigned && val < 0) {
      error("unsigned value expected\n");
      return p;
   }
   return skip_digits(val < 0 ? p + 1 : p);
}


/* Parse a range of integers. Upon errors return initial value of P. */
static const char *
parse_range(const char *p, int is_unsigned, long long *from,
            long long *to)
{
   if (sscanf(p, "%lld..%lld", from, to) == 2) {
      if (*from > *to) {
         error("range %lld..%lld is not ascending\n", *from, *to);
         return p;
      }
      if (is_unsigned && (*from < 0 || *to < 0)) {
         error("unsigned value expected\n");
         return p;
      }
      p = strstr(p, "..") + 2;
      return skip_digits(*to < 0 ? p + 1 : p);
   }
   return NULL;  // not a range of values
}


/* An element is either a single integer or a range of integers
   e.g. 3..6
   Note, the function recurses which is a neat trick to avoid
   having to realloc the array for the values.
   P points to an integer; if not syntax error.
   The function returns NULL upon encounterning syntax errors. */
static long long *
consume_elements(const char *p, int is_unsigned, int num_val)
{
   const char *begin = p;
   long long from, to, val;
   long long *values;

   /* Try range first. */
   p = parse_range(begin, is_unsigned, &from, &to);
   if (p) {
      if (p == begin)   // errors
         return NULL;
      if (to < from) {
         error("range %lld..%lld is not ascending\n", from, to);
         return NULL;
      }
      num_val += to - from + 1;

      if (*p == '}') {
         values = mallock((num_val + 1) * sizeof(long long));
         values[0] = num_val;
      } else if (*p == ',') {
         values = consume_elements(p + 1, is_unsigned, num_val);
         if (values == NULL)
            return NULL;
      } else {
         error("syntax error near '%s'\n", p);
         return NULL;
      }
      for (long long v = to; v >= from; --v)
         values[num_val--] = v;
      return values;
   }

   p = parse_int(begin, is_unsigned, &val);
   if (p == begin)   // errors
      return NULL;
   ++num_val;
   if (*p == '}') {
      values = mallock((num_val + 1) * sizeof(long long));
      values[0] = num_val;
   } else if (*p == ',') {
      values = consume_elements(p + 1, is_unsigned, num_val);
      if (values == NULL)
         return NULL;
   } else {
      error("syntax error near '%s'\n", p);
      return NULL;
   }
   values[num_val] = val;

   return values;
}


/* This function is invoked upon encountering a '{' in an opcode
   specification. It parses the set elements, explodes any value
   ranges and returns an array with the values found.
   The function returns NULL upon encountering syntax errors.  */
static long long *
consume_set(const char *p, unsigned num_bits, int is_unsigned)
{
   assert(*p == '{');

   if (p[1] == '}') {
      error("empty value set not allowed\n");
      return NULL;
   }
   long long *values = consume_elements(p + 1, is_unsigned, 0);

   if (values == NULL)  // there were errors
      return NULL;

   long long max_val = is_unsigned ? (1LL << num_bits) - 1
                                   : (1LL << (num_bits - 1)) - 1;
   long long min_val = is_unsigned ? 0 : -max_val - 1;

   /* Check for out-of-range values. */
   for (int i = 1; i <= values[0]; ++i) {
      long long val = values[i];
      if (val < min_val) {
         error("value %lld too small for %s %u bits\n", val,
               is_unsigned ? "unsigned" : "signed", num_bits);
         return NULL;
      }
      if (val > max_val) {
         error("value %lld too large for %s %u bits\n", val,
               is_unsigned ? "unsigned" : "signed", num_bits);
         return NULL;
      }
   }
   return values;
}


/* Construct an invalid operand. It is used to indicate that there
   were parse errors reading an operand. */
static opnd
invalid_opnd(char *name)
{
   return (opnd){ .name = name, .kind = OPND_INVALID, .num_bits = 0,
                  .is_unsigned = 1, .allowed_values = 0 };
}


/* GPRs and VRs are understood. Specification not allowed. */
static opnd
register_operand(const char *opnd_string)
{
   char *name = strsave(opnd_string);
   const char *p = skip_digits(opnd_string + 1);

   if (p == opnd_string + 1) {   // no digits found
      error("%s: invalid register name\n", opnd_string);
      return invalid_opnd(name);
   }
   if (*p == ':') {
      error("%s: specification is invalid for registers\n", opnd_string);
      return invalid_opnd(name);
   }
   if (*p != '\0') {
      error("'%s' is not understood\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   opnd_t kind = OPND_GPR;
   unsigned num_bits = 4;

   if (opnd_string[0] == 'v') {
      kind = OPND_VR;
      num_bits = 5;
   }
   return (opnd){ .name = name, .kind = kind, .num_bits = num_bits,
                  .is_unsigned = 1, .allowed_values = 0 };
}


static opnd
mask_operand(const char *opnd_string)
{
   unsigned num_bits = 4;

   const char *p = skip_digits(opnd_string + 1);

   if (p == opnd_string + 1) {   // no digits found
      error("%s: invalid mask name\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   char *name = strnsave(opnd_string, (unsigned)(p - opnd_string));
   long long *allowed_values = NULL;

   if (p[0] == ':') {
      ++p;
      /* Either e.g.  m7:u2   or  m7:{...}  or m7:u2{...} */
      if (p[0] == 's') {
         error("%s: cannot be a signed integer\n", name);
         return invalid_opnd(name);
      }

      if (p[0] != 'u' && p[0] != '{') {
         error("%s: expected 'u' or '{'\n", name);
         return invalid_opnd(name);
      }

      if (p[0] == 'u') {
         if (sscanf(p + 1, "%u", &num_bits) != 1) {
            error("%s: missing #bits\n", name);
            return invalid_opnd(name);
         }
         p = skip_digits(p + 1);
      }
      if (p[0] == '{') {
         allowed_values = consume_set(p, num_bits, 1);
         if (allowed_values == NULL)
            return invalid_opnd(name);
         p = strchr(p + 1, '}');
         if (p == NULL) {
            error("%s: expected '}' not found\n", name);
            return invalid_opnd(name);
         }
         ++p;
      }
   }
   if (p[0] != '\0') {
      error("'%s' is not understood\n", opnd_string);
      return invalid_opnd(name);
   }
   return (opnd){ .name = name, .kind = OPND_MASK, .num_bits = num_bits,
                  .is_unsigned = 1, .allowed_values = allowed_values };
}


static opnd
dxb_operand(const char *opnd_string)
{
   unsigned x, b, l, v;
   opnd_t kind = OPND_INVALID;

   if (sscanf(opnd_string, "d12(x%u,b%u)", &x, &b) == 2) {
      kind = OPND_D12XB;
   } else if (sscanf(opnd_string, "d20(x%u,b%u)", &x, &b) == 2) {
      kind = OPND_D20XB;
   } else if (sscanf(opnd_string, "d12(b%u)", &b) == 1) {
      kind = OPND_D12B;
   } else if (sscanf(opnd_string, "d20(b%u)", &b) == 1) {
      kind = OPND_D20B;
   } else if (sscanf(opnd_string, "d12(l,b%u)", &b) == 1) {
      kind = OPND_D12LB;
   } else if (sscanf(opnd_string, "d12(l%u,b%u)", &l, &b) == 2) {
      kind = OPND_D12LB;
   } else if (sscanf(opnd_string, "d12(v%u,b%u)", &v, &b) == 2) {
      kind = OPND_D12VB;
   }

   if (kind == OPND_INVALID) {
      error("%s: not a valid dxb operand\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   const char *p = strchr(opnd_string, ')') + 1;
   char *name = strnsave(opnd_string, (unsigned)(p - opnd_string));

   if (p[0] == ':') {
      error("%s: specification is invalid for dbx operands\n", name);
      return invalid_opnd(name);
   }
   if (p[0] != '\0') {
      error("'%s' is not understood\n", opnd_string);
      return invalid_opnd(name);
   }

   return (opnd){ .name = name, .kind = kind, .num_bits = 0,
                  .is_unsigned = 1, .allowed_values = 0 };
}


static opnd
integer_operand(const char *opnd_string)
{
   const char *colon = strchr(opnd_string, ':');
   if (colon == NULL) {
      error("%s: missing signedness specification\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   opnd_t kind;
   char *name = strnsave(opnd_string, (unsigned)(colon - opnd_string));
   if (colon[1] == 's') {
      kind = OPND_SINT;
   } else if (colon[1] == 'u') {
      kind = OPND_UINT;
   } else {
      error("%s: invalid type spec '%c'\n", name, colon[1]);
      return invalid_opnd(name);
   }

   /* Check for PC-relative operand */
   if (strncmp(name, "ri", 2) == 0) {
      if (kind == OPND_UINT) {
         error("%s: PC-relative operand is unsigned\n", name);
         return invalid_opnd(name);
      }
      kind = OPND_PCREL;
   }

   unsigned num_bits;
   if (sscanf(colon + 2, "%u", &num_bits) != 1) {
      error("%s: missing #bits\n", name);
      return invalid_opnd(name);
   }

   long long *allowed_values = NULL;
   const char *p = skip_digits(colon + 2);
   if (p[0] == '{') {
      allowed_values = consume_set(p, num_bits, kind == OPND_UINT);
      if (allowed_values == NULL)
         return invalid_opnd(name);

      p = strchr(p + 1, '}');
      if (p == NULL) {
         error("%s: expected '}' not found\n", name);
         return invalid_opnd(name);
      }
      ++p;
   }
   if (p[0] != '\0') {
      error("'%s' is not understood\n", opnd_string);
      return invalid_opnd(name);
   }

   return (opnd){ .name = name, .kind = kind, .num_bits = num_bits,
                  .is_unsigned = kind == OPND_UINT,
                  .allowed_values = allowed_values };
}


static opnd
get_operand(const char *opnd_string)
{
   switch (opnd_string[0]) {
   case 'r':    // GPR
      if (! isdigit(opnd_string[1])) break;
      /* fall through */
   case 'b':    // GPR
   case 'x':    // GPR
   case 'v':    // VR
      return register_operand(opnd_string);

      /* Masks without specification are understood:
         4-bit wide, unsigned integer value. In contrast to registers
         a specification is allowed. However, the interpretation must
         be 'unsigned'. */
   case 'm':
      return mask_operand(opnd_string);

   /* Address computation using base register, optional index
      register and displacement are understood.
      d12 = 12-bit displacement = unsigned integer value
      d20 = 20-bit displacement = signed integer value */
   case 'd':
      return dxb_operand(opnd_string);

   default:
      break;
   }

   /* All other operands require specification of #bits and signedness.
      E.g. ri3:s8, ri4:u24 */
   return integer_operand(opnd_string);
}


static const char *
opnd_kind_as_string(int kind)
{
   switch (kind) {
   case OPND_GPR:     return "gpr";
   case OPND_VR:      return "vr";
   case OPND_D12XB:   return "d12xb";
   case OPND_D20XB:   return "d20xb";
   case OPND_D12B:    return "d12b";
   case OPND_D20B:    return "d20b";
   case OPND_D12LB:   return "d12lb";
   case OPND_D12VB:   return "d12vb";
   case OPND_SINT:    return "sint";
   case OPND_UINT:    return "uint";
   case OPND_MASK:    return "mask";
   case OPND_PCREL:   return "pcrel";
   case OPND_INVALID: return "INVALID";
   default:
      assert(0);
   }
}


opcode *
get_opcode_by_name(const char *name)
{
   unsigned len = strlen(name);

   for (int i = 0; i < num_opcodes; ++i) {
      const char *op = opcodes[i];
      if (strncmp(op, name, len) == 0 &&
          (op[len] == ' ' || op[len] == '\0'))
         return get_opcode_by_index(i);
   }
   return NULL;
}


/* Returns a block of information for the given opcode. */
static opcode *
parse_opcode(const char *spec)
{
   if (debug)
      printf("spec:   |%s|\n", spec);

   /* Make local copy */
   char copy[strlen(spec) + 1];
   strcpy(copy, spec);

   /* Skip over the opcode name */
   char *p;
   for (p = copy; *p; ++p) {
      if (*p == ' ')
         break;
   }
   // *p == ' ' or *p == '\0'

   char *name = strnsave(copy, p - copy);

   while (*p == ' ')
      ++p;

   /* Remove any blanks from operand list */
   char *opnd_string = p;
   char *q;
   for (q = p = opnd_string; *p; ++p) {
      if (*p != ' ')
         *q++ = *p;
   }
   *q = '\0';

   /* Count number of operands by counting ','. That number may be
      larger than the actual number of operands because of operands
      like d12(x2,b2) with embedded comma. That's OK. */
   unsigned num_comma = 0;
   for (p = opnd_string; *p; ++p)
      if (*p == ',')
         ++num_comma;

   ++num_comma;     // 1 comma --> 2 operands

   unsigned need = num_comma * sizeof(opnd);

   opnd *opnds = mallock(need);

   /* Parse operand list */
   int in_paren = 0;
   int in_brace = 0;
   int num_opnds = 0;

   for (p = opnd_string; *p; ++p) {
      int c = *p;

      if (c == '{') ++in_brace;
      if (c == '}') --in_brace;

      if (in_paren == 0) {
         if (c == '(') {
            ++in_paren;
         } else if (c == ',' && ! in_brace) {
            *p = '\0';
            opnds[num_opnds++] = get_operand(opnd_string);
            opnd_string = p + 1;
         }
      } else {
         if (c == ')')
            --in_paren;
      }
   }
   if (*opnd_string)
      opnds[num_opnds++] = get_operand(opnd_string);

   /* Determine the number of fields in this opcode.
      A field is an entity that test generation will assign a
      value to */
   unsigned num_fields = 0;

   for (int i = 0; i < num_opnds; ++i) {
      switch (opnds[i].kind) {
      case OPND_GPR:
      case OPND_VR:
      case OPND_SINT:
      case OPND_UINT:
      case OPND_MASK:
      case OPND_PCREL:
         num_fields += 1;
         break;

      case OPND_D12B:
      case OPND_D20B:
         num_fields += 2;
         break;

      case OPND_D12XB:
      case OPND_D12LB:
      case OPND_D12VB:
      case OPND_D20XB:
         num_fields += 3;
         break;

      case OPND_INVALID:
         break;

      default:
         assert(0);
      }
   }

   /* Finalise opcode */
   opcode *opc = mallock(sizeof(opcode));

   opc->name = name;
   opc->opnds = opnds;
   opc->num_opnds = num_opnds;
   opc->num_fields = num_fields;

   if (debug) {
      printf("opcode: |%s|\n", opc->name);
      for (int i = 0; i < opc->num_opnds; ++i) {
         const opnd *d = opc->opnds + i;
         printf("opnd %2d: %-8s  type: %-5s", i, d->name,
                opnd_kind_as_string(d->kind));
         if (d->kind != OPND_D12XB && d->kind != OPND_D12B &&
             d->kind != OPND_D20XB && d->kind != OPND_D20B &&
             d->kind != OPND_D12LB && d->kind != OPND_D12VB)
            printf("  #bits: %2u", d->num_bits);
         if (d->allowed_values) {
            printf("  values:");
            unsigned nval = d->allowed_values[0];
            for (int j = 1; j <= nval; ++j) {
               if (d->is_unsigned)
                  printf(" %u", (unsigned)d->allowed_values[j]);
               else
                  printf(" %d", (int)d->allowed_values[j]);
            }
         }
         printf("\n");
      }
   }

   return opc;
}


/* Returns a block of information for the given opcode. */
opcode *
get_opcode_by_index(unsigned ix)
{
   assert(ix < num_opcodes);

   return parse_opcode(opcodes[ix]);
}


void
release_opcode(opcode *opc)
{
   for (int i = 0; i < opc->num_opnds; ++i) {
      const opnd *q = opc->opnds + i;
      free(q->name);
      free(q->allowed_values);
   }
   free(opc->name);
   free(opc->opnds);
   free(opc);
}


/* Unit tests */

static const char *unit_tests[] = {
   "ok1", "ok2 m2", "ok3 m3:u3", "ok4 m4:{1}", "ok5 m5:{1..4}",
   "ok6 m6:{1,2,3}", "ok7 m7:{1..4,5,7..8}", "ok8 m8:{10,1..3,0}",
   "ok9 m9:u7{3,2,10..11,15,16}", "ok10 m10:u2", "ok11 m11:{11..11}",
   "ok12 r1,d12(l,b2)", "ok13 r3,d12(l2,b3)", "ok14 d12(v1,b3)",
   "err1 m", "err2 m2:", "err3 m3:s", "err4 m4:s5",
   "err5 m5{", "err6 m6:{", "err7 m7:{}", "err8 m8:{1", "err9 m9:{1,",
   "err10 m0:{2..}", "err11 m11:{2..1}", "err12 m11:u{1,2}",
   "err13 m13:r", "err14 m14:u"
};


void
run_unit_tests(void)
{
   unsigned num_tests = sizeof unit_tests / sizeof unit_tests[0];

   debug = 1;

   for (int i = 0; i < num_tests; ++i)
      parse_opcode(unit_tests[i]);
}
