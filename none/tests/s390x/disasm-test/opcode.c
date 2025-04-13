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
   - 'a[0-9]+' denotes an AR (4-bit wide unsigned value)
   - 'f[0-9]+' denotes an FPR (4-bit wide unsigned value)
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

   If a set of allowed values is specified for an operand this
   implies that any other value would cause a specification exception.
   UNLESS otherwise noted.
*/

/* List of for hardware facilities.
   The abbreviated name appears as structured comment in below opcode list to
   indicate that the opcode is only available when the facility is installed.

   dflt  --> deflate-conversion facility
   dfp   --> decimal floating point facility
   diop  --> distinct-operands  == lsc
   exe   --> execute-extensions
   eimm  --> extended-immediate facility
   exhi  --> execution-hint
   extr2 --> extended-translation facility 2
   extr3 --> extended-translation facility 3
   fgx   --> FPR-GR-transfer facility
   fpext --> floating-point extension facility
   fpssh --> floating-point-support-sign-handling  == fgx
   gie   --> general-instruction-extension facility
   hiwo  --> high-word  == lsc
   ia1   --> interlocked-access facility 1
   lat   --> load-and-trap facility
   laz   --> load-and-zero-rightmost byte facility
   lsc   --> load/store-on-condition facility 1  (bit 45)
             As well as distinct-operands, high-word, population-count,
             and interlocked-access facility 1
   lsc2  --> load/store-on-condition facility 2
   mi1   --> miscellaneous-instruction-extensions facility 1
   mi2   --> miscellaneous-instruction-extensions facility 2
   mi3   --> miscellaneous-instruction-extensions facility 3
   msa4  --> message-security-assist extension 4
   msa5  --> message-security-assist extension 5
   msa8  --> message-security-assist extension 8
   msa9  --> message-security-assist extension 9
   nnpa  --> neural-network-processing-assist facility
   ppa   --> processor-assist facility
   pfpo  --> PFPO facility
   popc  --> population-count facility  == lsc
   stckf --> STCKF facility
   stfle --> STFLE facility
   vx    --> vector facility
   vxe   --> vector enhancements facility 1
   vxe2  --> vector enhancements facility 2   implies vxe and vx
   vxd   --> vector packed decimal facility
*/

/* List of all supported opcodes in the order as defined in
   Principles of Operations. */

static const char *opcodes[] = {
   // Chapter 7:  General Instructions

   "ar      r1,r2",
   "agr     r1,r2",
   "agfr    r1,r2",
   "ark     r1,r2,r3",          // diop
   "agrk    r1,r2,r3",          // diop
   "a       r1,d12(x2,b2)",
   "ay      r1,d20(x2,b2)",
   "ag      r1,d20(x2,b2)",
   "agf     r1,d20(x2,b2)",

   "afi     r1,i2:s32",         // eimm
   "agfi    r1,i2:s32",         // eimm
   "ahik    r1,r3,i2:s16",      // diop
   "aghik   r1,r3,i2:s16",      // diop
   "asi     d20(b2),i2:s8",     // gie
   "agsi    d20(b2),i2:s8",     // gie

   "ah      r1,d12(x2,b2)",
   "ahy     r1,d20(x2,b2)",
   "agh     r1,d20(x2,b2)",     // mi2

   "ahi     r1,i2:s16",
   "aghi    r1,i2:s16",

   "ahhhr   r1,r2,r3",          // hiwo
   "ahhlr   r1,r2,r3",          // hiwo

   "aih     r1,i2:s32",         // hiwo

   "alr     r1,r2",
   "algr    r1,r2",
   "algfr   r1,r2",
   "alrk    r1,r2,r3",          // diop
   "algrk   r1,r2,r3",          // diop
   "al      r1,d12(x2,b2)",
   "aly     r1,d20(x2,b2)",
   "alg     r1,d20(x2,b2)",
   "algf    r1,d20(x2,b2)",

   "alfi    r1,i2:u32",         // eimm
   "algfi   r1,i2:u32",         // eimm

   "alhhhr  r1,r2,r3",          // hiwo
   "alhhlr  r1,r2,r3",          // hiwo

   "alcr    r1,r2",
   "alcgr   r1,r2",
   "alc     r1,d20(x2,b2)",
   "alcg    r1,d20(x2,b2)",

   "alsi    d20(b1),i2:s8",     // gie
   "algsi   d20(b1),i2:s8",     // gie
   "alhsik  r1,r3,i2:s16",      // diop
   "alghsik r1,r3,i2:s16",      // diop

   "alsih   r1,i2:s32",         // hiwo
   "alsihn  r1,i2:s32",         // hiwo

   "nr      r1,r2",
   "ngr     r1,r2",
   "nrk     r1,r2,r3",          // diop
   "ngrk    r1,r2,r3",          // diop
   "n       r1,d12(x2,b2)",
   "ny      r1,d20(x2,b2)",
   "ng      r1,d20(x2,b2)",
   "ni      d12(b1),i2:u8",
   "niy     d20(b1),i2:u8",
   "nc      d12(l,b1),d12(b2)",

   "nihf    r1,i2:u32",         // eimm
   "nihh    r1,i2:u16",
   "nihl    r1,i2:u16",
   "nilf    r1,i2:u32",         // eimm
   "nilh    r1,i2:u16",
   "nill    r1,i2:u16",

   "ncrk    r1,r2,r3",          // mi3
   "ncgrk   r1,r2,r3",          // mi3

   // balr   not implemented
   // bal    not implemented

   "basr   r1,r2",
   "bas    r1,d12(x2,b2)",

   // bassm  not implemented
   // bsm    not implemented

   "bic    m1,d20(x2,b2)",      // mi2

   "bcr    m1,r2",
   "bc     m1,d12(x2,b2)",

   "bctr   r1,r2",
   "bctgr  r1,r2",
   "bct    r1,d12(x2,b2)",
   "bctg   r1,d20(x2,b2)",

   "bxh    r1,r3,d12(b2)",
   "bxhg   r1,r3,d20(b2)",

   "bxle   r1,r3,d12(b2)",
   "bxleg  r1,r3,d20(b2)",

   "bpp    i1:u4,ri2:s16,d12(b3)",  // exhi
   "bprp   i1:u4,ri2:s12,ri3:s24",  // exhi
   "bras   r1,ri2:s16",
   "brasl  r1,ri2:s32",

   "brc    m1,ri2:s16",

   "brcl   m1,ri2:s32",

   "brct   r1,ri2:s16",
   "brctg  r1,ri2:s16",

   "brcth  r1,ri2:s32",         // hiwo

   "brxh   r1,r3,ri2:s16",
   "brxhg  r1,r3,ri2:s16",

   "brxle  r1,r3,ri2:s16",
   "brxlg  r1,r3,ri2:s16",

   "cksm   r1,r2:{0,2,4,6,8,10,12,14}",

   "km     r1:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",   // msa
   "kmc    r1:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",   // msa
   "kma    r1:{2,4,6,8,10,12,14},r3:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",  // also: r3 != r1 && r3 != r2  // msa8
   "kmf    r1:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",   // msa4
   "kmctr  r1:{2,4,6,8,10,12,14},r3:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",  // msa4
   "kmo    r1:{2,4,6,8,10,12,14},r2:{2,4,6,8,10,12,14}",   // msa4

   "cr     r1,r2",
   "cgr    r1,r2",
   "cgfr   r1,r2",
   "c      r1,d12(x2,b2)",
   "cy     r1,d20(x2,b2)",
   "cg     r1,d20(x2,b2)",
   "cgf    r1,d20(x2,b2)",

   "cfi    r1,i2:s32",                  // eimm
   "cgfi   r1,i2:s32",                  // eimm

   "crl    r1,ri2:s32",                 // gie
   "cgrl   r1,ri2:s32",                 // gie
   "cgfrl  r1,ri2:s32",                 // gie

   "crb    r1,r2,m3,d12(b4)",           // gie
   "cgrb   r1,r2,m3,d12(b4)",           // gie

   "crj    r1,r2,m3,ri4:s16",           // gie
   "cgrj   r1,r2,m3,ri4:s16",           // gie

   "cib    r1,i2:s8,m3,d12(b4)",        // gie
   "cgib   r1,i2:s8,m3,d12(b4)",        // gie

   "cij    r1,i2:s8,m3,ri4:s16",        // gie
   "cgij   r1,i2:s8,m3,ri4:s16",        // gie

   // cfc  not implemented

   "cs     r1,r3,d12(b2)",
   "csy    r1,r3,d20(b2)",
   "csg    r1,r3,d20(b2)",

   "cds    r1:{0,2,4,6,8,10,12,14},r3:{0,2,4,6,8,10,12,14},d12(b2)",
   "cdsy   r1:{0,2,4,6,8,10,12,14},r3:{0,2,4,6,8,10,12,14},d20(b2)",
   "cdsg   r1:{0,2,4,6,8,10,12,14},r3:{0,2,4,6,8,10,12,14},d20(b2)",

   // csst not implemented

   "crt    r1,r2,m3",                   // gie
   "cgrt   r1,r2,m3",                   // gie

   "cit    r1,i2:s16,m3",               // gie
   "cgit   r1,i2:s16,m3",               // gie

   "ch     r1,d12(x2,b2)",
   "chy    r1,d20(x2,b2)",
   "cgh    r1,d20(x2,b2)",              // gie

   "chi    r1,i2:s16",
   "cghi   r1,i2:s16",
   "chhsi  d12(b1),i2:s16",             // gie
   "chsi   d12(b1),i2:s16",             // gie
   "cghsi  d12(b1),i2:s16",             // gie

   "chrl   r1,ri2:s32",                 // gie
   "cghrl  r1,ri2:s32",                 // gie

   "chhr   r1,r2",                      // hiwo
   "chlr   r1,r2",                      // hiwo
   "chf    r1,d20(x2,b2)",              // hiwo

   "cih    r1,i2:s32",                  // hiwo

   "clr    r1,r2",
   "clgr   r1,r2",
   "clgfr  r1,r2",
   "cl     r1,d12(x2,b2)",
   "cly    r1,d20(x2,b2)",
   "clg    r1,d20(x2,b2)",
   "clgf   r1,d20(x2,b2)",
   "clc    d12(l,b1),d12(b2)",

   "clfi   r1,i2:u32",                  // eimm
   "clgfi  r1,i2:u32",                  // eimm
   "cli    d12(b1),i2:u8",
   "cliy   d20(b1),i2:u8",
   "clfhsi d12(b1),i2:u16",             // gie
   "clghsi d12(b1),i2:u16",             // gie
   "clhhsi d12(b1),i2:u16",             // gie

   "clrl   r1,ri2:s32",                 // gie
   "clgrl  r1,ri2:s32",                 // gie
   "clgfrl r1,ri2:s32",                 // gie
   "clhrl  r1,ri2:s32",                 // gie
   "clghrl r1,ri2:s32",                 // gie

   "clrb   r1,r2,m3,d12(b4)",           // gie
   "clgrb  r1,r2,m3,d12(b4)",           // gie

   "clrj   r1,r2,m3,ri4:s16",           // gie
   "clgrj  r1,r2,m3,ri4:s16",           // gie

   "clib   r1,i2:u8,m3,d12(b4)",        // gie
   "clgib  r1,i2:u8,m3,d12(b4)",        // gie

   "clij   r1,i2:u8,m3,ri4:s16",        // gie
   "clgij  r1,i2:u8,m3,ri4:s16",        // gie

   "clrt   r1,r2,m3",                   // gie
   "clgrt  r1,r2,m3",                   // gie
   "clt    r1,m3,d20(b2)",              // mi1
   "clgt   r1,m3,d20(b2)",              // mi1

   "clfit  r1,i2:u16,m3",
   "clgit  r1,i2:u16,m3",

   "clm    r1,m3,d12(b2)",
   "clmy   r1,m3,d20(b2)",
   "clmh   r1,m3,d20(b2)",

   "clhhr  r1,r2",                      // hiwo
   "clhlr  r1,r2",                      // hiwo
   "clhf   r1,d20(x2,b2)",              // hiwo

   "clih   r1,i2:u32",                  // hiwo

   "clcl   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14}",

   "clcle  r1:{0,2,4,6,8,10,12,14},r3:{0,2,4,6,8,10,12,14},d12(b2)",

   // clclu not implemented

   "clst   r1,r2",

   // cuse  not implemented
   // cmpsc not implemented

   "kimd   r1,r2:{2,4,6,8,10,12,14}",   // msa
   "klmd   r1,r2:{2,4,6,8,10,12,14}",   // msa
   "kmac   r1,r2:{2,4,6,8,10,12,14}",   // msa

   "cvb    r1,d12(x2,b2)",
   "cvby   r1,d20(x2,b2)",
   // cvbg not implemented

   "cvd    r1,d12(x2,b2)",
   "cvdy   r1,d20(x2,b2)",
   // cvdg not implemented

   "cu24   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14},m3:{0,1}",  // extr3  no spec exc for m3

   "cu21   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14},m3:{0,1}",  // no spec exc for m3

   "cu42   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14}",           // extr3
   "cu41   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14}",           // extr3

   "cu12   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14},m3:{0,1}",  // no spec exc for m3

   "cu14   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14},m3:{0,1}",  // extr3  no spec exc for m3

   "cpya   a1,a2",

   "dr     r1:{0,2,4,6,8,10,12,14},r2",
   "d      r1:{0,2,4,6,8,10,12,14},d12(x2,b2)",

   "dlr    r1:{0,2,4,6,8,10,12,14},r2",
   "dlgr   r1:{0,2,4,6,8,10,12,14},r2",
   "dl     r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",
   "dlg    r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",

   "dsgr   r1:{0,2,4,6,8,10,12,14},r2",
   "dsgfr  r1:{0,2,4,6,8,10,12,14},r2",
   "dsg    r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",
   "dsgf   r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",

   "xr     r1,r2",
   "xgr    r1,r2",
   "xrk    r1,r2,r3",           // diop
   "xgrk   r1,r2,r3",           // diop
   "x      r1,d12(x2,b2)",
   "xy     r1,d20(x2,b2)",
   "xg     r1,d20(x2,b2)",
   "xi     d12(b1),i2:u8",
   "xiy    d20(b1),i2:u8",
   "xc     d12(l,b1),d12(b2)",

   "xihf   r1,i2:u32",          // eimm
   "xilf   r1,i2:u32",          // eimm

   "ex     r1,d12(x2,b2)",

   "exrl   r1,ri2:s32",         // exe

   "ear    r1,a2",

   "ecag   r1,r3,d20(b2)",      // gie

   // ectg  not implemented
   // epsw  not implemented
   // etnd  not implemented

   "flogr  r1:{0,2,4,6,8,10,12,14},r2",          // eimm

   "ic     r1,d12(b2,x2)",
   "icy    r1,d20(b2,x2)",

   "icm    r1,m3,d12(b2)",
   "icmy   r1,m3,d20(b2)",
   "icmh   r1,m3,d20(b2)",

   "iihf   r1,i2:u32",          // eimm
   "iihh   r1,i2:u8",
   "iihl   r1,i2:u8",
   "iilf   r1,i2:u32",          // eimm
   "iilh   r1,i2:u8",
   "iill   r1,i2:u8",

   "ipm    r1",

   "lr     r1,r2",
   "lgr    r1,r2",
   "lgfr   r1,r2",
   "l      r1,d12(x2,b2)",
   "ly     r1,d20(x2,b2)",
   "lg     r1,d20(x2,b2)",
   "lgf    r1,d20(x2,b2)",

   "lgfi   r1,i2:s32",          // eimm

   "lrl    r1,i2:s32",          // gie
   "lgrl   r1,i2:s32",          // gie
   "lgfrl  r1,i2:s32",          // gie

   "lam    a1,a3,d12(b2)",
   "lamy   a1,a3,d20(b2)",

   "la     r1,d12(x2,b2)",
   "lay    r1,d20(x2,b2)",

   "lae    r1,d12(x2,b2)",
   "laey   r1,d20(x2,b2)",      // gie

   "larl   r1,ri2:s32",

   "laa    r1,r3,d20(b2)",      // ia1
   "laag   r1,r3,d20(b2)",      // ia1

   "laal   r1,r3,d20(b2)",      // ia1
   "laalg  r1,r3,d20(b2)",      // ia1

   "lan    r1,r3,d20(b2)",      // ia1
   "lang   r1,r3,d20(b2)",      // ia1

   "lax    r1,r3,d20(b2)",      // ia1
   "laxg   r1,r3,d20(b2)",      // ia1

   "lao    r1,r3,d20(b2)",      // ia1
   "laog   r1,r3,d20(b2)",      // ia1

   "ltr    r1,r2",
   "ltgr   r1,r2",
   "ltgfr  r1,r2",
   "lt     r1,d20(x2,b2)",      // eimm
   "ltg    r1,d20(x2,b2)",      // eimm
   "ltgf   r1,d20(x2,b2)",      // gie

   "lat    r1,d20(x2,b2)",      // lat
   "lgat   r1,d20(x2,b2)",      // lat

   "lzrf   r1,d20(x2,b2)",      // laz
   "lzrg   r1,d20(x2,b2)",      // laz

   "lbr    r1,r2",              // eimm
   "lgbr   r1,r2",              // eimm
   "lb     r1,d20(x2,b2)",
   "lgb    r1,d20(x2,b2)",

   "lbh    r1,d20(x2,b2)",      // hiwo

   "lcr    r1,r2",
   "lcgr   r1,r2",
   "lcgfr  r1,r2",

   "lcbb   r1,d12(x2,b2),m3:{0..6}",    // vx

   // lgg    not implemented
   // llgfsg not implemented
   // lgsc   not implemented

   "lhr    r1,r2",              // eimm
   "lghr   r1,r2",              // eimm
   "lh     r1,d12(x2,b2)",
   "lhy    r1,d20(x2,b2)",
   "lgh    r1,d20(x2,b2)",

   "lhi    r1,i2:s16",
   "lghi   r1,i2:s16",
   "lhrl   r1,ri2:s32",         // gie
   "lghrl  r1,ri2:s32",         // gie

   "lhh    r1,d20(x2,b2)",      // hiwo

   "lochi  r1,i2:s16,m3",       // lsc2
   "locghi r1,i2:s16,m3",       // lsc2

   "lochhi r1,i2:s16,m3",       // lsc2

   "lfh    r1,d20(x2,b2)",      // hiwo

   "lfhat  r1,d20(x2,b2)",      // lat

   "llgfr  r1,r2",
   "llgf   r1,d20(x2,b2)",

   "llgfrl r1,ri2:s32",         // gie

   "llgfat r1,d20(x2,b2)",

   "llzrgf r1,d20(x2,b2)",      // laz

   "llcr   r1,r2",              // gie
   "llgcr  r1,r2",              // gie
   "llc    r1,d20(x2,b2)",      // gie
   "llgc   r1,d20(x2,b2)",

   "llch   r1,d20(x2,b2)",      // hiwo

   "llhr   r1,r2",              // eimm
   "llghr  r1,r2",              // eimm
   "llh    r1,d20(x2,b2)",      // eimm
   "llgh   r1,d20(x2,b2)",

   "llhrl  r1,ri2:s32",         // gie
   "llghrl r1,ri2:s32",         // gie

   "llhh   r1,d20(x2,b2)",      // hiwo

   "llihf  r1,i2:u32",          // eimm
   "llihh  r1,i2:u16",
   "llihl  r1,i2:u16",
   "llilf  r1,i2:u32",          // eimm
   "llilh  r1,i2:u16",
   "llill  r1,i2:u16",

   "llgtr  r1,r2",
   "llgt   r1,d20(x2,b2)",

   "llgtat r1,d20(x2,b2)",      // lat

   "lm     r1,r3,d12(b2)",
   "lmy    r1,r3,d20(b2)",
   "lmg    r1,r3,d20(b2)",

   // lmd  not implemented

   "lmh    r1,r3,d20(b2)",

   "lnr    r1,r2",
   "lngr   r1,r2",
   "lngfr  r1,r2",

   "locr   r1,r2,m3",           // lsc
   "locgr  r1,r2,m3",           // lsc
   "loc    r1,d20(b2),m3",      // lsc
   "locg   r1,d20(b2),m3",      // lsc

   "locfhr r1,r2,m3",           // lsc2
   "locfh  r1,d20(b2),m3",      // lsc2

   // lpd   not implemented
   // lpdg  not implemented

   "lpq    r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",

   "lpr    r1,r2",
   "lpgr   r1,r2",
   "lpgfr  r1,r2",

   "lrvr   r1,r2",
   "lrvgr  r1,r2",
   "lrvh   r1,d20(x2,b2)",
   "lrv    r1,d20(x2,b2)",
   "lrvg   r1,d20(x2,b2)",

   // mc   not implemented

   "mvc    d12(l,b1),d12(b2)",
   "mvhhi  d12(b2),i2:s16",             // gie
   "mvhi   d12(b2),i2:s16",             // gie
   "mvghi  d12(b2),i2:s16",             // gie
   "mvi    d12(b2),i2:u8",
   "mviy   d20(b2),i2:u8",

   "mvcin  d12(l,b1),d12(b2)",

   "mvcl   r1:{0,2,4,6,8,10,12,14},r2:{0,2,4,6,8,10,12,14}",

   "mvcle  r1:{0,2,4,6,8,10,12,14},r3:{0,2,4,6,8,10,12,14},d12(b2)",

   // mvclu  not implemented
   // mvn    not implemented

   "mvcrl  d12(b1),d12(b2)",            // mi3

   "mvst   r1,r2",

   // mvo    not implemented
   // mvz    not implemented

   "mr     r1:{0,2,4,6,8,10,12,14},r2",
   "mgrk   r1:{0,2,4,6,8,10,12,14},r2,r3",              // mi2
   "m      r1:{0,2,4,6,8,10,12,14},d12(x2,b2)",
   "mfy    r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",         // gie
   "mg     r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",         // mi2

   "mh     r1,d12(x2,b2)",
   "mhy    r1,d20(x2,b2)",              // gie
   "mgh    r1,d20(x2,b2)",              // mi2

   "mhi    r1,i2:s16",
   "mghi   r1,i2:s16",

   "mlr    r1:{0,2,4,6,8,10,12,14},r2",
   "mlgr   r1:{0,2,4,6,8,10,12,14},r2",
   "ml     r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",
   "mlg    r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",

   "msr    r1,r2",
   "msrkc  r1,r2,r3",           // mi2
   "msgr   r1,r2",
   "msgrkc r1,r2,r3",           // mi2
   "msgfr  r1,r2",
   "ms     r1,d12(x2,b2)",
   "msc    r1,d20(x2,b2)",      // mi2
   "msy    r1,d20(x2,b2)",
   "msg    r1,d20(x2,b2)",
   "msgc   r1,d20(x2,b2)",      // mi2
   "msgf   r1,d20(x2,b2)",

   "msfi   r1,i2:s32",          // gie
   "msgfi  r1,i2:s32",          // gie

   "nnrk   r1,r2,r3",           // mi3
   "nngrk  r1,r2,r3",           // mi3

   "niai   i1:u4{0..3},i2:u4{0..3}",  // exhi   no spec exc for i1,i2

   // ntstg not implemented

   "nork   r1,r2,r3",           // mi3
   "nogrk  r1,r2,r3",           // mi3

   "nxrk   r1,r2,r3",           // mi3
   "nxgrk  r1,r2,r3",           // mi3

   "or     r1,r2",
   "ogr    r1,r2",
   "ork    r1,r2,r3",           // diop
   "ogrk   r1,r2,r3",           // diop
   "o      r1,d12(x2,b2)",
   "oy     r1,d20(x2,b2)",
   "og     r1,d20(x2,b2)",
   "oi     d12(b1),i2:u8",
   "oiy    d20(b1),i2:u8",
   "oc     d12(l,b1),d12(b2)",

   "oihf   r1,i2:u32",          // eimm
   "oihh   r1,i2:u16",
   "oihl   r1,i2:u16",
   "oilf   r1,i2:u32",          // eimm
   "oilh   r1,i2:u16",
   "oill   r1,i2:u16",

   "ocrk   r1,r2,r3",           // mi3
   "ocgrk  r1,r2,r3",           // mi3

   // pack  not implemented
   // pka   not implemented
   // pku   not implemented

   "pcc",                       // msa4

   // plo   not implemented
   "ppa    r1,r2,m3:{1,15}",    // ppa     no spec exception for m3

   "ppno   r1,r2",              // msa5
   "prno   r1,r2",              // msa5

   "popcnt r1,r2,m3:{0,8}",     // popc    no spec exception for m3

   "pfd    m1,d20(x2,b2)",      // gie
   "pfdrl  m1,ri2:s32",         // gie

   "rll    r1,r3,d20(b2)",
   "rllg   r1,r3,d20(b2)",

   // Rotate and .... opcodes require special handling
   //
   // For rosbg and friends
   // - Bit #0 of i3 is the T-bit and bit #1 of i3 ought to be 0.
   // - i5 is optional and will not be written when 0
   //
   // For risbg and friends
   // - Bit #0 of i4 is the Z-bit and bit #1 of i4 ought to be 0.
   // - i5 is optional and will not be written when 0
   //
   // This implies that we need to model i3, i4 and i5 as masks so
   // we can manipulate their value when disassembling or suppress
   // the mask altogether. Note that we limit the set of allowed values
   // for those masks to avoid excessively large numbers of testcases.
   "rnsbg  r1,r2,m3:u8{0,1,2,63,128,129,191},m4:u6{0,1,2,63},m5:u6{0,1,2,63}",  // gie
   "rxsbg  r1,r2,m3:u8{0,1,2,63,128,129,191},m4:u6{0,1,2,63},m5:u6{0,1,2,63}",  // gie
   "rosbg  r1,r2,m3:u8{0,1,2,63,128,129,191},m4:u6{0,1,2,63},m5:u6{0,1,2,63}",  // gie

   "risbg  r1,r2,m3:u6{0,1,2,63},m4:u8{0,1,2,63,128,129,191},m5:u6{0,1,2,63}",  // gie
   "risbgn r1,r2,m3:u6{0,1,2,63},m4:u8{0,1,2,63,128,129,191},m5:u6{0,1,2,63}",  // mi1

   "risbhg r1,r2,m3:u5{0,1,2,31},m4:u8{0,1,2,31,128,129,159},m5:u6{0,1,2,63}",  // hiwo
   "risblg r1,r2,m3:u5{0,1,2,31},m4:u8{0,1,2,31,128,129,159},m5:u6{0,1,2,63}",  // hiwo

   "srst   r1,r2",

   // srstu not implemented

   "selr   r1,r2,r3,m4",               // mi3
   "selgr  r1,r2,r3,m4",               // mi3

   "selfhr r1,r2,r3,m4",               // mi3

   "sar    a1,r2",

   // sam24  not implemented
   // sam31  not implemented
   // sam64  not implemented
   // spm    not implemented

   "slda   r1:{0,2,4,6,8,10,12,14},d12(b2)",

   "sldl   r1:{0,2,4,6,8,10,12,14},d12(b2)",

   "sla    r1,d12(b2)",
   "slak   r1,r3,d20(b2)",      // diop
   "slag   r1,r3,d20(b2)",

   "sll    r1,d12(b2)",
   "sllk   r1,r3,d20(b2)",      // diop
   "sllg   r1,r3,d20(b2)",

   "srda   r1:{0,2,4,6,8,10,12,14},d12(b2)",

   "srdl   r1:{0,2,4,6,8,10,12,14},d12(b2)",

   "sra    r1,d12(b2)",
   "srak   r1,r3,d20(b2)",      // diop
   "srag   r1,r3,d20(b2)",

   "srl    r1,d12(b2)",
   "srlk   r1,r3,d20(b2)",      // diop
   "srlg   r1,r3,d20(b2)",

   "st     r1,d12(x2,b2)",
   "sty    r1,d20(x2,b2)",
   "stg    r1,d20(x2,b2)",

   "strl   r1,ri2:s32",         // gie
   "stgrl  r1,ri2:s32",         // gie

   "stam   a1,a3,d12(b2)",
   "stamy  a1,a3,d20(b2)",

   "stc    r1,d12(x2,b2)",
   "stcy   r1,d20(x2,b2)",

   "stch   r1, d20(x2,b2)",     // hiwo

   "stcm   r1,m3,d12(b2)",
   "stcmy  r1,m3,d20(b2)",
   "stcmh  r1,m3,d20(b2)",

   "stck   d12(b2)",
   "stckf  d12(b2)",            // stckf
   "stcke  d12(b2)",

   "stfle  d12(b2)",            // stfle

   // stgsc  not implemented

   "sth    r1,d12(x2,b2)",
   "sthy   r1,d20(x2,b2)",

   "sthrl  r1,ri2:s32",         // gie

   "sthh   r1,d20(x2,b2)",      // hiwo

   "stfh   r1,d20(x2,b2)",      // hiwo

   "stm    r1,r3,d12(b2)",
   "stmy   r1,r3,d20(b2)",
   "stmg   r1,r3,d20(b2)",

   "stmh   r1,r3,d20(b2)",

   "stoc   r1,d20(b2),m3",      // lsc
   "stocg  r1,d20(b2),m3",      // lsc

   "stocfh r1,d20(b2),m3",      // lsc2

   "stpq   r1:{0,2,4,6,8,10,12,14},d20(x2,b2)",

   "strvh  r1,d20(x2,b2)",
   "strv   r1,d20(x2,b2)",
   "strvg  r1,d20(x2,b2)",

   "sr     r1,r2",
   "sgr    r1,r2",
   "sgfr   r1,r2",
   "srk    r1,r2,r3",           // diop
   "sgrk   r1,r2,r3",           // diop
   "s      r1,d12(x2,b2)",
   "sy     r1,d20(x2,b2)",
   "sg     r1,d20(x2,b2)",
   "sgf    r1,d20(x2,b2)",

   "sh     r1,d12(x2,b2)",
   "shy    r1,d20(x2,b2)",
   "sgh    r1,d20(x2,b2)",      // mi2

   "shhhr  r1,r2,r3",           // hiwo
   "shhlr  r1,r2,r3",           // hiwo

   "slr    r1,r2",
   "slgr   r1,r2",
   "slgfr  r1,r2",
   "slrk   r1,r2,r3",           // diop
   "slgrk  r1,r2,r3",           // diop
   "sl     r1,d12(x2,b2)",
   "sly    r1,d20(x2,b2)",
   "slg    r1,d20(x2,b2)",
   "slgf   r1,d20(x2,b2)",

   "slfi   r1,i2:u32",          // eimm
   "slgfi  r1,i2:u32",          // eimm

   "slhhhr r1,r2,r3",           // hiwo
   "slhhlr r1,r2,r3",           // hiwo

   "slbr   r1,r2",
   "slbgr  r1,r2",
   "slb    r1,d20(x2,b2)",
   "slbg   r1,d20(x2,b2)",

   "svc    i:u8",

   // tam  not implemented
   // ts   not implemented

   "tm     d12(b1),i2:u8",
   "tmy    d20(b1),i2:u8",
   "tmhh   r1,i2:u16",
   "tmhl   r1,i2:u16",
   "tmlh   r1,i2:u16",
   "tmll   r1,i2:u16",

   // tabort  not implemented
   // tbegin  not implemented
   // tbeginc not implemented
   // tend    not implemented

   "tr     d12(l,b1),d12(b2)",

   // trt   not implemented
   // trte  not implemented
   // trtre not implemented
   // trtr  not implemented

   "tre    r1:{0,2,4,6,8,10,12,14},r2",

   "troo   r1:{0,2,4,6,8,10,12,14},r2,m3:{0,1}",  // extr2 no spec exception on m3
   "trot   r1:{0,2,4,6,8,10,12,14},r2,m3:{0,1}",  // extr2 no spec exception on m3
   "trto   r1:{0,2,4,6,8,10,12,14},r2,m3:{0,1}",  // extr2 no spec exception on m3
   "trtt   r1:{0,2,4,6,8,10,12,14},r2,m3:{0,1}",  // extr2 no spec exception on m3

   // unpk  not implemented
   // unpka not implemented
   // unpku not implemented
   // upt   not implemented

   // Chapter 8: Decimal Instructions                      not implemented

   // Chapter 9: Floating-Point Overview and Support Instructions

   // thder   not implemented
   // thdr    not implemented
   // tbedr   not implemented
   // tbdr    not implemented

   "cpsdr  f1,f3,f3",   // fpssh
   "efpc   r1",
   "ler    f1,f2",
   "ldr    f1,f2",
   "lxr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "le     f1,d12(x2,b2)",
   "ld     f1,d12(x2,b2)",
   "ley    f1,d20(x2,b2)",
   "ldy    f1,d20(x2,b2)",
   "lcdfr  f1,f2",                 // fpssh
   "lfpc   d12(b2)",
   // lfas   not implemented
   "ldgr   f1,r2",                 // fgx
   "lgdr   r1,f2",                 // fgx
   "lndfr  f1,f2",                 // fpssh
   "lpdfr  f1,f2",                 // fpssh
   "lzer   f1",
   "lzdr   f1",
   "lzxr   f1:{0,1,4,5,8,9,12,13}",
   "pfpo",                         // pfpo
   "srnm   d12(b2)",
   "srnmb  d12(b2)",               // fpext
   "sfpc   r1",
   // sfasr  not implemented
   "ste    f1,d12(x2,b2)",
   "std    f1,d12(x2,b2)",
   "stey   f1,d20(x2,b2)",
   "stdy   f1,d20(x2,b2)",
   "stfpc  d12(b2)",

   // Chapter 10: Control Instructions                      not implemented
   // Chapter 14: I/O Instructions                          not implemented
   // Chapter 18: Hexadecimal-Floating-Point Instructions   not implemented

   // Chapter 19: Binary-Floating-Point Instructions
   // Register pairs: 0-2 1-3 4-6 5-7 8-10 9-11 12-14 13-15
   // no BFP facility bit (unlike DFP)
   "aebr    f1,f2",
   "adbr    f1,f2",
   "axbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "aeb     f1,d12(x2,b2)",
   "adb     f1,d12(x2,b2)",
   "cebr    f1,f2",
   "cdbr    f1,f2",
   "cxbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "ceb     f1,d12(x2,b2)",
   "cdb     f1,d12(x2,b2)",
   "kebr    f1,f2",
   "kdbr    f1,f2",
   "kxbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "keb     f1,d12(x2,b2)",
   "kdb     f1,d12(x2,b2)",
   "cefbr   f1,r2",
   "cdfbr   f1,r2",
   "cxfbr   f1:{0,1,4,5,8,9,12,13},r2",
   "cegbr   f1,r2",
   "cdgbr   f1,r2",
   "cxgbr   f1:{0,1,4,5,8,9,12,13},r2",
   "cefbra  f1,m3:{0,1,3..7},r2,m4",
   "cdfbra  f1,m3:{0,1,3..7},r2,m4",
   "cxfbra  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},r2,m4",
   "cegbra  f1,m3:{0,1,3..7},r2,m4",
   "cdgbra  f1,m3:{0,1,3..7},r2,m4",
   "cxgbra  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},r2,m4",
   "celfbr  f1,m3:{0,1,3..7},r2,m4",                            // fpext
   "cdlfbr  f1,m3:{0,1,3..7},r2,m4",                            // fpext
   "cxlfbr  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},r2,m4",        // fpext
   "celgbr  f1,m3:{0,1,3..7},r2,m4",                            // fpext
   "cdlgbr  f1,m3:{0,1,3..7},r2,m4",                            // fpext
   "cxlgbr  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},r2,m4",        // fpext
   "cfebr   r1,m3:{0,1,3..7},f2",
   "cfdbr   r1,m3:{0,1,3..7},f2",
   "cfxbr   r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13}",
   "cgebr   r1,m3:{0,1,3..7},f2",
   "cgdbr   r1,m3:{0,1,3..7},f2",
   "cgxbr   r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13}",
   "cfebra  r1,m3:{0,1,3..7},f2,m4",
   "cfdbra  r1,m3:{0,1,3..7},f2,m4",
   "cfxbra  r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",
   "cgebra  r1,m3:{0,1,3..7},f2,m4",
   "cgdbra  r1,m3:{0,1,3..7},f2,m4",
   "cgxbra  r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",
   "clfebr  r1,m3:{0,1,3..7},f2,m4",                            // fpext
   "clfdbr  r1,m3:{0,1,3..7},f2,m4",                            // fpext
   "clfxbr  r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",        // fpext
   "clgebr  r1,m3:{0,1,3..7},f2,m4",                            // fpext
   "clgdbr  r1,m3:{0,1,3..7},f2,m4",                            // fpext
   "clgxbr  r1,m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",        // fpext
   "debr    f1,f2",
   "ddbr    f1,f2",
   "dxbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "deb     f1,d12(x2,b2)",
   "ddb     f1,d12(x2,b2)",
   // diebr  not implemented
   // didbr  not implemented
   "ltebr   f1,f2",
   "ltdbr   f1,f2",
   "ltxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "lcebr   f1,f2",
   "lcdbr   f1,f2",
   "lcxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "fiebr   f1,m3:{0,1,3..7},f2",
   "fidbr   f1,m3:{0,1,3..7},f2",
   "fixbr   f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13}",
   "fiebra  f1,m3:{0,1,3..7},f2,m4",
   "fidbra  f1,m3:{0,1,3..7},f2,m4",
   "fixbra  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",
   "ldebr   f1,f2",
   "lxdbr   f1:{0,1,4,5,8,9,12,13},f2",
   "lxebr   f1:{0,1,4,5,8,9,12,13},f2",
   "ldeb    f1,d12(x2,b2)",
   "lxdb    f1:{0,1,4,5,8,9,12,13},d12(x2,b2)",
   "lxeb    f1:{0,1,4,5,8,9,12,13},d12(x2,b2)",
   "lnebr   f1,f2",
   "lndbr   f1,f2",
   "lnxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "lpebr   f1,f2",
   "lpdbr   f1,f2",
   "lpxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "ledbr   f1,f2",
   "ldxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "lexbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "ledbra  f1,m3:{0,1,3..7},f2,m4",
   "ldxbra  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",
   "lexbra  f1:{0,1,4,5,8,9,12,13},m3:{0,1,3..7},f2:{0,1,4,5,8,9,12,13},m4",
   "meebr   f1,f2",
   "mdbr    f1,f2",
   "mxbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   // mdebr  not implemented
   // mxdbr  not implemented
   "meeb    f1,d12(x2,b2)",
   "mdb     f1,d12(x2,b2)",
   // mdeb  not implemented
   // mxdb  not implemented
   "maebr   f1,f3,f2",
   "madbr   f1,f3,f2",
   "maeb    f1,f3,d12(x2,b2)",
   "madb    f1,f3,d12(x2,b2)",
   "msebr   f1,f3,f2",
   "msdbr   f1,f3,f2",
   "mseb    f1,f3,d12(x2,b2)",
   "msdb    f1,f3,d12(x2,b2)",
   "sqebr   f1,f2",
   "sqdbr   f1,f2",
   "sqxbr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "sqeb    f1,d12(x2,b2)",
   "sqdb    f1,d12(x2,b2)",
   "sebr    f1,f2",
   "sdbr    f1,f2",
   "sxbr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "seb     f1,d12(x2,b2)",
   "sdb     f1,d12(x2,b2)",
   "tceb    f1,d12(x2,b2)",
   "tcdb    f1,d12(x2,b2)",
   "tcxb    f1:{0,1,4,5,8,9,12,13},d12(x2,b2)",

   // Chapter 20: Decimal-Floating-Point Instructions
   // Register pairs: 0-2 1-3 4-6 5-7 8-10 9-11 12-14 13-15

   // All opcodes require the dfp facility

   "adtr    f1,f2,f3",
   "axtr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13}",
   "adtra   f1,f2,f3,m4",
   "axtra   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},m4",
   "cdtr    f1,f2",
   "cxtr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   // kdtr  not implemented
   // kxtr  not implemented
   "cedtr   f1,f2",
   "cextr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   "cdgtr   f1,r2",
   "cxgtr   f1:{0,1,4,5,8,9,12,13},r2",
   "cdgtra  f1,m3,r2,m4",
   "cxgtra  f1:{0,1,4,5,8,9,12,13},m3,r2,m4",
   "cdftr   f1,m3,r2,m4",
   "cxftr   f1:{0,1,4,5,8,9,12,13},m3,r2,m4",
   "cdlgtr  f1,m3,r2,m4",                               // fpext
   "cxlgtr  f1:{0,1,4,5,8,9,12,13},m3,r2,m4",           // fpext
   "cdlftr  f1,m3,r2,m4",                               // fpext
   "cxlftr  f1:{0,1,4,5,8,9,12,13},m3,r2,m4",           // fpext
   // cdpt  not implemented
   // cxpt  not implemented
   // cdstr not implemented
   // cxstr not implemented
   // cdutr not implemented
   // cxutr not implemented
   // cdzt  not implemented
   // cxzt  not implemented
   "cgdtr   r1,m3,f2",
   "cgxtr   r1,m3,f2:{0,1,4,5,8,9,12,13}",
   "cgdtra  r1,m3,f2,m4",                               // fpext
   "cgxtra  r1,m3,f2:{0,1,4,5,8,9,12,13},m4",           // fpext
   "cfdtr   r1,m3,f2,m4",                               // fpext
   "cfxtr   r1,m3,f2:{0,1,4,5,8,9,12,13},m4",           // fpext
   "clgdtr  r1,m3,f2,m4",                               // fpext
   "clgxtr  r1,m3,f2:{0,1,4,5,8,9,12,13},m4",           // fpext
   "clfdtr  r1,m3,f2,m4",                               // fpext
   "clfxtr  r1,m3,f2:{0,1,4,5,8,9,12,13},m4",           // fpext
   // cpdt  not implemented
   // cpxt  not implemented
   // csdtr not implemented
   // csxtr not implemented
   // cudtr not implemented
   // cuxtr not implemented
   // czdt  not implemented
   // czxt  not implemented
   "ddtr    f1,f2,f3",
   "dxtr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13}",
   "ddtra   f1,f2,f3,m4",
   "dxtra   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},m4",
   "eedtr   f1,f2",
   "eextr   f1,f2:{0,1,4,5,8,9,12,13}",
   "esdtr   f1,f2",
   "esxtr   f1,f2:{0,1,4,5,8,9,12,13}",
   "iedtr   f1,f3,f2",
   "iextr   f1:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},f2",
   "ltdtr   f1,f2",
   "ltxtr   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13}",
   // fidtr  not implemented
   // fixtr  not implemented
   "ldetr   f1,f2,m4",
   "lxdtr   f1:{0,1,4,5,8,9,12,13},f2,m4",
   "ledtr   f1,m3,f2,m4",
   "ldxtr   f1:{0,1,4,5,8,9,12,13},m3,f2:{0,1,4,5,8,9,12,13},m4",
   "mdtr    f1,f2,f3",
   "mxtr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13}",
   "mdtra   f1,f2,f3,m4",
   "mxtra   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},m4",
   "qadtr   f1,f3,f2,m4",
   "qaxtr   f1:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},m4",
   "rrdtr   f1,f3,f2,m4",
   "rrxtr   f1:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},f2,m4",
   "sldt    f1,f3,d12(x2,b2)",
   "slxt    f1:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},d12(x2,b2)",
   "srdt    f1,f3,d12(x2,b2)",
   "srxt    f1:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},d12(x2,b2)",
   "sdtr    f1,f2,f3",
   "sxtr    f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13}",
   "sdtra   f1,f2,f3,m4",
   "sxtra   f1:{0,1,4,5,8,9,12,13},f2:{0,1,4,5,8,9,12,13},f3:{0,1,4,5,8,9,12,13},m4",
   "tdcet   f1,d12(x2,b2)",
   "tdcdt   f1,d12(x2,b2)",
   "tdcxt   f1:{0,1,4,5,8,9,12,13},d12(x2,b2)",
   "tdget   f1,d12(x2,b2)",
   "tdgdt   f1,d12(x2,b2)",
   "tdgxt   f1:{0,1,4,5,8,9,12,13},d12(x2,b2)",

   // Chapter 21: Vector Overview and Support Instructions

   // all opcodes require VX facility

   "vbperm  v1,v2,v3",                          // vxe
   "vgef    v1,d12(v2,b2),m3:{0,1,2,3}",
   "vgeg    v1,d12(v2,b2),m3:{0,1}",
   "vgbm    v1,i2:u16",
   "vgm     v1,i2:u8,i3:u8,m4:{0..3}",
   "vl      v1,d12(x2,b2),m3:{0,3,4}",    // no spec. exc
   "vlr     v1,v2",
   "vlrep   v1,d12(x2,b2),m3:{0..3}",
   "vlebrh  v1,d12(x2,b2),m3:{0..7}",           // vxe2
   "vlebrf  v1,d12(x2,b2),m3:{0..3}",           // vxe2
   "vlebrg  v1,d12(x2,b2),m3:{0,1}",            // vxe2
   "vlbrrep v1,d12(x2,b2),m3:{1..3}",           // vxe2
   "vllebrz v1,d12(x2,b2),m3:{1..3,6}",         // vxe2
   "vlbr    v1,d12(x2,b2),m3:{1..4}",           // vxe2
   "vleb    v1,d12(x2,b2),m3",
   "vleh    v1,d12(x2,b2),m3:{0..7}",
   "vlef    v1,d12(x2,b2),m3:{0..3}",
   "vleg    v1,d12(x2,b2),m3:{0,1}",
   "vleib   v1,i2:s16,m3",
   "vleih   v1,i2:s16,m3:{0..7}",
   "vleif   v1,i2:s16,m3:{0..3}",
   "vleig   v1,i2:s16,m3:{0,1}",
   "vler    v1,d12(x2,b2),m3:{1..3}",           // vxe2
   "vlgv    r1,v3,d12(b2),m4:{0..3}",
   "vllez   v1,d12(x2,b2),m3:{0..3,6}",
   //   "vlm     v1,v3,d12(b2),m4",  // cannot express constraint
   "vlrlr   v1,r3,d12(b2)",                     // vxd
   "vlrl    v1,d12(b2),i3:u8{0..15}",           // vxd
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
//   "vrep    v1,v3,i2:u16,m4:{0..3}",
   "vrepi   v1,i2:s16,m3:{0..3}",
   "vscef   v1,d12(v2,b2),m3:{0..3}",
   "vsceg   v1,d12(v2,b2),m3:{0,1}",
   "vsel    v1,v2,v3,v4",
   "vseg    v1,v2,m3:{0..2}",
   "vst     v1,d12(x2,b2),m3",
   "vstebrh v1,d12(x2,b2),m3:{0..7}",            // vxe2
   "vstebrf v1,d12(x2,b2),m3:{0..3}",            // vxe2
   "vstebrg v1,d12(x2,b2),m3:{0,1}",             // vxe2
   "vstbr   v1,d12(x2,b2),m3:{1..4}",            // vxe2
   "vsteb   v1,d12(x2,b2),m3",
   "vsteh   v1,d12(x2,b2),m3:{0..7}",
   "vstef   v1,d12(x2,b2),m3:{0..3}",
   "vsteg   v1,d12(x2,b2),m3:{0,1}",
   "vster   v1,d12(x2,b2),m3:{1..3}",            // vxe2
   //   "vstm    v1,v3,d12(b2),m4",  // cannot express constraint
   "vstrlr  v1,r3,d12(b2)",                      // vxd
   "vstrl   v1,d12(b2),i3:u8{0..15}",            // vxd
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
   "vmsl    v1,v2,v3,v4,m5:{3},m6:{0,4,8,12}",   // vxe  no spec. exception for m6
   "vnn     v1,v2,v3",                           // vxe
   "vno     v1,v2,v3",
   "vnx     v1,v2,v3",                           // vxe
   "vo      v1,v2,v3",
   "voc     v1,v2,v3",                           // vxe
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
   "vsld    v1,v2,v3,i4:u8{0..7}",   // vxe2  spec exc.
   "vsldb   v1,v2,v3,i4:u8{0..15}",  // no spec. exception - otherwise unpredictable
   "vsra    v1,v2,v3",
   "vsrab   v1,v2,v3",
   "vsrd    v1,v2,v3,i4:u8{0..7}",   // vxe2
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
   "vstrs   v1,v2,v3,v4,m5:{0..2},m6:{0,2}",    // vxe2

   // Chapter 24: Vector Floating-Point Instructions
   "vfa    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "wfc    v1,v2,m3:{2,3,4},m4:{0}",
   "wfk    v1,v2,m3:{2,3,4},m4:{0}",
   "vfce   v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vfch   v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vfche  v1,v2,v3,m4:{2,3,4},m5:{0,4,8,12},m6:{0,1}",
   "vcfps  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcdg   // vxe2
   "vcdg   v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcfps
   "vcfpl  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcdlg  // vxe2
   "vcdlg  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcfpl
   "vcsfp  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcgd
   "vcgd   v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vcsfp
   "vclfp  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vclgd  // vxe2
   "vclgd  v1,v2,m3:{2,3},m4:{0,4,8,12},m5:{0,1,3..7}", // aka vclfp
   "vfd    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vfi    v1,v2,m3:{2,3,4},m4:{0,4,8,12},m5:{0,1,3..7}",
   "vfll   v1,v2,m3:{2,3},m4:{0,8}",
   "vflr   v1,v2,m3:{3,4},m4:{0,4,8,12},m5:{0,1,3..7}",
   "vfmax  v1,v2,v3,m4:{2,3,4},m5:{0,8},m6:{0..4,8..12}",             // vxe
   "vfmin  v1,v2,v3,m4:{2,3,4},m5:{0,8},m6:{0..4,8..12}",             // vxe
   "vfm    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vfma   v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfms   v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",
   "vfnma  v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",                          // vxe
   "vfnms  v1,v2,v3,v4,m5:{0,8},m6:{2,3,4}",                          // vxe
   "vfpso  v1,v2,m3:{2,3,4},m4:{0,8},m5:{0..2}",
   "vfsq   v1,v2,m3:{2,3,4},m4:{0,8}",
   "vfs    v1,v2,v3,m4:{2,3,4},m5:{0,8}",
   "vftci  v1,v2,i3:u12,m4:{2,3,4},m5:{0,8}",

   // Chapter 25: Vector Decimal Instructions          not implemented

   // Chapter 26: Specialized-Function-Assist Instructions

   //   "kdsa   r1,r2",    // cannot express constraint   // msa9
   //   "dfltcc r1,r2,r3", // cannot express constraint   // dflt
   //   "nnpa",            // cannot express constraint   // nnpa

   // sortl   not implemented
   "vclfnh  v1,v2,m3:{3},m4:{0}",    // nnpa  no spec exc. for m3, m4 but IEEE exc.
   "vclfnl  v1,v2,m3:{3},m4:{0}",    // nnpa  no spec exc. for m3, m4 but IEEE exc.
   "vcrnf   v1,v2,v3,m4:{0},m5:{2}", // nnpa  no spec exc. for m4, m5 but IEEE exc.
   "vcfn    v1,v2,m3:{1},m4:{0}",    // nnpa  no spec exc. for m3, m4 but IEEE exc.
   "vcnf    v1,v2,m3:{0},m4:{1}",    // nnpa  no spec exc. for m3, m4 but IEEE exc.
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


/* GPRs, VRs, ARs and FPRs are understood. */
static opnd
register_operand(const char *opnd_string)
{
   opnd_t kind = OPND_GPR;
   unsigned num_bits = 4;

   if (opnd_string[0] == 'v') {
      kind = OPND_VR;
      num_bits = 5;
   } else if (opnd_string[0] == 'f') {
      kind = OPND_FPR;
   } else if (opnd_string[0] == 'a') {
      kind = OPND_AR;
   }

   const char *p = skip_digits(opnd_string + 1);

   if (p == opnd_string + 1) {   // no digits found
      error("%s: invalid register name\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   char *name = strnsave(opnd_string, (unsigned)(p - opnd_string));
   long long *allowed_values = NULL;

   if (*p == ':') {
      ++p;
      if (p[0] == 's' || p[0] == 'u') {
         error("%s: specification of signedness is invalid for registers\n", name);
         return invalid_opnd(name);
      }

      if (p[0] != '{') {
         error("%s: expected '{'\n", name);
         return invalid_opnd(name);
      }

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
   if (*p != '\0') {
      error("'%s' is not understood\n", opnd_string);
      return invalid_opnd(strsave(opnd_string));
   }

   return (opnd){ .name = name, .kind = kind, .num_bits = num_bits,
                  .is_unsigned = 1, .allowed_values = allowed_values };
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
   case 'a':    // AR
   case 'f':    // FPR
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
   case OPND_AR:      return "ar";
   case OPND_FPR:     return "fpr";
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
      case OPND_AR:
      case OPND_FPR:
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
