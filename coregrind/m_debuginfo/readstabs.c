
/*--------------------------------------------------------------------*/
/*--- Read stabs debug info.                           readstabs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/

/* "on Linux (except android), or on Darwin" */
#if (defined(VGO_linux) && \
    !(defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android) \
      || defined(VGPV_mips32_linux_android)) \
    || defined(VGO_darwin))

#include "pub_core_basics.h"
#include "pub_core_debuginfo.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "priv_misc.h"             /* dinfo_zalloc/free/strdup */
#include "priv_image.h"
#include "priv_tytypes.h"
#include "priv_d3basics.h"
#include "priv_storage.h"
#include "priv_readstabs.h"        /* self */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#if defined(VGO_linux)
#  include <a.out.h> /* stabs defns */
#elif defined(VGO_darwin)
#  include <mach-o/nlist.h>
#  define n_other n_sect
#  if VG_WORDSIZE == 8
#     define nlist nlist_64
#  endif
#else
#  error "Unknown OS"
#endif
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/*------------------------------------------------------------*/
/*--- Read STABS format debug info.                        ---*/
/*------------------------------------------------------------*/

/* Stabs entry types, from:
 *   The "stabs" debug format
 *   Menapace, Kingdon and MacKenzie
 *   Cygnus Support
 */
typedef enum { N_UNDEF = 0,	/* undefined symbol, new stringtab  */
	       N_GSYM  = 32,    /* Global symbol                    */
               N_FUN   = 36,    /* Function start or end            */
               N_STSYM = 38,    /* Data segment file-scope variable */
               N_LCSYM = 40,    /* BSS segment file-scope variable  */
               N_RSYM  = 64,    /* Register variable                */
               N_SLINE = 68,    /* Source line number               */
               N_SO    = 100,   /* Source file path and name        */
               N_LSYM  = 128,   /* Stack variable or type           */
	       N_BINCL = 130,	/* Beginning of an include file	    */
               N_SOL   = 132,   /* Include file name                */
	       N_PSYM  = 160,   /* Function parameter               */
	       N_EINCL = 162,	/* End of an include file           */
               N_LBRAC = 192,   /* Start of lexical block           */
	       N_EXCL  = 194,	/* Placeholder for an include file  */
               N_RBRAC = 224    /* End   of lexical block           */
             } stab_types;
      

/* Read stabs-format debug info.  This is all rather horrible because
   stabs is a underspecified, kludgy hack.
*/
void ML_(read_debuginfo_stabs) ( DebugInfo* di,
                                 UChar* stabC,   Int stab_sz, 
                                 HChar* stabstr, Int stabstr_sz )
{
   Int    i;
   Int    n_stab_entries;
   struct nlist* stab = (struct nlist*)stabC;
   HChar *next_stabstr = NULL;
   /* state for various things */
   struct {
      Addr     start;         /* start address */
      Addr     end;           /* end address */
      Int      line;          /* first line */
   } func = { 0, 0, -1 };
   struct {
      HChar   *name;
      Bool     same;
   } file = { NULL, True };
   struct {
      Int      prev;          /* prev line */
      Int      no;            /* current line */
      Int      ovf;           /* line wrap */
      Addr     addr;          /* start of this line */
      Bool     first;         /* first line in function */
   } line = { 0, 0, 0, 0, False };

   /* Ok.  It all looks plausible.  Go on and read debug data. 
         stab kinds: 100   N_SO     a source file name
                      68   N_SLINE  a source line number
                      36   N_FUN    start of a function

      In this loop, we maintain a current file name, updated as 
      N_SO/N_SOLs appear, and a current function base address, 
      updated as N_FUNs appear.  Based on that, address ranges for 
      N_SLINEs are calculated, and stuffed into the line info table.

      Finding the instruction address range covered by an N_SLINE is
      complicated;  see the N_SLINE case below.
   */
   file.name     = ML_(addStr)(di,"???", -1);

   n_stab_entries = stab_sz/(int)sizeof(struct nlist);

   TRACE_SYMTAB("\n--- Reading STABS (%d entries) ---\n", n_stab_entries);

   for (i = 0; i < n_stab_entries; i++) {
      const struct nlist *st = &stab[i];
      HChar *string;

      TRACE_SYMTAB("%2d  type=%d   othr=%d   desc=%d   "
                   "value=0x%x   strx=%d  %s\n", i,
                   st->n_type, st->n_other, st->n_desc, 
                   (Int)st->n_value,
                   (Int)st->n_un.n_strx, 
                   stabstr + st->n_un.n_strx );

      /* handle continued string stabs */
      {
         Int   qbuflen = 0;
         Int   qidx = 0;
         HChar* qbuf = NULL;
         Int   qlen;
         Bool  qcontinuing = False;
         UInt  qstringidx;

         qstringidx = st->n_un.n_strx;
         string = stabstr + qstringidx;
         qlen = VG_(strlen)(string);

         while (string 
                && qlen > 0 
                && (qcontinuing || string[qlen-1] == '\\')) {
            /* Gak, we have a continuation. Skip forward through
               subsequent stabs to gather all the parts of the
               continuation.  Increment i, but keep st pointing at
               current stab. */

            qcontinuing = string[qlen-1] == '\\';

            /* remove trailing \ */
            while (string[qlen-1] == '\\' && qlen > 0)
               qlen--;

            TRACE_SYMTAB("cont: found extension string: \"%s\" "
                         "len=%d(%c) idx=%d buflen=%d\n", 
                         string, qlen, string[qlen-1], qidx, qbuflen);

            /* XXX this is silly.  The si->strtab should have a way of
               appending to the last added string... */
            if ((qidx + qlen) >= qbuflen) {
               HChar *n;
               
               if (qbuflen == 0)
                  qbuflen = 16;
               while ((qidx + qlen) >= qbuflen)
                  qbuflen *= 2;
               n = ML_(dinfo_zalloc)("di.readstabs.rds.1", qbuflen);
               VG_(memcpy)(n, qbuf, qidx);
               
               if (qbuf != NULL)
                  ML_(dinfo_free)(qbuf);
               qbuf = n;
            }

            VG_(memcpy)(&qbuf[qidx], string, qlen);
            qidx += qlen;
            if (di->trace_symtab) {
               qbuf[qidx] = '\0';
               TRACE_SYMTAB("cont: working buf=\"%s\"\n", qbuf);
            }

            i++;
            if (i >= n_stab_entries)
               break;

            if (stab[i].n_un.n_strx) {
               string = stabstr + stab[i].n_un.n_strx;
               qlen = VG_(strlen)(string);
            } else {
               string = NULL;
               qlen = 0;
            }
         }

         if (qbuf != NULL) {
            i--;                        /* overstepped */
            string = ML_(addStr)(di, qbuf, qidx);
            ML_(dinfo_free)(qbuf);
            TRACE_SYMTAB("cont: made composite: \"%s\"\n", string);
         }
      }

      switch(st->n_type) {
         case N_UNDEF:
            /* new string table base */
            if (next_stabstr != NULL) {
               stabstr_sz -= next_stabstr - stabstr;
               stabstr = next_stabstr;
               if (stabstr_sz <= 0) {
                  VG_(printf)(" @@ bad stabstr size %d\n", stabstr_sz);
                  return;
               }
            }
            next_stabstr = stabstr + st->n_value;
            break;

         case N_BINCL: {
            break;
         }

         case N_EINCL:
            break;

         case N_EXCL:
            break;

         case N_SOL:                /* sub-source (include) file */
            if (line.ovf != 0) 
               VG_(message)(Vg_UserMsg, 
                            "Warning: file %s is very big (> 65535 lines) "
                            "Line numbers and annotation for this file might "
                            "be wrong.  Sorry.\n",
                            file.name);
            /* FALLTHROUGH */

         case N_SO: {                /* new source file */
            HChar *nm = string;
            UInt len = VG_(strlen)(nm);
            Addr addr = func.start + st->n_value;

            if (line.addr != 0) {
               /* finish off previous line */
               ML_(addLineInfo)(di, file.name, NULL, line.addr,
                                addr, line.no + line.ovf * LINENO_OVERFLOW, i);
            }

            /* reset line state */
            line.ovf = 0;            
            line.addr = 0;
            line.prev = 0;
            line.no = 0;

            if (len > 0 && nm[len-1] != '/') {
               file.name = ML_(addStr)(di, nm, -1);
               TRACE_SYMTAB("new source: %s\n", file.name);
            } else if (len == 0)
               file.name = ML_(addStr)(di, "?1\0", -1);

            break;
         }

         case N_SLINE: {        /* line info */
            Addr addr = func.start + st->n_value;

            if (line.addr != 0) {
               /* there was a previous */
               ML_(addLineInfo)(di, file.name, NULL, line.addr,
                                addr, line.no + line.ovf * LINENO_OVERFLOW, i);
            }

            line.addr = addr;
            line.prev = line.no;
            line.no = (Int)((UShort)st->n_desc);

            if (line.prev > line.no + OVERFLOW_DIFFERENCE && file.same) {
               VG_(message)(Vg_DebugMsg, 
                  "Line number overflow detected (%d --> %d) in %s\n", 
                  line.prev, line.no, file.name);
               line.ovf++;
            }
            file.same = True;

            /* This is pretty horrible.  If this is the first line of
               the function, then bind any unbound symbols to the arg
               scope, since they're probably arguments. */
            if (line.first) {
               line.first = False;
               
               /* remember first line of function */
               if (func.start != 0) {
                  func.line = line.no;
               }
            }
            break;
         }

         case N_FUN: {                /* function start/end */
            Addr addr = 0;        /* end address for prev line/scope */

            /* if this the end of the function or we haven't
               previously finished the previous function... */
            if (*string == '\0' || func.start != 0) {
               /* end of function */
               line.first = False;

               /* end line at end of function */
               addr = func.start + st->n_value;

               /* now between functions */
               func.start = 0;

               // XXXX DEAD POINT XXXX
            }

            if (*string != '\0') {
               /* new function */
               line.first = True;

               /* line ends at start of next function */
               addr = di->text_debug_bias + st->n_value;

               func.start = addr;
            }

            if (line.addr) {
               ML_(addLineInfo)(di, file.name, NULL, line.addr,
                                addr, line.no + line.ovf * LINENO_OVERFLOW, i);
               line.addr = 0;
            }

            //DEAD POINT
            //DEAD POINT
            break;
         }

         case N_LBRAC: {
            /* open new scope */
            // DEAD POINT
            break;
         }

         case N_RBRAC: {
            /* close scope */
            // DEAD POINT
            break;
         }

         case N_GSYM:                /* global variable */
         case N_STSYM:                /* static in data segment */
         case N_LCSYM:                /* static in bss segment */
         case N_PSYM:                /* function parameter */
         case N_LSYM:                /* stack variable */
         case N_RSYM:                  /* register variable */
            break;
      }
   }
}

#endif /* (defined(VGO_linux) && !defined(VGPV_*_linux_android)) \
          || defined(VGO_darwin) */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
