
/*--------------------------------------------------------------------*/
/*--- Launching Valgrind on AIX5.                  launcher-aix5.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Cut-down version of the normal launcher, except it is completely
   different on AIX5.  Does not handle shell scripts, only real
   machine code XCOFF executables.

   Note: this is a "normal" program and not part of Valgrind proper,
   and so it doesn't have to conform to Valgrind's arcane rules on
   no-glibc-usage etc.
*/

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/ptrace.h>
#include <sys/wait.h>

/* Get both struct __ld_info32 and struct __ld_info64. */
#define __LDINFO_PTRACE32__ 1
#define __LDINFO_PTRACE64__ 1
#include <sys/ldr.h>

#include <sys/reg.h>     /* GPR0 .. GPR31 */
#include <sys/procfs.h>  /* prsysent_t */

#include "pub_core_debuglog.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcproc.h"  // For VALGRIND_LIB, VALGRIND_LAUNCHER

/* Get the definition for the AIX5Bootblock structure.  This is what
   we will generate and patch into the child's address space. */
#include "launcher-aix5-bootblock.h"

/* Simple routines for Huffman compression/decompression */
#include "m_initimg/simple_huffman.c"


/* -------------------------------------------------------------- */
/* ---                                                        --- */
/* --- A uniform interface to the ptrace facilities we need.  --- */
/* ---                                                        --- */
/* -------------------------------------------------------------- */

typedef
   struct {
      pid_t pid;
      Bool  is64;
   } 
   Child;


/* Read len bytes from target's rsrc to local ldst.  Returns True if
   error. */
static 
Bool ptrace_READ_BLOCK ( Child* ch, Int len, void* ldst, Addr64 rsrc )
{
   Int r;
   assert(len >= 0 && len <= 1024);
   r = ptrace64( PT_READ_BLOCK, (ULong)ch->pid, rsrc, len, ldst );
   if (r == len)
      return False; /* success */
   return True; /* error */
}


/* Write len bytes to target's rdst from local lsrc.  Returns True if
   error. */
static
Bool ptrace_WRITE_BLOCK ( Child* child, Int len, Addr64 rdst, void* lsrc )
{
   Int r;
   assert(len >= 0 && len <= 1024);
   r = ptrace64( PT_WRITE_BLOCK, (ULong)child->pid, rdst, len, lsrc );
   if (r == len)
      return False; /* success */
   return True; /* error */
}


/* Read a GPR from the target.  Returns True if error. */
static
Bool ptrace_READ_GPR ( Child* child, Int reg, ULong* ldst )
{
   ULong w64;
   UInt  w32;
   errno = 0;
   if (child->is64) {
      (void)ptrace64( PT_READ_GPR, 
                      (ULong)child->pid, (ULong)reg, 8, (Int*)(&w64) );
      if (errno != 0) return True; /* error */
   } else {
      w32 = ptrace64( PT_READ_GPR, 
                      (ULong)child->pid, (ULong)reg, 0, 0 );
      if (errno != 0) return True; /* error */
      w64 = (ULong)w32;
   }
   *ldst = w64;
   return False; /* success */
}


/* Write a GPR to the target.  Returns True if error. */
static
Bool ptrace_WRITE_GPR ( Child* child, Int reg, ULong val )
{
   ULong w64;
   UInt w32;
   errno = 0;
   if (child->is64) {
      w64 = val;
      (void)ptrace64( PT_WRITE_GPR,
                      (ULong)child->pid, (ULong)reg, 8, (Int*)&w64 );
      if (errno != 0) return True; /* error */
   } else {
      w32 = (UInt)val;
      (void)ptrace64( PT_WRITE_GPR, 
                      (ULong)child->pid, (ULong)reg, w32, 0 );
      if (errno != 0) return True; /* error */
   }
   return False; /* success */
}


/* -------------------------------------------------------------- */
/* ---                                                        --- */
/* --- Helper functions                                       --- */
/* ---                                                        --- */
/* -------------------------------------------------------------- */

/* Search the path for the client program */
static const char* find_client ( const char* clientname )
{
   static char fullname[PATH_MAX];
   const char *path = getenv("PATH");
   const char *colon;

   while (path)
   {
      if ((colon = strchr(path, ':')) == NULL)
      {
         strcpy(fullname, path);
         path = NULL;
      }
      else
      {
         memcpy(fullname, path, colon - path);
         fullname[colon - path] = '\0';
         path = colon + 1;
      }
      strcat(fullname, "/");
      strcat(fullname, clientname);

      if (access(fullname, R_OK|X_OK) == 0)
         return fullname;
    }

    return clientname;
}

/* Examine the given file.  If it looks like valid XCOFF32 return 32,
   if valid XCOFF64 return 64, else return 0. */
static Int examine_client ( const char* clientname )
{
   UChar buf[16];
   Int n;
   FILE* f = fopen( clientname, "r" );
   if (f == NULL)
      return 0;
   n = fread( buf, 1, 16, f );
   fclose(f);
   if (n != 16)
      return 0;
   if (buf[0] == 0x01 && buf[1] == 0xDF)
      return 32; /* XCOFF32 */
   if (buf[0] == 0x01 && buf[1] == 0xF7)
      return 64; /* XCOFF64 */
   return 0;
}

static Bool file_exists ( char* fname )
{
   struct stat buf;
   int r = stat(fname, &buf);
   return r == 0;
}

static Addr64 ROUNDDN_PAGE ( Addr64 v )
{
   ULong p = (ULong)v;
   ULong a = PAGE_SIZE;
   p &= ~(a-1);
   return (Addr64)p;
}

static Bool IS_PAGE_ALIGNED ( Addr64 v )
{
   ULong p = (ULong)v;
   ULong a = PAGE_SIZE;
   if (p & (a-1))
      return False;
   else
      return True;
}

static Bool IS_8_ALIGNED ( Addr64 v )
{
   ULong p = (ULong)v;
   ULong a = 8;
   if (p & (a-1))
      return False;
   else
      return True;
}


/* Read a 4096-byte page from CHILD's address space at location SRC,
   into local address space at DST.  Returns True if error, False
   otherwise.
*/
static Bool ptrace_read_page ( Child* child, UChar* ldst, Addr64 rsrc )
{
   Int  off;
   Bool err;

   assert(IS_PAGE_ALIGNED(rsrc));

   off = 0;
   err = ptrace_READ_BLOCK(child, 1024, ldst + off, rsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_READ_BLOCK(child, 1024, ldst + off, rsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_READ_BLOCK(child, 1024, ldst + off, rsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_READ_BLOCK(child, 1024, ldst + off, rsrc + off);
   if (err) return err;

   off += 1024;
   assert(off == PAGE_SIZE);

   return False;
}


/* Write a 4096-byte page from local address space at SRC to CHILD's
   address space at location DST.  Returns True if error, False
   otherwise.
*/
static Bool ptrace_write_page ( Child* child, Addr64 rdst, UChar* lsrc )
{
   Int  off;
   Bool err;

   assert(IS_PAGE_ALIGNED(rdst));

   off = 0;
   err = ptrace_WRITE_BLOCK(child, 1024, rdst + off, lsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_WRITE_BLOCK(child, 1024, rdst + off, lsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_WRITE_BLOCK(child, 1024, rdst + off, lsrc + off);
   if (err) return err;

   off += 1024;
   err = ptrace_WRITE_BLOCK(child, 1024, rdst + off, lsrc + off);
   if (err) return err;

   off += 1024;
   assert(off == PAGE_SIZE);

   return False;
}


/* Get 37 integer registers (GPR0 .. GPR31, PC, CR, LR, CTR, XER) from
   CHILD into the given array.  Returns True if there is any kind of
   error. */
static 
Bool ptrace_get_iregs_pc_cr_lr_ctr_xer ( 
        Child* child, 
        /*OUT*/ULong* iregs_pc_cr_lr_ctr_xer 
     )
{
   Int  i, j;
   Bool err;

   for (i = GPR0; i <= GPR31; i++) {
      j = i - GPR0;
      assert(j >= 0 && j < 32);
      err = ptrace_READ_GPR( child, i, &iregs_pc_cr_lr_ctr_xer[j] );
      if (err) return err;
   }

   /* PC */
   err = ptrace_READ_GPR( child, IAR, &iregs_pc_cr_lr_ctr_xer[32+0] );
   if (err) return err;

   /* CR */
   err = ptrace_READ_GPR( child, CR, &iregs_pc_cr_lr_ctr_xer[32+1] );
   if (err) return err;

   /* LR */
   err = ptrace_READ_GPR( child, LR, &iregs_pc_cr_lr_ctr_xer[32+2] );
   if (err) return err;

   /* CTR */
   err = ptrace_READ_GPR( child, CTR, &iregs_pc_cr_lr_ctr_xer[32+3] );
   if (err) return err;

   /* XER */
   err = ptrace_READ_GPR( child, XER, &iregs_pc_cr_lr_ctr_xer[32+4] );
   if (err) return err;

   return False;
}


/* Set CHILD's program counter to the given value.  Returns True if
   there is any kind of error. */
static 
Bool ptrace_put_pc ( Child* child, ULong newpc )
{
   return ptrace_WRITE_GPR( child, IAR, newpc );
}


/* Set CHILD's R31 to the given value.  Returns True if there is any
   kind of error. */
static 
Bool ptrace_put_r31 ( Child* child, ULong newr31 )
{
   return ptrace_WRITE_GPR( child, GPR31, newr31 );
}


/* ------ Instruction generators ------ */

static UInt mkFormD ( UInt opc1, UInt r1, UInt r2, UInt imm )
{
   UInt theInstr;
   assert(opc1 < 0x40);
   assert(r1   < 0x20);
   assert(r2   < 0x20);
   imm = imm & 0xFFFF;
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (imm));
   return theInstr;
}
static UInt mkFormX ( UInt opc1, 
                      UInt r1, UInt r2, UInt r3, UInt opc2, UInt b0 )
{
   UInt theInstr;
   assert(opc1 < 0x40);
   assert(r1   < 0x20);
   assert(r2   < 0x20);
   assert(r3   < 0x20);
   assert(opc2 < 0x400);
   assert(b0   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (opc2<<1) | (b0));
   return theInstr;
}
static UInt mkFormXFX ( UInt r1, UInt f2, UInt opc2 )
{
   UInt theInstr;
   assert(r1   < 0x20);
   assert(f2   < 0x20);
   assert(opc2 < 0x400);
   switch (opc2) {
   case 144:  // mtcrf
      assert(f2 < 0x100);
      f2 = f2 << 1;
      break;
   case 339:  // mfspr
   case 371:  // mftb
   case 467:  // mtspr
      assert(f2 < 0x400);
      // re-arrange split field
      f2 = ((f2>>5) & 0x1F) | ((f2 & 0x1F)<<5);
      break;
   default: assert(0);
   }
   theInstr = ((31<<26) | (r1<<21) | (f2<<11) | (opc2<<1));
   return theInstr;
}
static UInt mkFormMD ( UInt opc1, UInt r1, UInt r2,
                       UInt imm1, UInt imm2, UInt opc2 )
{
   UInt theInstr;
   assert(opc1 < 0x40);
   assert(r1   < 0x20);
   assert(r2   < 0x20);
   assert(imm1 < 0x40);
   assert(imm2 < 0x40);
   assert(opc2 < 0x08);
   imm2 = ((imm2 & 0x1F) << 1) | (imm2 >> 5);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               ((imm1 & 0x1F)<<11) | (imm2<<5) |
               (opc2<<2) | ((imm1 >> 5)<<1));
   return theInstr;
}
static UInt mkFormXO ( UInt opc1, UInt r1, UInt r2,
                       UInt r3, UInt b10, UInt opc2, UInt b0 )
{
   UInt theInstr;
   assert(opc1 < 0x40);
   assert(r1   < 0x20);
   assert(r2   < 0x20);
   assert(r3   < 0x20);
   assert(b10  < 0x2);
   assert(opc2 < 0x200);
   assert(b0   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (b10 << 10) | (opc2<<1) | (b0));
   return theInstr;
}

static UInt gen_lis_r_N ( UInt r, UInt N ) {
   return mkFormD(15, r, 0, N & 0xFFFF); /* lis r,r,N */
}
static UInt gen_ori_r_r_N ( UInt r, UInt N ) {
   return mkFormD(24, r, r, N & 0xFFFF); /* ori r,r,N */
}
static UInt gen_addi_rd_rs_N ( UInt rd, UInt rs, UInt N ) {
   assert(rs != 0);
   return mkFormD(14, rd, rs, N & 0xFFFF); /* addi rd,rs,N */
}
static UInt gen_addis_rd_rs_N ( UInt rd, UInt rs, UInt N ) {
   assert(rs != 0);
   return mkFormD(15, rd, rs, N & 0xFFFF); /* addis rd,rs,N */
}
static UInt gen_crorc_6_6_6 ( void ) {
   return 0x4CC63342; /* crorc 6,6,6 */
}
static UInt gen_mr_rd_rs ( UInt rd, UInt rs ) {
   return mkFormX(31, rs, rd, rs, 444, 0); /* or rd,rs,ts */
}
static UInt gen_bl_next ( void ) {
   return 0x48000005; /* bl .+4 */
}
static UInt gen_mflr_r ( UInt r ) {
   return mkFormXFX(r, 8, 339); /* mflr r */
}
static UInt gen_mtlr_r ( UInt r ) {
   return mkFormXFX(r, 8, 467); /* mtlr r */
}
static UInt gen_blr ( void ) {
   return 0x4E800020; /* blr */
}
__attribute__((unused))
static UInt gen_blrl ( void ) {
   return 0x4E800021; /* blrl */
}
static UInt gen_add_r_N ( UInt r, UInt N ) {
   return mkFormD(14, r, r, N & 0xFFFF); /* addi r,r,N */
}
static UInt gen_cmpli_cr7_r_N ( UInt r, UInt N ) {
   return mkFormD(10, 7<<2, r, N & 0xFFFF); /* cmpli cr7,r,N */
}
static UInt gen_bne_cr7_delta ( UInt delta ) {
   return 0x409E0000 | (delta & 0x0000FFFC); /* bne- cr7,delta */
}
__attribute__((unused))
static UInt gen_beq_cr7_delta ( UInt delta ) {
   return 0x419E0000 | (delta & 0x0000FFFC); /* beq- cr7,delta */
}
static UInt gen_sc ( void ) {
   return 0x44000002; /* sc */
}
static UInt gen_lwz_rd_off_ra ( UInt rd, UInt off, UInt ra ) {
   return mkFormD(32, rd, ra, off); /* lwz rd, off(ra) */
}
static UInt gen_add_rd_rL_rR (UInt rd, UInt rsrcL, UInt rsrcR ) {
   return mkFormXO(31, rd, rsrcL, rsrcR, 0, 266, 0);
}
static UInt gen_subf_rd_rL_rR (UInt rd, UInt rsrcL, UInt rsrcR ) {
   return mkFormXO(31, rd, rsrcL, rsrcR, 0, 40, 0);
}

static Int emit_insn ( UInt* code, Int ix, UInt insn ) {
   code[ix++] = insn;
   return ix;
}
static Int emit_li32 ( UInt* code, Int ix, UInt rd, UInt imm32 ) {
   code[ix++] = gen_lis_r_N(rd, imm32 >> 16);
   if (imm32 & 0xFFFF)
      code[ix++] = gen_ori_r_r_N(rd, imm32 & 0xFFFF);
   return ix;
}
static Int emit_dosc ( UInt* code, Int ix ) {
   /* Generate code to do a syscall and continue at the next insn.
      Note: trashes r29. */
   code[ix++] = gen_crorc_6_6_6();
   code[ix++] = gen_bl_next();
   code[ix++] = gen_mflr_r(29);
   code[ix++] = gen_add_r_N(29,16);
   code[ix++] = gen_mtlr_r(29);
   code[ix++] = gen_sc();
   return ix;
}

/* Generate 64-bit insns */
static Int emit_li64 ( UInt* code, Int ix, UInt rd, ULong imm64 ) {
   if (imm64 >= 0xFFFFFFFF80000000ULL || imm64 < 0x80000000ULL) {
      // sign-extendable from 32 bits
      // addis rd,r0,(imm64>>16) => lis rd, (imm64>>16)
      code[ix++] = mkFormD(15, rd, 0, (imm64>>16) & 0xFFFF);
      // ori rd, rd, (imm64 & 0xFFFF)
      code[ix++] = mkFormD(24, rd, rd, imm64 & 0xFFFF);
   } else {
      // load high word
      // lis rd, (imm64>>48) & 0xFFFF
      code[ix++] = mkFormD(15, rd, 0, (imm64>>48) & 0xFFFF);
      // ori rd, rd, (imm64>>32) & 0xFFFF
      code[ix++] = mkFormD(24, rd, rd, (imm64>>32) & 0xFFFF);
      // shift rd low word to high word => rldicr
      code[ix++] = mkFormMD(30, rd, rd, 32, 31, 1);
      // load low word
      // oris rd, rd, (imm64>>16) & 0xFFFF
      code[ix++] = mkFormD(25, rd, rd, (imm64>>16) & 0xFFFF);
      // ori rd, rd, (imm64) & 0xFFFF
      code[ix++] = mkFormD(24, rd, rd, imm64 & 0xFFFF);
   }
   return ix;
}
static UInt gen_ld_rd_off_ra ( UInt rd, UInt off, UInt ra ) {
   assert((off & 3) == 0);
   return mkFormD(58, rd, ra, off); /* ld rd, off(ra) */
}

static UInt compute_adler32 ( void* addr, UWord len )
{
   UInt   s1 = 1;
   UInt   s2 = 0;
   UChar* buf = (UChar*)addr;
   while (len > 0) {
      s1 += buf[0];
      s2 += s1;
      s1 %= 65521;
      s2 %= 65521;
      len--;
      buf++;
   }
   return (s2 << 16) + s1;
}


/* -------------------------------------------------------------- */
/* ---                                                        --- */
/* --- BEGIN write bootstrap loader into child process        --- */
/* ---                                                        --- */
/* -------------------------------------------------------------- */

/* From using truss, __loadx is used to load a module into a running
   process in 32-bit mode, and kload in 64-bit mode.  __loadx is
   simple: it returns a pointer to a standard function descriptor to
   the entry point.

   kload isn't: it returns a pointer which, from examination of
   /proc/<pid>/maps, doesn't point into the loaded object image.  It
   does appear to point to some kind of struct, words [4] and [6] of
   which do point into the loaded object image.  From comparison with
   /proc/<pid>/maps, they are respectively the actual VMAs of the text
   and data sections of the loaded module.

   Knowing this it is possible to find the entry point descriptor:
   - figure out where the auxiliary header is.  We have a pointer to
     the start of the mapped text section, so just add the size of
     the XCOFF file header to that.
   - figure out the data bias.  We know the avma of the data section;
     and the svma of it is in the auxiliary header in field
     o_data_start.  The data bias is therefore the difference between
     them.
   - The auxiliary header also gives the svma of the entry point
     descriptor; (o_entry); therefore its avma is o_entry + the data
     bias.

   ULong* kr  = (result of kload)
   // r3 is this value

   AOUTHDR* aux = kr[4] (text_avma) + 24 (size of XCOFF file header);
   // ld 9,32(3)     kr[4]
   // addi 9,9,24    + 24
   // 9=aux

   ULong data_avma = kr[6];
   // ld 11,48(3)    kr[6]
   // 9=aux
   // 11=data_avma

   ULong data_svma = aux->o_data_start;
   // ld 0,16(9)     aux->o_data_start
   // 9=aux
   // 11=data_avma
   // 0=data_svma

   ULong data_bias = data_avma - data_svma;
   // subf 11,0,11
   // 9=aux
   // 11=data_bias
   // 0=data_svma

   ULong ent_svma = (ULong)aux->o_entry;
   // ld 9,80(9)   aux->o_entry
   // 9=ent_svma
   // 11=data_bias
   // 0=data_svma

   ULong ent_avma = ent_svma + data_bias;
   // add 10,9,11
   // 9=ent_svma
   // 11=data_bias
   // 0=data_svma
   // 10=ent_avma
*/

#define LAUNCHER_SYSENT_SIZE 100000
static char sysent_buf[LAUNCHER_SYSENT_SIZE];

/* The executable loaded must have no more than N_LDINFOs direct
   shared-object dependencies.  Just increase this value and rebuild,
   if you ever run out.  We have two arrays, one for each kind of
   target process. */
#define N_LDINFOs 1000
static  struct __ld_info32  ld_info32_array[N_LDINFOs];
static  struct __ld_info64  ld_info64_array[N_LDINFOs];


static 
UChar* bootstrap_errmsg 
         = "\nvalgrind: bootstrap loader failed.  Cannot continue.\n\n";


/* Write the bootstrap loader and associated data (iow, an
   AIX5Bootblock structure) into CHILD, so that when
   ptrace-detached, it will continue by loading TOOLNAME and
   continuing with that.  Returns NULL on success or an error string
   on failure. */

static char* write_bootstrap_loader_into_child 
                ( Child* child, char* toolfile )
{
   /* ------ STEP 1: Fill in most parts of the bootblock. ------ */

   /* All parts except code[], off_zdata and len_zdata. */

   AIX5Bootblock block;

   VG_(debugLog)(1, "launcher", "parent: size of bootblock is %ld\n",
                    sizeof(AIX5Bootblock));

   assert(IS_8_ALIGNED( sizeof(AIX5Bootblock) ));

   memset(&block, 0, sizeof(block));

   /* --- OFFSETS--- */

   /* off_zdata not known yet */
   /* len_zdata not known yet */

   /* --- SYSCALL NUMBERS --- */

   /* Read some system call entries from the child's
      /proc/<pid>/sysent file. */
   char        sysent_name[50];
   FILE*       sysent_file;
   int         sysent_used = 0;
   prsysent_t* sysent_hdr;
   int         i;

   VG_(debugLog)(1, "launcher", 
                    "parent: reading child's /proc/../sysent\n");

   sprintf(sysent_name, "/proc/%d/sysent", child->pid);
   sysent_file = fopen(sysent_name, "r");
   if (sysent_file == NULL)
      return "Can't open child's /proc/<pid>/sysent file";

   sysent_used = fread(sysent_buf, 1, LAUNCHER_SYSENT_SIZE, sysent_file);
   if (sysent_used == 0)
      return "Error reading child's /proc/<pid>/sysent file";
   if (sysent_used == LAUNCHER_SYSENT_SIZE)
      return "LAUNCHER_SYSENT_SIZE is too low; increase and recompile";
   assert(sysent_used > 0 && sysent_used < LAUNCHER_SYSENT_SIZE);

   fclose(sysent_file);

   sysent_hdr = (prsysent_t*)&sysent_buf[0];

   /* Find some syscall numbers for the child. */
   Int __nr__getpid = -1;
   Int __nr_kwrite  = -1;
   Int __nr___loadx = -1; /* 32-bit child only */
   Int __nr_kload   = -1; /* 64-bit child only */
   Int __nr__exit   = -1;
   Int __nr_open    = -1;
   Int __nr_kread   = -1;
   Int __nr_close   = -1;

   for (i = 0; i < sysent_hdr->pr_nsyscalls; i++) {
      char* name = &sysent_buf[ sysent_hdr->pr_syscall[i].pr_nameoff ];
      int   nmbr = sysent_hdr->pr_syscall[i].pr_number;
      if (0 == strcmp(name, "_getpid"))
         __nr__getpid = nmbr;
      if (0 == strcmp(name, "kwrite"))
          __nr_kwrite = nmbr;
      if (0 == strcmp(name, "__loadx"))
          __nr___loadx = nmbr;
      if (0 == strcmp(name, "kload"))
          __nr_kload = nmbr;
      if (0 == strcmp(name, "_exit"))
          __nr__exit = nmbr;
      if (0 == strcmp(name, "open"))
          __nr_open = nmbr;
      if (0 == strcmp(name, "kread"))
          __nr_kread = nmbr;
      if (0 == strcmp(name, "close"))
          __nr_close = nmbr;
   }

   if (__nr__getpid == -1 
       || __nr_kwrite == -1 
       || ((!child->is64) && __nr___loadx == -1)
       || ((child->is64) && __nr_kload == -1)
       || __nr__exit == -1
       || __nr_open == -1 
       || __nr_kread == -1 
       || __nr_close == -1)
      return "can't establish syscall #s needed for bootstrap";

   block.__NR_getpid = __nr__getpid;
   block.__NR_write  = __nr_kwrite;
   block.__NR_exit   = __nr__exit;
   block.__NR_open   = __nr_open;
   block.__NR_read   = __nr_kread;
   block.__NR_close  = __nr_close;

   /* --- REGS --- */

   /* Continue by copying out the child's current integer register
      state. */
   VG_(debugLog)(1, "launcher", 
                    "parent: reading child's int registers\n");

   Bool err = ptrace_get_iregs_pc_cr_lr_ctr_xer 
                 ( child, &block.iregs_pc_cr_lr_ctr_xer[0] );
   if (err)
      return "read of child's int registers failed";

   /* --- CODE --- */

   /* We'll leave that till last (is difficult). */

   /* --- ERRMSG --- */

   if (1 + strlen(bootstrap_errmsg) > N_BOOTBLOCK_ERRMSG)
      return "bootstrap error message won't fit in bootblock";

   for (i = 0; bootstrap_errmsg[i]; i++)
      block.errmsg[i] = bootstrap_errmsg[i];
   assert(i <= N_BOOTBLOCK_ERRMSG);

   /* --- TOOLFILE --- */

   if (1 + strlen(toolfile) > N_BOOTBLOCK_TOOLFILE)
      return "tool file path is too long, won't fit in bootblock";

   for (i = 0; toolfile[i]; i++)
      block.toolfile[i] = toolfile[i];
   assert(i <= N_BOOTBLOCK_TOOLFILE);


   /* ------ STEP 2: Generate the bootblock code. ------ */

   VG_(debugLog)(1, "launcher", 
                    "parent: creating bootblock code ..\n");

   /* This is the tricky bit.  The resulting code has to be position
      independent since we don't yet know where it's going to be
      placed.  The code is entered with r31 pointing at the bootblock.
      r29-31 are callee-saved, so presumably they don't get trashed
      across syscalls.  r30 is used as scratch, and r29 is also used
      as scratch by 'emit_dosc'. */

   /* Preliminaries: to do a syscall, we have to do 'crorc 6,6,6' and
      put the continuation address in LR, which is a bit of a drag.
      Hence the following macro:

         SYSCALL_SEQUENCE = crorc 6,6,6
                            bl   .+4
                            mflr 29
                            addi 29,29,16
                            mtlr 29
                            sc

      Also: 'imm' is an imaginary instruction to get a 32-bit literal into
      a register.  It's really li followed by oris.
   */

   /* So, the code.  First, prepare for and do a _loadx syscall, to
      get the tool aboard:
         addis 1, 1, -4
         imm  2, __NR__loadx
         imm  3, VKI_DL_LOAD
         mr   4, 1
         imm  5, 3<<16
         addi 6, 31, offset_of_toolfile
         mr   7, 4
         mr   8, 4
         mr   9, 4
         mr   10,4
         SYSCALL_SEQUENCE
         addis 1, 1, 4

      If the syscall failed, r4 will be nonzero.  Branch elsewhere if so.
         cmpi 4, 0
         bne  error
   */
   int ix = 0;

#  if 1
#  define TRAP \
      do { \
         ix=emit_insn( &block.code[0],ix, 0x7fe00008 ); } \
      while (0)
#  define SEGV \
      do { \
         if (child->is64) { \
            ix=emit_li64( &block.code[0],ix, 28,0); \
            ix=emit_insn( &block.code[0],ix, \
                          gen_ld_rd_off_ra(27,0xfffc,28)); \
         } else { \
            ix=emit_li32( &block.code[0],ix, 28,0); \
            ix=emit_insn( &block.code[0],ix, \
                          gen_lwz_rd_off_ra(27,0xffff,28)); \
	 } \
      } while (0)
#  define ILL \
      do { \
         ix=emit_insn( &block.code[0],ix, 0 ); } \
      while (0)      
#  endif

   if (child->is64) {

      /* 64-bit sequence */
      /* Set up for 'sys_kload(toolfile, 0, 0)'
         li64  2, __NR_kload
         addi  3, 31, offset_toolfile
         li64  4, 0
         mr    5, 4
         mr    6, 4
         mr    7, 4
         mr    8, 4
         mr    9, 4
         mr    10,4
         SYSCALL_SEQUENCE

         // if kload failed, r3 will hold zero
         cmpdi 3,0
         beq error

         // from result of kload, figure out entry point address
         // as described above
         ld   9,32(3)
         addi 9,9,24
         ld   11,48(3)
         ld   0,16(9)
         subf 11,0,11
         ld   9,80(9)
         add  10,9,11  // r10 is entry descriptor avma

         void(*fn)(void*) = (void(*)(void*))ent_avma;
         fn();
         ld   9,0(10)
         mtlr 9
         ld   2,8(10)
         ld   11,16(10)
         mr   3,31  // arg to pass
         blr
      */
      ix = emit_li64( &block.code[0],ix, 2, __nr_kload );
      ix = emit_insn( &block.code[0],ix, 
                      gen_addi_rd_rs_N(3,31,offsetof(AIX5Bootblock,toolfile)));
      ix = emit_li64( &block.code[0],ix, 4, 0 );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(5,4) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(6,4) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(7,4) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(8,4) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(9,4) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(10,4) );
      ix = emit_dosc( &block.code[0],ix );

      ix = emit_insn( &block.code[0],ix, gen_cmpli_cr7_r_N(3,0) );
      Int ix_beq = ix; /* Patch this later */
      ix = emit_insn( &block.code[0],ix, 0 );

      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 9, 32, 3 ) );
      ix = emit_insn( &block.code[0],ix, gen_addi_rd_rs_N( 9, 9, 24 ) );
      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 11, 48, 3 ) );
      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 0, 16, 9 ) );
      ix = emit_insn( &block.code[0],ix, gen_subf_rd_rL_rR( 11, 0, 11 ) );
      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 9, 80, 9 ) );
      ix = emit_insn( &block.code[0],ix, gen_add_rd_rL_rR( 10, 9, 11 ) );

      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 9, 0, 10 ) );
      ix = emit_insn( &block.code[0],ix, gen_mtlr_r( 9 ) );
      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 2, 8, 10 ) );
      ix = emit_insn( &block.code[0],ix, gen_ld_rd_off_ra( 11, 16, 10 ) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(3, 31) );
      ix = emit_insn( &block.code[0],ix, gen_blr() );
      TRAP;
      assert(ix <= N_BOOTBLOCK_INSNS);

      /* error:
         We get here if the kload syscall fails.  Write a terse message
         to stderr saying so, then exit, carrying the error code of the
         kload call.  The latter is saved in r30 across the write() call.
            mr   30,4 (4 contains the error result from kload)
            imm  2, __NR_write
            imm  3,2 (2=stderr)
            addi 4, 31, offset_of_errormsg
            imm  5, length(errormsg)
            SYSCALL_SEQUENCE
            imm  2, __NR_exit
            mr   3, 30
            SYSCALL_SEQUENCE
   
         Well, we shouldn't be alive here.  But just in case we do, put
         a zero word, which will generate SIGILL and definitely stop the
         party.
            .word 0
      */
      /* fill in the conditional jump */
      (void)emit_insn( &block.code[0],ix_beq, 
                                      gen_beq_cr7_delta(4*(ix-ix_beq)));
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(30,4) );
      ix = emit_li64( &block.code[0],ix, 2, __nr_kwrite);
      ix = emit_li64( &block.code[0],ix, 3, 2);
      ix = emit_insn( &block.code[0],ix, 
                      gen_addi_rd_rs_N(4,31,offsetof(AIX5Bootblock,errmsg)));
      ix = emit_li64( &block.code[0],ix, 5, strlen(bootstrap_errmsg));
      ix = emit_dosc( &block.code[0],ix );
      ix = emit_li64( &block.code[0],ix, 2, __nr__exit);
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(3,30) );
      ix = emit_dosc( &block.code[0],ix );
      ix = emit_insn( &block.code[0],ix, 0 );
      assert(ix <= N_BOOTBLOCK_INSNS);

   } else {

      /* 32-bit sequence */
      ix = emit_insn( &block.code[0],ix,
                      gen_addis_rd_rs_N(1,1,-4) );
      ix = emit_li32( &block.code[0],ix, 2, __nr___loadx );
      ix = emit_li32( &block.code[0],ix, 3, VKI_DL_LOAD );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(4,1) );
      ix = emit_li32( &block.code[0],ix, 5, 3<<16 );
      ix = emit_insn( &block.code[0],ix, 
                      gen_addi_rd_rs_N(6,31,offsetof(AIX5Bootblock,toolfile)));
      ix = emit_li32( &block.code[0],ix, 7, 0);
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(8,7) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(9,7) );
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(10,7) );
      ix = emit_dosc( &block.code[0],ix );
      ix = emit_insn( &block.code[0],ix,
		      gen_addis_rd_rs_N(1,1,4) );
      ix = emit_insn( &block.code[0],ix, gen_cmpli_cr7_r_N(4,0) );
      Int ix_bne = ix; /* Patch this later */
      ix = emit_insn( &block.code[0],ix, 0 );
      assert(ix <= N_BOOTBLOCK_INSNS);

      /* Looks like we're good.  r3 now points at a standard function
         descriptor for the entry point of the module we just loaded.
         Load r2/r11 from the descriptor, then put the address of the
         bootstrap area in r3, and jump to the code address.  Not a
         call -- we don't intend to return here.  Note, must use r30
         as scratch here since r31 is live.
            lwz  30, 0(3)
            mtlr 30
            lwz  2, 4(3)
            lwz  11, 8(3)
            mr   3, 31
            blr
      */
      ix = emit_insn( &block.code[0],ix, gen_lwz_rd_off_ra(30, 0, 3));
      ix = emit_insn( &block.code[0],ix, gen_mtlr_r(30) );
      ix = emit_insn( &block.code[0],ix, gen_lwz_rd_off_ra( 2, 4, 3));
      ix = emit_insn( &block.code[0],ix, gen_lwz_rd_off_ra(11, 8, 3));
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(3,31));
      ix = emit_insn( &block.code[0],ix, gen_blr() );
      assert(ix <= N_BOOTBLOCK_INSNS);

      /* error:
         We get here if the _loadx syscall fails.  Write a terse message
         to stderr saying so, then exit, carrying the error code of the
         _loadx call.  The latter is saved in r30 across the write() call.
            mr   30,4 (4 contains the error result from __loadx)
            imm  2, __NR_write
            imm  3,2 (2=stderr)
            addi 4, 31, offset_of_errormsg
            imm  5, length(errormsg)
            SYSCALL_SEQUENCE
            imm  2, __NR_exit
            mr   3, 30
            SYSCALL_SEQUENCE
   
         Well, we shouldn't be alive here.  But just in case we do, put
         a zero word, which will generate SIGILL and definitely stop the
         party.
            .word 0
      */
      /* fill in the conditional jump */
      (void)emit_insn( &block.code[0],ix_bne, 
                                      gen_bne_cr7_delta(4*(ix-ix_bne)));
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(30,4) );
      ix = emit_li32( &block.code[0],ix, 2, __nr_kwrite);
      ix = emit_li32( &block.code[0],ix, 3, 2);
      ix = emit_insn( &block.code[0],ix, 
                      gen_addi_rd_rs_N(4,31,offsetof(AIX5Bootblock,errmsg)));
      ix = emit_li32( &block.code[0],ix, 5, strlen(bootstrap_errmsg));
      ix = emit_dosc( &block.code[0],ix );
      ix = emit_li32( &block.code[0],ix, 2, __nr__exit);
      ix = emit_insn( &block.code[0],ix, gen_mr_rd_rs(3,30) );
      ix = emit_dosc( &block.code[0],ix );
      ix = emit_insn( &block.code[0],ix, 0 );
      assert(ix <= N_BOOTBLOCK_INSNS);

   }

   VG_(debugLog)(1, "launcher", 
                    "parent: .. %d instructions emitted\n", ix);

#  if 0
   for (i = 0; i < ix; i++) {
      if (0) printf("code[%d] = 0x%08x\n", i, block.code[i]);
      char buff[100];
      sprintf(buff, "echo 0x%x | ./ascii2u32", block.code[i]);
      system(buff);
   }
#  endif

   /* ------ STEP 3: Find out where to place stuff in the child. ------ */

   /* We'll have to hijack some space in the data section of the main
      executable.  First off, find the first and last pages of said
      data section.  We can't use the text section, because the child
      is unable to write to its own text section, to undo the
      compression of the hijacked page.  We can't use the stack
      because it appears, although stacks in AIX 5.3 appear to be
      executable, the child gets SIGKILL'd after the ptrace detach if
      its program counter is pointing into its stack.  The data
      section of the main executable appears to be executable, though,
      so use that.

      This requires wading though the list of loaded modules in the
      child, to find the main executable. */

   long lr;
   if (child->is64) {
      lr = ptrace64(PT_LDINFO, (ULong)child->pid,
                               (ULong)(UWord)&ld_info64_array, 
                               sizeof(ld_info64_array), 0/*ignored*/);
   } else {
      lr = ptrace64(PT_LDINFO, (ULong)child->pid,
                               (ULong)(UWord)&ld_info32_array, 
                               sizeof(ld_info32_array), 0/*ignored*/);
   }
   VG_(debugLog)(1, "launcher", "parent: ptrace PT_LDINFO got %ld\n", lr);
   if (lr == -1)
      return "ptrace(PT_LDINFO, ...) failed";
   else 
      assert(lr == 0);

   /* We have to iterate through the entire array to close the object
      files that this has opened.  Duh. */
   if (child->is64) {
      char* p = (char*)&ld_info64_array;
      while (1) {
         struct __ld_info64* info = (struct __ld_info64*)p;
      
         VG_(debugLog)(1, 
            "launcher", "parent: text 0x%llx-0x%llx data 0x%llx-0x%llx\n",
            (Addr64)info->ldinfo_textorg, 
            (Addr64)info->ldinfo_textorg + (Addr64)info->ldinfo_textsize,
            (Addr64)info->ldinfo_dataorg, 
            (Addr64)info->ldinfo_dataorg + (Addr64)info->ldinfo_datasize
         );

         Int ir = close(info->_file._ldinfo_fd);
         assert(ir == 0);
         /* The last entry in the array is marked by having a zero
            offset-link field. */
         if (info->ldinfo_next == 0)
            break;
         p += info->ldinfo_next;
      }
   } else {
      char* p = (char*)&ld_info32_array;
      while (1) {
         struct __ld_info32* info = (struct __ld_info32*)p;
      
         VG_(debugLog)(1, 
            "launcher", "parent: text 0x%llx-0x%llx data 0x%llx-0x%llx\n",
            (Addr64)(UWord)info->ldinfo_textorg, 
            (Addr64)(UWord)info->ldinfo_textorg + info->ldinfo_textsize,
            (Addr64)(UWord)info->ldinfo_dataorg, 
            (Addr64)(UWord)info->ldinfo_dataorg + info->ldinfo_datasize
         );

         Int ir = close(info->_file._ldinfo_fd);
         assert(ir == 0);
         /* The last entry in the array is marked by having a zero
            offset-link field. */
         if (info->ldinfo_next == 0)
            break;
         p += info->ldinfo_next;
      }
   }

   /* The first entry in that array -- and it is guaranteed to to have
      at least one entry -- is that of the the main executable.  We
      need to put our bootblock in one of the pages the main
      executable's data segment.  The abovementioned AIX 'ptrace'
      documentation says:

        To allow a debugger to generate code more easily (in order to
        handle fast trap instructions, for example), memory from the
        end of the main program up to the next segment boundary can be
        modified. That memory is read-only to the process but can be
        modified by the debugger.

      which would be great if it actually worked reliably; but not so.
      On AIX 5.2 this is true, but on 5.3 it appears to be impossible
      to read or write (via ptrace) anything beyond the last page of
      the executable's text section.
   */
   Addr64 c_cand_text_first, c_cand_text_last;

   if (child->is64) {
      c_cand_text_first
         = (Addr64)ld_info64_array[0].ldinfo_dataorg;
      c_cand_text_last 
         = c_cand_text_first
           + ld_info64_array[0].ldinfo_datasize - 1;
   } else {
      c_cand_text_first
         = (Addr64)(UWord)ld_info32_array[0].ldinfo_dataorg;
      c_cand_text_last 
         = c_cand_text_first
           + ld_info32_array[0].ldinfo_datasize - 1;
   }

   VG_(debugLog)(1, "launcher", 
                    "parent: candidate first 0x%llx last 0x%llx\n",
                    c_cand_text_first, c_cand_text_last);

   /* Page align the text section limits. */
   Addr64 c_first_page = ROUNDDN_PAGE( c_cand_text_first );
   Addr64 c_last_page  = ROUNDDN_PAGE( c_cand_text_last );

   /* It's safe to try out any page p satisfying
         c_first_page <= p && p <= c_last_page
   */

   /* CHOOSE A PAGE.  Do a test compression of available pages until
      we find one for which compression yields enough free space to
      put the bootblock in. */
   Int    zsize;
   Addr64 c_chosen_page = 0;
   Addr64 c_page;
   UChar  p_page_unzbuf[PAGE_SIZE];
   UChar  p_page_unzbuf2[PAGE_SIZE];
   UChar  p_page_zbuf[PAGE_SIZE + 384 + 8/*paranoia*/];

   for (c_page = c_first_page; c_page <= c_last_page; c_page += PAGE_SIZE) {
      assert(IS_PAGE_ALIGNED(c_page));
      err = ptrace_read_page( child, p_page_unzbuf, c_page );
      if (err)
         return "read of page from child failed(1)";
      zsize = Huffman_Compress(p_page_unzbuf, p_page_zbuf, PAGE_SIZE);
      assert(zsize >= 0 && zsize <= PAGE_SIZE + 384);

      /* Do a test decompression, to check the compress/decompress
         cycle works properly */
      Huffman_Uncompress( p_page_zbuf, p_page_unzbuf2, 
                          PAGE_SIZE + 384, PAGE_SIZE);
      assert(0 == memcmp(p_page_unzbuf, p_page_unzbuf2, PAGE_SIZE));

      VG_(debugLog)(1, "launcher", 
                       "parent: page 0x%llx has %d usable bytes\n", 
                       c_page, PAGE_SIZE - zsize);

      if ( (Int)(PAGE_SIZE - zsize) 
           >= (Int)sizeof(AIX5Bootblock)+8/*paranoia*/) {
         c_chosen_page = c_page;
         break;
      }
   }

   if (c_chosen_page == NULL)
      return "can't find a page with enough free space for bootblock";

   /* Compress the chosen page, leaving the compressed data at the
      start of the page, and put the bootblock at the end of the
      page. */

   VG_(debugLog)(1, "launcher", 
                    "parent: reading page at 0x%llx\n", c_chosen_page);

   err = ptrace_read_page( child, p_page_unzbuf, c_chosen_page );
   if (err)
      return "read of page from child failed(2)";

   block.adler32 = compute_adler32( p_page_unzbuf, PAGE_SIZE );
   VG_(debugLog)(1, "launcher", 
                    "parent: adler32 of unz page is 0x%x\n", block.adler32);

   memset(p_page_zbuf, 0, sizeof(p_page_zbuf));
   zsize = Huffman_Compress(p_page_unzbuf, p_page_zbuf, PAGE_SIZE);
   assert(zsize >= 0 && zsize <= PAGE_SIZE + 384);

   assert(PAGE_SIZE - zsize >= sizeof(AIX5Bootblock)+8/*paranoia*/);

   UChar* p_dst = p_page_zbuf   + PAGE_SIZE - sizeof(AIX5Bootblock);
   Addr64 c_dst = c_chosen_page + PAGE_SIZE - sizeof(AIX5Bootblock);
   assert(IS_8_ALIGNED(c_dst));

   VG_(debugLog)(1, "launcher", 
                    "parent: free space starts at 0x%llx in child\n",
                    c_chosen_page + zsize);
   VG_(debugLog)(1, "launcher", 
                    "parent: bootblock will be at 0x%llx in child\n",
                    c_dst);

   *(AIX5Bootblock*)p_dst = block;

   VG_(debugLog)(1, "launcher", 
                    "parent: writing page at 0x%llx\n", c_chosen_page);

   err = ptrace_write_page( child, c_chosen_page, p_page_zbuf );
   if (err)
      return "write of page to child failed";

   /* Do a test read back to ensure ptrace didn't screw up. */

   err = ptrace_read_page( child, p_page_unzbuf2, c_chosen_page );
   if (err)
      return "test read back of boot page failed (1)";
   if (0 != memcmp(p_page_zbuf, p_page_unzbuf2, PAGE_SIZE))
      return "test read back of boot page failed (2)";

   /* Finally .. set the program counter so that when we detach, our
      magic stub is run, not the original program. */

   VG_(debugLog)(1, "launcher", 
                    "parent: set child's pc to 0x%llx\n",
                    c_dst + offsetof(AIX5Bootblock,code) );
   err = ptrace_put_pc ( child, c_dst + offsetof(AIX5Bootblock,code) );
   if (err)
      return "write of new initial pc into child failed";

   VG_(debugLog)(1, "launcher", 
                    "parent: set child's r31 to 0x%llx\n", c_dst);
   err = ptrace_put_r31 ( child, c_dst );
   if (err)
      return "write of new r31 into child failed";

   return NULL; /* success */
}


/* -------------------------------------------------------------- */
/* ---                                                        --- */
/* --- END write bootstrap loader into child process          --- */
/* ---                                                        --- */
/* -------------------------------------------------------------- */

static void barf ( int exitcode, char* argv0, char* msg )
{
   fprintf(stderr, "%s: %s\n", argv0, msg);
   exit(exitcode);
}

int main ( int argc, char** argv, char** envp )
{
   Child child;
   Int i, loglevel;
   const char *toolname = NULL;
         char *clientname = NULL;

   /* First, look in our own /proc/<pid>/sysent file to find
      the syscall numbers for kwrite and _getpid.  These are needed
      to make the VG_(debugLog) usable.  We'll temporarily use
      the sysent_buf used by write_bootstrap_loader_into_child for this
      purpose. */

   char        sysent_name[50];
   FILE*       sysent_file;
   int         sysent_used = 0;
   prsysent_t* sysent_hdr;

   child.pid  = 0;
   child.is64 = False;

   sprintf(sysent_name, "/proc/%d/sysent", getpid());
   sysent_file = fopen(sysent_name, "r");
   if (sysent_file == NULL)
      barf(1, argv[0], "Can't open my own /proc/<pid>/sysent file");

   sysent_used = fread(sysent_buf, 1, LAUNCHER_SYSENT_SIZE, sysent_file);
   if (sysent_used == 0)
      barf(1, argv[0], "Error reading my own /proc/<pid>/sysent file");
   if (sysent_used == LAUNCHER_SYSENT_SIZE)
      barf(1, argv[0], "LAUNCHER_SYSENT_SIZE is too low; increase and recompile");
   assert(sysent_used > 0 && sysent_used < LAUNCHER_SYSENT_SIZE);

   fclose(sysent_file);

   sysent_hdr = (prsysent_t*)&sysent_buf[0];

   /* Find some syscall numbers for the child.  Note, we copy them
      from our own /proc/../sysent file, which isn't really right. */
   Word __nr__getpid = -1;
   Word __nr_kwrite  = -1;
   for (i = 0; i < sysent_hdr->pr_nsyscalls; i++) {
      char* name = &sysent_buf[ sysent_hdr->pr_syscall[i].pr_nameoff ];
      int   nmbr = sysent_hdr->pr_syscall[i].pr_number;
      if (0 == strcmp(name, "_getpid"))
         __nr__getpid = nmbr;
      if (0 == strcmp(name, "kwrite"))
          __nr_kwrite = nmbr;
   }
   if (__nr__getpid == -1 || __nr_kwrite == -1)
      barf(1, argv[0], "can't establish syscall #s needed for startup");

   /* "Tell" m_vkiscnums about them */
   __NR_getpid = __nr__getpid;
   __NR_write = __nr_kwrite;

   /* Right, now we're safe to start the debug logging system. */
   /* Start the debugging-log system ASAP.  First find out how many
      "-d"s were specified.  This is a pre-scan of the command line.
      At the same time, look for the tool name. */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      if (argv[i][0] != '-') {
         clientname = argv[i];
         break;
      }
      if (0 == strcmp(argv[i], "--")) {
         if (i+1 < argc)
            clientname = argv[i+1];
         break;
      }
      if (0 == strcmp(argv[i], "-d"))
         loglevel++;
      if (0 == strncmp(argv[i], "--tool=", 7))
         toolname = argv[i] + 7;
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 1");

   /* Make sure we know which tool we're using */
   if (toolname) {
      VG_(debugLog)(1, "launcher", "tool '%s' requested\n", toolname);
   } else {
      VG_(debugLog)(1, "launcher",
                       "no tool requested, defaulting to 'memcheck'\n");
      toolname = "memcheck";
   }

   /* Do some preliminary sanity checks */
   long pagesize = sysconf(_SC_PAGESIZE);
   if (pagesize != 4096)
      barf(1, argv[0], "config error: sysconf(_SC_PAGESIZE) is not 4096");

   assert(PAGE_SIZE == 4096); /* stay sane */

   const char* valgrind_lib = VG_LIBDIR;

   /* If there is no program to run, which will be the case if the
      user just does "valgrind --help", etc, run a dummy do-nothing
      program so at least the tool can get started and handle the
      --help/--version etc.  It spots the fact that this is a dummy
      program and acts like it was started with no program, hence
      behaving the same as the Linux ports would have. */
   if (clientname == NULL) {
      Int j;
      char** new_argv;
      const char* noop_exe_name = "no_op_client_for_valgrind";
      const char* up_n_bindir = "/../../bin";
      clientname = malloc(strlen(valgrind_lib) + strlen(up_n_bindir)
                          + 2 + strlen(noop_exe_name));
      if (clientname == NULL) {
         fprintf(stderr,"%s: malloc of clientname failed\n", argv[0]);
         return 1;
      }
      sprintf(clientname, "%s%s/%s", valgrind_lib, up_n_bindir, noop_exe_name);
      /* now we have to add it to the end of argv, which means making
	 that one word longer.  How tedious. */
      for (j = 0; argv[j]; j++)
	;
      j += 2; 
      new_argv = calloc(j, sizeof(char*));
      if (new_argv == NULL) {
         fprintf(stderr,"%s: malloc of new_argv failed\n", argv[0]);
         return 1;
      }
      for (i = 0; i < j-2; i++)
	new_argv[i] = argv[i];
      new_argv[j-2] = clientname;
      assert(new_argv[j-1] == NULL);
      argv = new_argv;
      argc++;
   }

   if (argc < 2 || toolname == NULL || clientname == NULL)
      barf(1, argv[0], "usage: valgrind [args-for-valgrind] prog args"); 

   /* Find the client, and figure out if it's a 32- or 64-bit
      executable. */
   VG_(debugLog)(1, "launcher", "searching for client in $PATH\n");
   if (strchr(clientname, '/') == NULL)
      clientname = (char*)find_client(clientname);
   VG_(debugLog)(1, "launcher", "found %s\n", clientname);

   Int client_exekind = examine_client ( clientname );
   switch (client_exekind) {
      case 32: 
         child.is64 = False; 
         break;
      case 64: 
         child.is64 = True; 
         break;
      default: 
         fprintf(stderr, "%s: requested executable %s\n", 
                         argv[0], clientname);
         fprintf(stderr, "%s: not found, or is not a valid XCOFF32 "
                         "or XCOFF64 executable.\n", argv[0]);
         return 1;
   }

   VG_(debugLog)(1, "launcher", "client is an XCOFF%d executable\n", 
                    client_exekind);

   const char* platform = child.is64 ? "ppc64-aix5" : "ppc32-aix5";

   VG_(debugLog)(1, "launcher", "looking for the tool file\n");

   char* toolfile = malloc(strlen(valgrind_lib) 
                    + strlen(toolname) + strlen(platform) + 3);
   if (toolfile == NULL) {
      fprintf(stderr,"%s: malloc of toolfile failed\n", argv[0]);
      return 1;
   }
   sprintf(toolfile, "%s/%s-%s", valgrind_lib, toolname, platform);

   if (!file_exists(toolfile)) {
      fprintf(stderr,"%s: can't stat %s\n", argv[0], toolfile);
      return 1;
   }

   /* Force the client to use a 1:1 threading model - this works
      because the client inherits our environment. */
   VG_(debugLog)(1, "launcher", "doing putenv(\"AIXTHREAD_SCOPE=S\")\n");
   Int putenv_err = putenv("AIXTHREAD_SCOPE=S");
   if (putenv_err) {
      fprintf(stderr,"%s: putenv(\"AIXTHREAD_SCOPE=S\") failed\n", argv[0]);
      return 1;
   }

   VG_(debugLog)(1, "launcher", "doing putenv(\"MP_SHARED_MEMORY=no\")\n");
   putenv_err = putenv("MP_SHARED_MEMORY=no");
   if (putenv_err) {
      fprintf(stderr,"%s: putenv(\"MP_SHARED_MEMORY=no\") failed\n", argv[0]);
      return 1;
   }

   /* Find out what the current working directory is, and stuff it into the
      environment so that the child can find it. */
   char wd_buf[4096];
   memset(wd_buf, 0, sizeof(wd_buf));
   if (getcwd(wd_buf, sizeof(wd_buf)-1) == NULL) {
      fprintf(stderr,"%s: getcwd(..) failed\n", argv[0]);
      return 1;
   }
   assert(wd_buf[ sizeof(wd_buf)-1 ] == 0);
   char* set_cwd = calloc(1, 100+sizeof(wd_buf));   
   if (set_cwd == NULL) {
      fprintf(stderr,"%s: calloc of set_cwd failed\n", argv[0]);
      return 1;
   }
   sprintf(set_cwd, "VALGRIND_STARTUP_PWD_%d_XYZZY=%s", getpid(), wd_buf);
   VG_(debugLog)(1, "launcher", "doing putenv(\"%s\")\n", set_cwd);
   putenv_err = putenv(set_cwd);
   if (putenv_err) {
      fprintf(stderr,"%s: putenv(\"VALGRIND_STARTUP_PWD_...\") failed\n", 
                     argv[0]);
      return 1;
   }

   /* Also, cook up the fully qualified name of this executable.  The
      following is a kludge, but I don't see how to really get the
      fully qualified name on AIX. */
   char* up_n_down = "/../../bin/valgrind";
   char* launcher = malloc(strlen(valgrind_lib)
                           + strlen(up_n_down) + 2);
   if (launcher == NULL) {
      fprintf(stderr,"%s: malloc of launcher failed\n", argv[0]);
      return 1;
   }
   sprintf(launcher, "%s%s", valgrind_lib, up_n_down);

   if (!file_exists(launcher)) {
      fprintf(stderr,"%s: can't stat %s\n", argv[0], launcher);
      return 1;
   }

   /* First, fork.  

      In the child, ask for a ptrace, then exec argv[2 ..].  This
      causes the kernel to complete the exec, hence loading the
      child, but does not start it; instead the child remains frozen
      so that the parent can mess with it via ptrace().
   */
   VG_(debugLog)(1, "launcher", "doing fork()\n");
   child.pid = fork();
   if (child.pid == -1) {
      fprintf(stderr,"%s: fork() failed\n", argv[0]);
      return 1;
   }

   if (child.pid == 0) {
      /* --- CHILD --- */
      VG_(debugLog)(1, "launcher", "child: before ptrace\n");
      long rl = ptrace64(PT_TRACE_ME, 0,0,0,0);
      if (rl != 0) {
         fprintf(stderr,"%s: child: ptrace(PT_TRACE_ME, ...) failed\n", argv[0]);
         fprintf(stderr,"%s: ", argv[0]);
         perror(NULL);
         fflush(stderr);
         _exit(1);
      }
      VG_(debugLog)(1, "launcher", "child: before execve\n");

      /* make VALGRIND_LAUNCHER point at something plausible. */
      VG_(debugLog)(1, "launcher", "child: launcher = %s\n", launcher);
      int r = setenv("VALGRIND_LAUNCHER", launcher, 1/*overwrite*/);
      if (r) {
         /* setenv failed. */
         fprintf(stderr,"%s: child: setenv failed\n", argv[0]);
         fprintf(stderr,"%s: ", argv[0]);
         perror(NULL);
         fflush(stderr);
         _exit(1);
         /* NOTREACHED */
      }

      /* This is kind-of strange.  We're execvp-ing the client but
         argv[0] is the toolname, which is irrelevant - m_main ignores
         it.  However, setting it like this at least makes m_main's
         view of the world (as far as the argv goes) look the same as
         it does in Linux-land: 
            tool-exe-name [args for V] client-name [args for client]
      */
      argv[0] = toolfile;
      int ri = execvp(clientname, &argv[0]);
      /* WE ONLY GET HERE IF execve FAILED */
      assert(ri == -1);
      fprintf(stderr,"%s: exec failed: %s: ", argv[0], clientname);
      perror("");
      return 1;
      /* NOTREACHED */
   }

   /* --- PARENT --- */
   VG_(debugLog)(1, "launcher", "parent: waitpid-ing for child\n");
   int status;
   /* Wait to hear back from the child. */
   pid_t p2 = waitpid(child.pid, &status, 0);
   /* We could hear back for two reasons.  (1) the exec was
      successful, and because the child is being ptraced, it is now
      waiting for the parent.  (2) the exec failed, and so the child
      did _exit(). */
   VG_(debugLog)(1, "launcher", "parent: waitpid got pid %d\n", (int)p2);
   VG_(debugLog)(1, "launcher", "parent: waitpid got status 0x%x\n", status);
   assert(p2 == child.pid); /* Huh?! We only have one child. */

   if (WIFEXITED(status)) {
      /* Case (2) - exec failed. */
      fprintf(stderr, "parent: child's exec failed.\n");
      return 0;
   }

   /* else case (1) must apply */
   assert(WIFSTOPPED(status));

   /* ------ BEGIN write bootstrap pages into child ------ */

   /* In this section, if for any reason we can't continue to the
      child-detach and so have to give up, we have to kill the child,
      else it'll become a zombie.  That's what the code at
      latched_error: does. */
   char* badness 
            = write_bootstrap_loader_into_child ( &child, toolfile );
   /* Returns NULL if no error, else points to a string of at least
      some descriptiveness. */
   if (badness)
      goto latched_error;

   /* ------ END write bootstrap pages into child ------ */

   VG_(debugLog)(1, "launcher", "parent: detaching child\n");
   long lr = ptrace64(PT_DETACH, (ULong)child.pid, 0, SIGCONT, 0);
   VG_(debugLog)(1, "launcher", "parent: detach got %ld\n", lr);
   assert(lr == 0);
   VG_(debugLog)(1, "launcher", "parent: waiting for child to finish\n");

   p2 = waitpid(child.pid, &status, 0);
   assert(p2 == child.pid);
   if (0)
      fprintf(stderr,"parent: child finished, status 0x%x 0x%x\n", 
                     status, WEXITSTATUS(status));

   if (WIFEXITED(status)) {
      VG_(debugLog)(1, "launcher", 
                       "parent: child finished normally, exit code %d\n",
                       WEXITSTATUS(status));
      return WEXITSTATUS(status);
   } 
   else if (WIFSIGNALED(status)) {
      VG_(debugLog)(1, "launcher",
                       "parent: child exited on signal %d\n",
                       (int)WTERMSIG(status));
      /* Since the child exited with a signal, we'd better
         whack ourselves on the head with the same signal. */
      kill( getpid(), (int)WTERMSIG(status) );
      /* presumably NOTREACHED? */
      return 0; /* This is completely bogus */
   } 
   else {
      /* erm.  Can we ever get here? */
      assert(0);
      return 0;
   }

  latched_error:
   /* We get here if there was some kind of problem messing with the
      child whilst we still had it latched by ptrace.  In this case we
      need to kill it before exiting, since otherwise it will become a
      zombie. */
   assert(badness);
   fprintf(stderr, "%s: error while doing ptracery on '%s'\n",
                   argv[0], clientname);
   fprintf(stderr, "%s: error is: %s\n",
                   argv[0], badness);
   return 0; /*BOGUS*/
}

/*--------------------------------------------------------------------*/
/*--- end                                          launcher-aix5.c ---*/
/*--------------------------------------------------------------------*/
