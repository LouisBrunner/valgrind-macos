
/*--------------------------------------------------------------------*/
/*--- Create initial process image on for the client               ---*/
/*---                                           pub_core_initimg.h ---*/
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
*/

#ifndef __PUB_CORE_INITIMG_H
#define __PUB_CORE_INITIMG_H


//--------------------------------------------------------------------
// PURPOSE: Map the client executable into memory, then set up its
// stack, environment and data section, ready for execution.  Quite a
// lot of work on Linux (ELF) but nearly a no-op on AIX (XCOFF) since
// the AIX kernel does most of the work for us.
//--------------------------------------------------------------------

/* These are OS-specific and defined below. */
typedef  struct _IICreateImageInfo    IICreateImageInfo;
typedef  struct _IIFinaliseImageInfo  IIFinaliseImageInfo;

/* This is a two stage process.  The first stage, which is most of the
   work, creates the initial image in memory to the extent possible.
   To do this it takes a bundle of information in an IICreateImageInfo
   structure, which is gathered in an OS-specific way at startup.
   This returns an IIFinaliseImageInfo structure: */
extern 
IIFinaliseImageInfo VG_(ii_create_image)( IICreateImageInfo );

/* Just before starting the client, we may need to make final
   adjustments to its initial image.  Also we need to set up the VEX
   guest state for thread 1 (the root thread) and copy in essential
   starting values.  This is handed the IIFinaliseImageInfo created by
   VG_(ii_create_image). */
extern 
void VG_(ii_finalise_image)( IIFinaliseImageInfo );

/* Note that both IICreateImageInfo and IIFinaliseImageInfo are
   OS-specific.  We now go on to give instantiations of them
   for supported OSes. */

/* ------------------------- Linux ------------------------- */

#if defined(VGO_linux)

struct _IICreateImageInfo {
   /* ------ Mandatory fields ------ */
   HChar*  toolname;
   Addr    sp_at_startup;
   Addr    clstack_top;
   /* ------ Per-OS fields ------ */
   HChar** argv;
   HChar** envp;
};

struct _IIFinaliseImageInfo {
   /* ------ Mandatory fields ------ */
   SizeT clstack_max_size;
   Addr  initial_client_SP;
   /* ------ Per-OS fields ------ */
   Addr  initial_client_IP;
   Addr  initial_client_TOC;
   UInt* client_auxv;
};


/* ------------------------- AIX5 ------------------------- */

#elif defined(VGO_aix5)

/* First we need to define this auxiliary structure. */
typedef
   struct {
      /* NOTE: VG_(ppc32/64_aix5_do_preloads_then_start_client) has
         these offsets hardwired in.  Do not change them without
         changing it too. */
      /* system call numbers */
      /*   0 */ UInt nr_load; /* is __NR___loadx for 32-bit, 
                                    __NR_kload for 64 */
      /*   4 */ UInt nr_kwrite;
      /*   8 */ UInt nr__exit;
      /* offset/length of error message, if the preloads fail */
      /*  12 */ UInt off_errmsg;
      /*  16 */ UInt len_errmsg;
      /* offsets from start of this struct to the the preload file
         names */
      /*  20 */ UInt off_preloadcorename;
      /*  24 */ UInt off_preloadtoolname;
      /*  28 */ UInt off_ld_preloadname;
      /* Once the preloading is done, we'll need to restore the guest
         state to what it needs to be at client startup.  Here's the
         relevant info.  Are ULongs; for 32-bit the data is at the
         lsb (high addressed) end. */
      /*  32 */ ULong client_start;
      /*  40 */ ULong r2;
      /*  48 */ ULong r3;
      /*  56 */ ULong r4;
      /*  64 */ ULong r5;
      /*  72 */ ULong r6;
      /*  80 */ ULong r7;
      /*  88 */ ULong r8;
      /*  96 */ ULong r9;
      /* 104 */ ULong r10;
      /* If the loading fails, we'll want to call a diagnostic
         function in C to figure out what happened.  Here's it's
         function descriptor.  Note, this runs on the simd cpu
         (a kludge, and will segfault in 64-bit mode). */
      /* 112 */ void* p_diagnose_load_failure;
   }
   AIX5PreloadPage;

struct _IICreateImageInfo {
   /* ------ Mandatory fields ------ */
   HChar* toolname; 
   Addr   sp_at_startup; /* Not used on AIX. */
   Addr   clstack_top;   /* Not used on AIX. */
   /* ------ Per-OS fields ------ */
   /* Initial values for guest int registers (GPR0 .. GPR31, PC, CR,
      LR, CTR, XER).  Passed to us from the launcher. */
   ULong* intregs37;
   /* AIX5Bootblock*, really */
   void* bootblock;
   /* Adler32 checksum of uncompressed data of compressed page. */
   UInt adler32_exp;
};

struct _IIFinaliseImageInfo {
   /* ------ Mandatory fields ------ */
   SizeT clstack_max_size;
   /* Initial value for SP (which is merely a copy of r1's value,
      intregs37[1]). */
   Addr initial_client_SP;
   /* ------ Per-OS fields ------ */
   /* Pointer to the preload page.  The preload page and this pointer
      to it are set up by VG_(ii_create_image). */
   AIX5PreloadPage* preloadpage;
   /* Initial values for guest int registers (GPR0 .. GPR31, PC,
      CR, LR, CTR, XER).  Copied from the CII. */
   ULong* intregs37;
   /* Address of the page compressed by the launcher. */
   Addr compressed_page;
   /* Adler32 checksum of uncompressed data of said page. */
   UInt adler32_exp;
};


/* ------------------------- Darwin ------------------------- */

#elif defined(VGO_darwin)

struct _IICreateImageInfo {
   /* ------ Mandatory fields ------ */
   HChar*  toolname;
   Addr    sp_at_startup;
   Addr    clstack_top;
   /* ------ Per-OS fields ------ */
   HChar** argv;
   HChar** envp;
   Addr    entry;            /* &_start */
   Addr    init_ip;          /* &__dyld_start, or copy of entry */
   Addr    stack_start;      /* stack segment hot */
   Addr    stack_end;        /* stack segment cold */
   Addr    text;             /* executable's Mach header */
   Bool    dynamic;          /* False iff executable is static */
   HChar*  executable_path;  /* path passed to execve() */
};

struct _IIFinaliseImageInfo {
   /* ------ Mandatory fields ------ */
   SizeT clstack_max_size;
   Addr  initial_client_SP;
   /* ------ Per-OS fields ------ */
   Addr  initial_client_IP;
};


#else
#  error "Unknown OS"
#endif


#endif   // __PUB_CORE_INITIMG_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
