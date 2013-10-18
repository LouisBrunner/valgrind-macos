
/*--------------------------------------------------------------------*/
/*--- Create initial process image on for the client               ---*/
/*---                                           pub_core_initimg.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2013 OpenWorks LLP
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

#include "pub_core_basics.h"      // Addr

//--------------------------------------------------------------------
// PURPOSE: Map the client executable into memory, then set up its
// stack, environment and data section, ready for execution.  Quite a
// lot of work on Linux (ELF).
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
   const HChar*  toolname;
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

/* ------------------------- Darwin ------------------------- */

#elif defined(VGO_darwin)

struct _IICreateImageInfo {
   /* ------ Mandatory fields ------ */
   const HChar*  toolname;
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
