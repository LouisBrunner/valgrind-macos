
/*--------------------------------------------------------------------*/
/*--- Create initial process image on for the client               ---*/
/*---                                           pub_core_initimg.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2006 OpenWorks LLP
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

#if defined(VGO_linux)
typedef
   struct {
      Addr  initial_client_SP;
      Addr  initial_client_IP;
      Addr  initial_client_TOC;
      UInt* client_auxv;
   }
   ClientInitImgInfo;

#elif defined(VGO_aix5)
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
         (another nasty AIX kludge) */
      /* 112 */ void* p_diagnose_load_failure;
   }
   AIX5PreloadPage;

typedef
   struct {
      /* Pointer to the preload page.  This is set up by
         VG_(setup_client_initial_image). */
      AIX5PreloadPage* preloadpage;
      /* Initial values for guest int registers (GPR0 .. GPR31, PC,
         CR, LR, CTR, XER).  Passed to us from the launcher. */
      ULong* intregs37;
      /* Initial value for SP (which is merely a copy of r1's value,
         intregs37[1]). */
      Addr initial_client_SP;
      /* Address of the page compressed by the launcher. */
      Addr compressed_page;
      /* Adler32 checksum of uncompressed data of said page. */
      UInt adler32_exp;
   }
   ClientInitImgInfo;

#else
#  error Unknown OS
#endif

/* Load the client, create the initial image (stack, etc), and
   return a bundle of info in a ClientInitImgInfo. */
extern ClientInitImgInfo
          VG_(setup_client_initial_image)(
             /*IN*/ HChar** argv,
             /*IN*/ HChar** envp,
             /*IN*/ HChar*  toolname,
             /*IN*/ Addr    clstack_top,
             /*IN*/ SizeT   clstack_max_size
          );

/* Make final adjustments to the initial image.  Also, initialise the
   VEX guest state for thread 1 (the root thread) and copy in
   essential starting values.  Is handed the ClientInitImgInfo created
   by VG_(setup_client_initial_image).  Upon return, the client's
   memory and register state should be ready to start the JIT. */
extern 
void VG_(finalise_thread1state)( /*MOD*/ThreadArchState* arch,
                                 ClientInitImgInfo ciii );

#endif   // __PUB_CORE_INITIMG_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
