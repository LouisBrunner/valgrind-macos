
/*--------------------------------------------------------------------*/
/*--- Startup: create initial process image on AIX5                ---*/
/*---                                               initimg-aix5.c ---*/
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

#if defined(VGO_aix5)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_mallocfree.h"
#include "pub_core_machine.h"
#include "pub_core_ume.h"
#include "pub_core_options.h"
#include "pub_core_threadstate.h"     /* ThreadArchState */
#include "pub_core_tooliface.h"       /* VG_TRACK */
#include "pub_core_trampoline.h" /* VG_(ppc32_aix5_do_preloads_then_start_client) */
#include "pub_core_syscall.h"         // VG_(do_syscall1)
#include "pub_core_initimg.h"         /* self */

#include "simple_huffman.c"

#if !defined(VGP_ppc32_aix5) && !defined(VGP_ppc64_aix5)
#error "This should only be compiled on AIX"
#endif


static void diagnose_load_failure ( void );

/* --- Create the client's initial memory image. --- */

IIFinaliseImageInfo VG_(ii_create_image)( IICreateImageInfo iicii )
{
   /* Set up an AIX5PreloadPage structure with the names of

         $VALGRIND_LIB/vgpreload_core_PLATFORM.so
         $VALGRIND_LIB/vgpreload_TOOL_PLATFORM.so, if it exists
         xxx in "LD_PRELOAD=xxx", if it exists

      The client is started by running (on the simulator, of course)
      VG_(ppc{32,64}_aix5_do_preloads_then_start_client), which uses
      __loadx/_kload to load these .so's.  When the preloading is
      done, various guest registers are restored to what they are
      really supposed to be at client startup, so these values too are
      stored in the AIX5PreloadPage.  Finally, we jump to the client's
      entry point address. 
   */
   const HChar* _so        = ".so";
   const HChar* vgpreload_ = "vgpreload_";
   const HChar* core       = "core";
   const HChar* errmsg_str 
                   = "valgrind: FATAL: core/tool/LD_PRELOAD= "
                     "preload failed.\n";
   Int    plcore_len,  pltool_len, ld_pre_len, errmsg_len;
   HChar *plcore_str, *pltool_str, *ld_pre_str;
   Bool   have_tool_so, have_ld_pre;

   AIX5PreloadPage* pp;
   UChar*           pc;
   Int              szB, szPG;
   SysRes           sres;

   IIFinaliseImageInfo iifii;
   VG_(memset)( &iifii, 0, sizeof(iifii) );

   /* this can happen, if m_main decides to NULL it out */
   if (VG_(args_the_exename) == NULL)
      VG_(err_missing_prog)();

   vg_assert( iicii.toolname );
   pltool_len = VG_(strlen)( VG_(libdir) ) 
                + 1 /*slash*/
                + VG_(strlen)( vgpreload_ )
                + VG_(strlen)( iicii.toolname )
                + 1 /*dash*/
                + VG_(strlen)(VG_PLATFORM)
                + VG_(strlen)( _so )
                + 1 /*NUL*/;
   vg_assert(pltool_len > 0);
   pltool_str = VG_(malloc)( "initimg-aix5.ici.1", pltool_len );
   pltool_str[0] = 0;
   VG_(strcat)( pltool_str, VG_(libdir) );
   VG_(strcat)( pltool_str, "/" );
   VG_(strcat)( pltool_str, vgpreload_ );
   VG_(strcat)( pltool_str, iicii.toolname );
   VG_(strcat)( pltool_str, "-" );
   VG_(strcat)( pltool_str, VG_PLATFORM );
   VG_(strcat)( pltool_str, _so );
   vg_assert( pltool_str[pltool_len-1] == 0);
   vg_assert( VG_(strlen)(pltool_str) == pltool_len-1 );

   plcore_len = VG_(strlen)( VG_(libdir) ) 
                + 1 /*slash*/
                + VG_(strlen)( vgpreload_ )
                + VG_(strlen)( core )
                + 1 /*dash*/
                + VG_(strlen)(VG_PLATFORM)
                + VG_(strlen)(_so)
                + 1 /*NUL*/;
   vg_assert(plcore_len > 0);
   plcore_str = VG_(malloc)( "initimg-aix5.ici.2", plcore_len );
   plcore_str[0] = 0;
   VG_(strcat)( plcore_str, VG_(libdir) );
   VG_(strcat)( plcore_str, "/" );
   VG_(strcat)( plcore_str, vgpreload_ );
   VG_(strcat)( plcore_str, core );
   VG_(strcat)( plcore_str, "-" );
   VG_(strcat)( plcore_str, VG_PLATFORM );
   VG_(strcat)( plcore_str, _so );
   vg_assert( plcore_str[plcore_len-1] == 0 );
   vg_assert( VG_(strlen)(plcore_str) == plcore_len-1 );

   errmsg_len = VG_(strlen)( errmsg_str )
                + 1 /*NUL*/;

   ld_pre_str = VG_(getenv)("LD_PRELOAD");
   if (ld_pre_str && VG_(strlen)(ld_pre_str) > 0) {
      have_ld_pre = True;
      ld_pre_len  = VG_(strlen)(ld_pre_str) + 1/*NUL*/;
      ld_pre_str = VG_(malloc)( "initimg-aix5.ici.3", ld_pre_len );
      ld_pre_str[0] = 0;
      VG_(strcat)( ld_pre_str, VG_(getenv)("LD_PRELOAD") );
      vg_assert( ld_pre_str[ld_pre_len-1] == 0);
      vg_assert( VG_(strlen)( ld_pre_str ) == ld_pre_len - 1 );
   } else {
      have_ld_pre = False;
      ld_pre_len  = 0;
      ld_pre_str  = NULL;
   }

   VG_(debugLog)(1, "initimg", "plcore_str = '%s'\n", plcore_str );
   VG_(debugLog)(1, "initimg", "pltool_str = '%s'\n", pltool_str );
   VG_(debugLog)(1, "initimg", "ld_pre_str = '%s'\n", ld_pre_str );

   if (0 != VG_(access)(plcore_str, True,False,True))
      VG_(err_config_error)("Can't find core preload "
                            "(vgpreload_core-<platform>.so)");

   have_tool_so = 0 == VG_(access)(pltool_str, True,False,True);

   /* Figure out how much space is needed for an AIX5PreloadInfo
      followed by the three preload strings. */

   vg_assert((sizeof(AIX5PreloadPage) % 4) == 0); /* paranoia */

   szB = sizeof(AIX5PreloadPage) + plcore_len 
                                 + (have_tool_so ? pltool_len : 0)
                                 + (have_ld_pre ? ld_pre_len : 0)
                                 + errmsg_len;
   szPG = VG_PGROUNDUP(szB+1) / VKI_PAGE_SIZE;
   VG_(debugLog)(2, "initimg", 
                    "preload page size: %d bytes, %d pages\n", szB, szPG);

   vg_assert(szB > 0);
   vg_assert(szB < szPG * VKI_PAGE_SIZE);

   /* We'll need szPG pages of anonymous, rw-, client space (needs w
      so we can write it here) */
   sres = VG_(am_mmap_anon_float_client)
             ( szPG * VKI_PAGE_SIZE, VKI_PROT_READ|VKI_PROT_WRITE);
   if (sres.isError)
      VG_(err_config_error)("Can't allocate client page(s) "
                            "for preload info");
   pp = (AIX5PreloadPage*)sres.res;

   VG_(debugLog)(2, "initimg", "preload page allocation succeeded at %p\n", pp);

   /* Zero out the initial structure. */
   VG_(memset)(pp, 0, sizeof(AIX5PreloadPage));

   pc = (UChar*)pp;
   pc += sizeof(AIX5PreloadPage);
   VG_(memcpy)(pc, plcore_str, plcore_len);
   pp->off_preloadcorename = pc - (UChar*)pp;
   pc += plcore_len;
   if (have_tool_so) {
      VG_(memcpy)(pc, pltool_str, pltool_len);
      pp->off_preloadtoolname = pc - (UChar*)pp;
      pc += pltool_len;
   }
   if (have_ld_pre) {
      VG_(memcpy)(pc, ld_pre_str, ld_pre_len);
      pp->off_ld_preloadname = pc - (UChar*)pp;
      pc += ld_pre_len;
   }
   VG_(memcpy)(pc, errmsg_str, errmsg_len);
   pp->off_errmsg = pc - (UChar*)pp;
   pp->len_errmsg = errmsg_len - 1; /* -1: skip terminating NUL */

   vg_assert(pc <= ((UChar*)pp) - 1 + szPG * VKI_PAGE_SIZE);

   VG_(free)(plcore_str);
   VG_(free)(pltool_str);

   /* Fill in all the other preload page fields that we can right
      now. */
#  if defined(VGP_ppc32_aix5)
   vg_assert(__NR_AIX5___loadx != __NR_AIX5_UNKNOWN);
   pp->nr_load = __NR_AIX5___loadx;
#  else /* defined(VGP_ppc64_aix5) */
   vg_assert(__NR_AIX5_kload != __NR_AIX5_UNKNOWN);
   pp->nr_load = __NR_AIX5_kload;
#  endif

   vg_assert(__NR_AIX5_kwrite  != __NR_AIX5_UNKNOWN);
   pp->nr_kwrite = __NR_AIX5_kwrite;   /* kwrite */

   vg_assert(__NR_AIX5__exit   != __NR_AIX5_UNKNOWN);
   pp->nr__exit = __NR_AIX5__exit;    /* _exit */

   pp->p_diagnose_load_failure = &diagnose_load_failure;

   iifii.preloadpage       = pp;
   iifii.intregs37         = iicii.intregs37;
   iifii.initial_client_SP = iicii.intregs37[1]; /* r1 */
   iifii.compressed_page   = VG_PGROUNDDN((Addr)iicii.bootblock);
   iifii.adler32_exp       = iicii.adler32_exp;
   iifii.clstack_max_size  = 0; /* we don't know yet */
   return iifii;
}


/* --- Finalise the initial image and register state. --- */

static UChar unz_page[VKI_PAGE_SIZE];

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

/* Just before starting the client, we may need to make final
   adjustments to its initial image.  Also we need to set up the VEX
   guest state for thread 1 (the root thread) and copy in essential
   starting values.  This is handed the IIFinaliseImageInfo created by
   VG_(ii_create_image).
*/
void VG_(ii_finalise_image)( IIFinaliseImageInfo iifii )
{
   UInt   adler32_act;
   SysRes sres;
   /* On AIX we get a block of 37 words telling us the initial state
      for (GPR0 .. GPR31, PC, CR, LR, CTR, XER), and we start with all
      the other registers zeroed. */

   ThreadArchState* arch = &VG_(threads)[1].arch;

#  if defined(VGP_ppc32_aix5)

   vg_assert(0 == sizeof(VexGuestPPC32State) % 16);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestPPC32_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestPPC32State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestPPC32State));

#  else /* defined(VGP_ppc64_aix5) */

   vg_assert(0 == sizeof(VexGuestPPC64State) % 16);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestPPC64_initialise(&arch->vex);

   /* Zero out the shadow areas. */
   VG_(memset)(&arch->vex_shadow1, 0, sizeof(VexGuestPPC64State));
   VG_(memset)(&arch->vex_shadow2, 0, sizeof(VexGuestPPC64State));

#  endif

   /* iifii.intregs37 contains the integer register state as it needs
      to be at client startup.  These values are supplied by the
      launcher.  The 37 regs are:initial values from launcher for:
      GPR0 .. GPR31, PC, CR, LR, CTR, XER. */

   /* Put essential stuff into the new state. */
   arch->vex.guest_GPR0  =  (UWord)iifii.intregs37[0];
   arch->vex.guest_GPR1  =  (UWord)iifii.intregs37[1];
   arch->vex.guest_GPR2  =  (UWord)iifii.intregs37[2];
   arch->vex.guest_GPR3  =  (UWord)iifii.intregs37[3];
   arch->vex.guest_GPR4  =  (UWord)iifii.intregs37[4];
   arch->vex.guest_GPR5  =  (UWord)iifii.intregs37[5];
   arch->vex.guest_GPR6  =  (UWord)iifii.intregs37[6];
   arch->vex.guest_GPR7  =  (UWord)iifii.intregs37[7];
   arch->vex.guest_GPR8  =  (UWord)iifii.intregs37[8];
   arch->vex.guest_GPR9  =  (UWord)iifii.intregs37[9];
   arch->vex.guest_GPR10 =  (UWord)iifii.intregs37[10];
   arch->vex.guest_GPR11 =  (UWord)iifii.intregs37[11];
   arch->vex.guest_GPR12 =  (UWord)iifii.intregs37[12];
   arch->vex.guest_GPR13 =  (UWord)iifii.intregs37[13];
   arch->vex.guest_GPR14 =  (UWord)iifii.intregs37[14];
   arch->vex.guest_GPR15 =  (UWord)iifii.intregs37[15];
   arch->vex.guest_GPR16 =  (UWord)iifii.intregs37[16];
   arch->vex.guest_GPR17 =  (UWord)iifii.intregs37[17];
   arch->vex.guest_GPR18 =  (UWord)iifii.intregs37[18];
   arch->vex.guest_GPR19 =  (UWord)iifii.intregs37[19];
   arch->vex.guest_GPR20 =  (UWord)iifii.intregs37[20];
   arch->vex.guest_GPR21 =  (UWord)iifii.intregs37[21];
   arch->vex.guest_GPR22 =  (UWord)iifii.intregs37[22];
   arch->vex.guest_GPR23 =  (UWord)iifii.intregs37[23];
   arch->vex.guest_GPR24 =  (UWord)iifii.intregs37[24];
   arch->vex.guest_GPR25 =  (UWord)iifii.intregs37[25];
   arch->vex.guest_GPR26 =  (UWord)iifii.intregs37[26];
   arch->vex.guest_GPR27 =  (UWord)iifii.intregs37[27];
   arch->vex.guest_GPR28 =  (UWord)iifii.intregs37[28];
   arch->vex.guest_GPR29 =  (UWord)iifii.intregs37[29];
   arch->vex.guest_GPR30 =  (UWord)iifii.intregs37[30];
   arch->vex.guest_GPR31 =  (UWord)iifii.intregs37[31];

   arch->vex.guest_CIA      = (UWord)iifii.intregs37[32+0];
   arch->vex.guest_LR       = (UWord)iifii.intregs37[32+2];
   arch->vex.guest_CTR      = (UWord)iifii.intregs37[32+3];

#  if defined(VGP_ppc32_aix5)

   LibVEX_GuestPPC32_put_CR(  (UWord)iifii.intregs37[32+1], &arch->vex );
   LibVEX_GuestPPC32_put_XER( (UWord)iifii.intregs37[32+4], &arch->vex );

   /* Set the cache line size (KLUDGE) */
   VG_(machine_ppc32_set_clszB)( 128 );

#  else /* defined(VGP_ppc64_aix5) */

   LibVEX_GuestPPC64_put_CR(  (UWord)iifii.intregs37[32+1], &arch->vex );
   LibVEX_GuestPPC64_put_XER( (UWord)iifii.intregs37[32+4], &arch->vex );

   /* Set the cache line size (KLUDGE) */
   VG_(machine_ppc64_set_clszB)( 128 );

#  endif

   /* Fix up the client's command line.  Its argc/v/envp is in r3/4/5
      (32-bit AIX) or r14/15/16 (64-bit AIX).  but that is for the
      Valgrind invokation as a whole.  Hence we need to decrement argc
      and advance argv to step over the args for Valgrind, and the
      name of the Valgrind tool exe bogusly inserted by the launcher
      (hence the "+1"). */

#  if defined(VGP_ppc32_aix5)

   { UWord n_vargs = VG_(sizeXA)( VG_(args_for_valgrind) );
     vg_assert(arch->vex.guest_GPR3 >= 1 + n_vargs);
     arch->vex.guest_GPR3 -= (1 + n_vargs);
     arch->vex.guest_GPR4 += sizeof(UWord) * (1 + n_vargs);
   }

#  else /* defined(VGP_ppc64_aix5) */

   { UWord n_vargs = VG_(sizeXA)( VG_(args_for_valgrind) );
     vg_assert(arch->vex.guest_GPR14 >= 1 + n_vargs);
     arch->vex.guest_GPR14 -= (1 + n_vargs);
     arch->vex.guest_GPR15 += sizeof(UWord) * (1 + n_vargs);
   }

#  endif

   /* At this point the guest register state is correct for client
      startup.  However, that's not where we want to start; in fact we
      want to start at VG_(ppc{32,64}_aix5_do_preloads_then_start_client),
      passing it iifii.preloadpage in r3.  This will load the core/tool
      preload .so's, then restore r2-r10 from what's stashed in the
      preloadpage, and then start the client really.  Hence: */

   /* Save r2-r10 and the client start point in preloadpage */
   iifii.preloadpage->r2  = (ULong)arch->vex.guest_GPR2;
   iifii.preloadpage->r3  = (ULong)arch->vex.guest_GPR3;
   iifii.preloadpage->r4  = (ULong)arch->vex.guest_GPR4;
   iifii.preloadpage->r5  = (ULong)arch->vex.guest_GPR5;
   iifii.preloadpage->r6  = (ULong)arch->vex.guest_GPR6;
   iifii.preloadpage->r7  = (ULong)arch->vex.guest_GPR7;
   iifii.preloadpage->r8  = (ULong)arch->vex.guest_GPR8;
   iifii.preloadpage->r9  = (ULong)arch->vex.guest_GPR9;
   iifii.preloadpage->r10 = (ULong)arch->vex.guest_GPR10;
   iifii.preloadpage->client_start = (ULong)arch->vex.guest_CIA;


#  if defined(VGP_ppc32_aix5)

   /* Set up to start at VG_(ppc32_aix5_do_preloads_then_start_client) */
   arch->vex.guest_CIA = (UWord)&VG_(ppc32_aix5_do_preloads_then_start_client);

#  else /* defined(VGP_ppc64_aix5) */

   /* Set up to start at VG_(ppc64_aix5_do_preloads_then_start_client) */
   arch->vex.guest_CIA = (UWord)&VG_(ppc64_aix5_do_preloads_then_start_client);

#  endif

   arch->vex.guest_GPR3 = (UWord)iifii.preloadpage;

   /* The rest of the preloadpage fields will already have been filled
      in by VG_(setup_client_initial_image).  So we're done. */

   /* Finally, decompress the page compressed by the launcher.  We
      can't do this any earlier, because the page is (effectively)
      decompressed in place, which trashes iifii.intregs37.  So we have
      to wait till this point, at which we're done with iifii.intregs37
      (to be precise, with what it points at). */
   VG_(debugLog)(1, "initimg", "decompressing page at %p\n", 
                    (void*)iifii.compressed_page);
   vg_assert(VG_IS_PAGE_ALIGNED(iifii.compressed_page));

   Huffman_Uncompress( (void*)iifii.compressed_page, unz_page,
                       VKI_PAGE_SIZE, VKI_PAGE_SIZE );
   adler32_act = compute_adler32(unz_page, VKI_PAGE_SIZE);

   VG_(debugLog)(1, "initimg", 
                    "decompress done, adler32s: act 0x%x, exp 0x%x\n",
                    adler32_act, iifii.adler32_exp );

   VG_(memcpy)((void*)iifii.compressed_page, unz_page, VKI_PAGE_SIZE);

   VG_(debugLog)(1, "initimg", "copy back done\n");

   /* Tell the tool that we just wrote to the registers. */
   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));

   /* Determine the brk limit. */
   VG_(debugLog)(1, "initimg", "establishing current brk ..\n");
   vg_assert(__NR_AIX5_sbrk != __NR_AIX5_UNKNOWN);
   sres = VG_(do_syscall1)(__NR_AIX5_sbrk, 0);
   vg_assert(sres.err == 0); /* assert no error */
   VG_(brk_base) = VG_(brk_limit) = sres.res;
   VG_(debugLog)(1, "initimg", ".. brk = %p\n", (void*)VG_(brk_base));
}


/* --- Diagnose preload failures. --- */

/* This is a nasty but effective kludge.  The address of the following
   function is put into the preload page.  So, if a preload failure
   happens, we call here to get helpful info printed out (the call
   site is in m_trampoline.S).  This is a dirty hack (1) because
   diagnose_load_failure runs on the simulated CPU, not the real one
   and (2) because it induces a libc dependency.  Oh well. */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <stdlib.h>
#include <sys/ldr.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

static void diagnose_load_failure ( void )
{
#  define NBUF 1024
   UChar buf[NBUF];
   VG_(debugLog)(0, "initimg", "Diagnosing load failure\n");
   if (sizeof(void*) == 8) {
      VG_(debugLog)(0, "initimg", "Can't safely do loadquery() "
                                  "in 64-bit mode.  Sorry.\n");
      /* because this requires dynamic linking to be working (IIRC)
         and it isn't; the tool file's dynamic linking was never done,
         because it was loaded by the bootstrap stub, which simply did
         sys_kload() but didn't make usla do the relevant
         relocations. */
   } else {
      UChar** p;
      Int r = loadquery(L_GETMESSAGES, buf, NBUF);
      VG_(debugLog)(0, "initimg", "loadquery returned %d (0 = success)\n", r);
      p = (UChar**)(&buf[0]);
      for (; *p; p++)
         VG_(debugLog)(0, "initimg", "\"%s\"\n", *p);
      VG_(debugLog)(0, "initimg", "Use /usr/sbin/execerror to make "
                                  "sense of above string(s)\n");
      VG_(debugLog)(0, "initimg", "See also comments at the bottom of\n");
      VG_(debugLog)(0, "initimg", "coregrind/m_initimg/"
                                  "initimg-aix5.c (in Valgrind sources)\n");
   }
#  undef NBUF
}

/* Take the strings that this prints out and feed them
   to /usr/sbin/execerror.  For example, it might print

     (ld 3 1 __libc_freeres /foo/bar/vgpreload_core-ppc32-aix5.so

   in which case 

     $ execerror xyzzy \
          "(ld 3 1 __libc_freeres /foo/bar/vgpreload_core-ppc32-aix5.so"

   gets you

     Could not load program xyzzy:
     rtld: 0712-001 Symbol __libc_freeres was referenced
     from module /foo/bar/vgpreload_core-ppc32-aix5.so(), 
        but a runtime definition
            of the symbol was not found.
*/

#endif // defined(VGO_aix5)

/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/
