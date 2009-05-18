
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vki.                         ---*/
/*---                                                      m_vki.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2009 OpenWorks LLP
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

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_vki.h"     /* self */

/* We have pub_{core,tool}_vki.h.  This is the matching implementation
   for that interface.  In fact there is no implementation, as the
   sole purpose of the module is to export types and constants
   describing the kernel interface, so this file is nearly empty. */


/* ppc32/64-linux determines page size at startup, hence m_vki is
   the logical place to store that info. */

#if defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
unsigned long VKI_PAGE_SHIFT = 12;
unsigned long VKI_PAGE_SIZE  = 1UL << 12;
#endif


/* Do initial consistency checks on some of the definitions to do with
   signals (vki_sigset_t and vki_sigaction_{toK,fromK}_t).  This stuff
   is fragile enough that it's important to check at startup that
   the world looks like what we expect it to look like. 

   The most important thing is to check that the definition of signal
   sets for this platform is right.  A signal set consists of some
   number _VKI_NSIG_WORDS of 32- or 64-bit words.  Because the kernel
   itself has some indexing scheme to set/clear individual bits in the
   set, we must make sure we use the same layout/scheme: where this
   requirement bites us is in the VG_(sigfillset) etc functions in
   m_libcsignal.c.  So we check carefully here that it's all sensible.
*/
void VG_(vki_do_initial_consistency_checks) ( void )
{
   /* --- Platform-independent checks on signal sets --- */

   vki_sigset_t set;
   // Set's size must agree with _VKI_NSIG
   vg_assert( 8 * sizeof(set) == _VKI_NSIG );
   // Set's word size must agree with _VKI_NSIG_BPW
   vg_assert( 8 * sizeof(set.sig[0]) == _VKI_NSIG_BPW );
   // The set elements are 32- or 64-bit
   vg_assert( _VKI_NSIG_BPW == 32 || _VKI_NSIG_BPW == 64 );

   /* --- Platform-specific checks on signal sets --- */

#  if defined(VGO_linux) || defined(VGO_aix5)
   /* nothing to check */
#  else
#    error "Unknown plat"
#  endif

   /* --- Platform-specific checks on sigactions --- */

#  if defined(VGO_linux) || defined(VGO_aix5)
   /* the toK- and fromK- forms are identical */
   vg_assert( sizeof(vki_sigaction_toK_t) 
              == sizeof(vki_sigaction_fromK_t) );

#  else
#     error "Unknown OS" 
#  endif
}


/*--------------------------------------------------------------------*/
/*--- end                                                  m_vki.c ---*/
/*--------------------------------------------------------------------*/
