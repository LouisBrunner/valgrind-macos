
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vki.                         ---*/
/*---                                                      m_vki.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2011 OpenWorks LLP
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

#  if defined(VGO_linux)
   /* nothing to check */
#  elif defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
   vg_assert(_VKI_NSIG == NSIG);
   vg_assert(_VKI_NSIG == 32);
   vg_assert(_VKI_NSIG_WORDS == 1);
   vg_assert(sizeof(sigset_t) /* defined by Darwin */ 
             == sizeof(vki_sigset_t) /* what we actually use */);
#  else
#    error "Unknown plat"
#  endif

   /* --- Platform-specific checks on sigactions --- */

#  if defined(VGO_linux)
   /* the toK- and fromK- forms are identical */
   vg_assert( sizeof(vki_sigaction_toK_t) 
              == sizeof(vki_sigaction_fromK_t) );
#  elif defined(VGO_darwin)
   /* the toK- and fromK- forms differ by one function-pointer field
      (sa_tramp) */
   vg_assert( sizeof(vki_sigaction_toK_t) 
              == sizeof(vki_sigaction_fromK_t) + sizeof(void*) );

   vg_assert(sizeof(struct sigaction) == sizeof(vki_sigaction_fromK_t));
   vg_assert(sizeof(struct __sigaction) == sizeof(vki_sigaction_toK_t));
   { struct __sigaction    t1;
     vki_sigaction_toK_t   t2;
     struct sigaction      f1;
     vki_sigaction_fromK_t f2;
     vg_assert(sizeof(t1.sa_handler) == sizeof(t2.ksa_handler));
     vg_assert(sizeof(t1.sa_tramp)   == sizeof(t2.sa_tramp));
     vg_assert(sizeof(t1.sa_mask)    == sizeof(t2.sa_mask));
     vg_assert(sizeof(t1.sa_flags)   == sizeof(t2.sa_flags));
     vg_assert(sizeof(f1.sa_handler) == sizeof(f2.ksa_handler));
     vg_assert(sizeof(f1.sa_mask)    == sizeof(f2.sa_mask));
     vg_assert(sizeof(f1.sa_flags)   == sizeof(f2.sa_flags));
#    if 0
     vg_assert(offsetof(t1,sa_handler) == offsetof(t2.ksa_handler));
     vg_assert(offsetof(t1.sa_tramp)   == offsetof(t2.sa_tramp));
     vg_assert(offsetof(t1.sa_mask)    == offsetof(t2.sa_mask));
     vg_assert(offsetof(t1.sa_flags)   == offsetof(t2.sa_flags));
     vg_assert(offsetof(f1.sa_handler) == offsetof(f2.ksa_handler));
     vg_assert(offsetof(f1.sa_mask)    == offsetof(f2.sa_mask));
     vg_assert(offsetof(f1.sa_flags)   == offsetof(f2.sa_flags));
#    endif
   }
   /* also .. */
   /* VKI_SET_SIGMASK is hardwired into syscall-x86-darwin.S and
      syscall-amd64-darwin.S */
   vg_assert(VKI_SIG_SETMASK == 3);

#  else
#     error "Unknown OS" 
#  endif
}


/*--------------------------------------------------------------------*/
/*--- end                                                  m_vki.c ---*/
/*--------------------------------------------------------------------*/
