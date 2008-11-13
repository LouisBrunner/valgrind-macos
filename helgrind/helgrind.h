/*
   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one file
   (helgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2008 OpenWorks LLP
      info@open-works.co.uk

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. The origin of this software must not be misrepresented; you must 
      not claim that you wrote the original software.  If you use this 
      software in a product, an acknowledgment in the product 
      documentation would be appreciated but is not required.

   3. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.

   4. The name of the author may not be used to endorse or promote 
      products derived from this software without specific prior written 
      permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one file
   (helgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ---------------------------------------------------------------- 
*/

#ifndef __HELGRIND_H
#define __HELGRIND_H

#include "valgrind.h"

typedef
   enum {
      VG_USERREQ__HG_CLEAN_MEMORY = VG_USERREQ_TOOL_BASE('H','G'),

      /* The rest are for Helgrind's internal use.  Not for end-user
         use.  Do not use them unless you are a Valgrind developer. */

      /* Notify the tool what this thread's pthread_t is. */
      _VG_USERREQ__HG_SET_MY_PTHREAD_T = VG_USERREQ_TOOL_BASE('H','G') 
                                         + 256,
      _VG_USERREQ__HG_PTH_API_ERROR,              /* char*, int */
      _VG_USERREQ__HG_PTHREAD_JOIN_POST,          /* pthread_t of quitter */
      _VG_USERREQ__HG_PTHREAD_MUTEX_INIT_POST,    /* pth_mx_t*, long mbRec */
      _VG_USERREQ__HG_PTHREAD_MUTEX_DESTROY_PRE,  /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,   /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_POST,  /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE, /* pth_mx_t*, long isTryLock */
      _VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,    /* pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_SIGNAL_PRE,    /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_COND_BROADCAST_PRE, /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_COND_WAIT_PRE,     /* pth_cond_t*, pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_WAIT_POST,    /* pth_cond_t*, pth_mx_t* */
      _VG_USERREQ__HG_PTHREAD_COND_DESTROY_PRE,   /* pth_cond_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_INIT_POST,   /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_DESTROY_PRE, /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,    /* pth_rwlk_t*, long isW */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,   /* pth_rwlk_t*, long isW */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_PRE,  /* pth_rwlk_t* */
      _VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_POST, /* pth_rwlk_t* */
      _VG_USERREQ__HG_POSIX_SEM_INIT_POST,        /* sem_t*, ulong value */
      _VG_USERREQ__HG_POSIX_SEM_DESTROY_PRE,      /* sem_t* */
      _VG_USERREQ__HG_POSIX_SEM_POST_PRE,         /* sem_t* */
      _VG_USERREQ__HG_POSIX_SEM_WAIT_POST,        /* sem_t* */
      _VG_USERREQ__HG_PTHREAD_BARRIER_INIT_PRE,   /* pth_bar_t*, ulong */
      _VG_USERREQ__HG_PTHREAD_BARRIER_WAIT_PRE,   /* pth_bar_t* */
      _VG_USERREQ__HG_PTHREAD_BARRIER_DESTROY_PRE /* pth_bar_t* */
   } Vg_TCheckClientRequest;

/* Clean memory state.  This makes Helgrind forget everything it knew
   about the specified memory range, and resets it to New.  This is
   particularly useful for memory allocators that wish to recycle
   memory. */
#define VALGRIND_HG_CLEAN_MEMORY(_qzz_start, _qzz_len)                    \
   do {                                                                   \
     unsigned long _qzz_res;                                              \
     VALGRIND_DO_CLIENT_REQUEST(_qzz_res, 0, VG_USERREQ__HG_CLEAN_MEMORY, \
                                _qzz_start, _qzz_len, 0, 0, 0);	          \
     (void)0;                                                             \
   } while(0)

#endif /* __HELGRIND_H */
