
/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (vg_memcheck.h) only.  The entire rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2.  See
   the COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors.

   Copyright (C) 2000-2002 Julian Seward.  All rights reserved.

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
   (vg_memcheck.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ---------------------------------------------------------------- 
*/


#ifndef __MEMCHECK_H
#define __MEMCHECK_H


/* This file is for inclusion into client (your!) code.

   You can use these macros to manipulate and query memory permissions
   inside your own programs.

   See comment near the top of valgrind.h on how to use them.
*/

#include "valgrind.h"

typedef
   enum { 
      VG_USERREQ__MAKE_NOACCESS = VG_USERREQ_SKIN_BASE('M','C'),
      VG_USERREQ__MAKE_WRITABLE,
      VG_USERREQ__MAKE_READABLE,
      VG_USERREQ__DISCARD,
      VG_USERREQ__CHECK_WRITABLE,
      VG_USERREQ__CHECK_READABLE,
      VG_USERREQ__MAKE_NOACCESS_STACK,
      VG_USERREQ__DO_LEAK_CHECK, /* untested */
   } Vg_MemCheckClientRequest;



/* Client-code macros to manipulate the state of memory. */

/* Mark memory at _qzz_addr as unaddressible and undefined for
   _qzz_len bytes.  Returns an int handle pertaining to the block
   descriptions Valgrind will use in subsequent error messages. */
#define VALGRIND_MAKE_NOACCESS(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_NOACCESS,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   }) 
      
/* Similarly, mark memory at _qzz_addr as addressible but undefined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_WRITABLE(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_WRITABLE,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   })

/* Similarly, mark memory at _qzz_addr as addressible and defined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_READABLE(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_READABLE,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   })

/* Discard a block-description-handle obtained from the above three
   macros.  After this, Valgrind will no longer be able to relate
   addressing errors to the user-defined block associated with the
   handle.  The permissions settings associated with the handle remain
   in place.  Returns 1 for an invalid handle, 0 for a valid
   handle. */
#define VALGRIND_DISCARD(_qzz_blkindex)                          \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__DISCARD,                 \
                            0, _qzz_blkindex, 0, 0);             \
    _qzz_res;                                                    \
   })


/* Client-code macros to check the state of memory. */

/* Check that memory at _qzz_addr is addressible for _qzz_len bytes.
   If suitable addressibility is not established, Valgrind prints an
   error message and returns the address of the first offending byte.
   Otherwise it returns zero. */
#define VALGRIND_CHECK_WRITABLE(_qzz_addr,_qzz_len)                \
   ({unsigned int _qzz_res;                                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__CHECK_WRITABLE,            \
                            _qzz_addr, _qzz_len, 0, 0);            \
    _qzz_res;                                                      \
   })

/* Check that memory at _qzz_addr is addressible and defined for
   _qzz_len bytes.  If suitable addressibility and definedness are not
   established, Valgrind prints an error message and returns the
   address of the first offending byte.  Otherwise it returns zero. */
#define VALGRIND_CHECK_READABLE(_qzz_addr,_qzz_len)                \
   ({unsigned int _qzz_res;                                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__CHECK_READABLE,            \
                            _qzz_addr, _qzz_len, 0, 0);            \
    _qzz_res;                                                      \
   })

/* Use this macro to force the definedness and addressibility of a
   value to be checked.  If suitable addressibility and definedness
   are not established, Valgrind prints an error message and returns
   the address of the first offending byte.  Otherwise it returns
   zero. */
#define VALGRIND_CHECK_DEFINED(__lvalue)                           \
   (void)                                                          \
   VALGRIND_CHECK_READABLE(                                        \
      (volatile unsigned char *)&(__lvalue),                       \
                      (unsigned int)(sizeof (__lvalue)))

/* Mark memory, intended to be on the client's stack, at _qzz_addr as
   unaddressible and undefined for _qzz_len bytes.  Does not return a
   value.  The record associated with this setting will be
   automatically removed by Valgrind when the containing routine
   exits. */
#define VALGRIND_MAKE_NOACCESS_STACK(_qzz_addr,_qzz_len)           \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__MAKE_NOACCESS_STACK,       \
                            _qzz_addr, _qzz_len, 0, 0);            \
   }



/* Do a memory leak check mid-execution.
   Currently implemented but untested.
*/
#define VALGRIND_DO_LEAK_CHECK                                     \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DO_LEAK_CHECK,             \
                            0, 0, 0, 0);                           \
   }


#endif
