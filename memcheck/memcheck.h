
/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (memcheck.h) only.  The entire rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2.  See
   the COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors.

   Copyright (C) 2002-2003 Julian Seward.  All rights reserved.

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
   (memcheck.h) only.  The entire rest of Valgrind is licensed under
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

#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"

typedef
   enum { 
      VG_USERREQ__MAKE_NOACCESS = VG_USERREQ_SKIN_BASE('M','C'),
      VG_USERREQ__MAKE_WRITABLE,
      VG_USERREQ__MAKE_READABLE,
      VG_USERREQ__DISCARD,
      VG_USERREQ__CHECK_WRITABLE,
      VG_USERREQ__CHECK_READABLE,
      VG_USERREQ__DO_LEAK_CHECK,
      VG_USERREQ__COUNT_LEAKS,
      VG_USERREQ__MALLOCLIKE_BLOCK,
      VG_USERREQ__FREELIKE_BLOCK,
      VG_USERREQ__GET_VBITS,
      VG_USERREQ__SET_VBITS
   } Vg_MemCheckClientRequest;



/* Client-code macros to manipulate the state of memory. */

/* Mark memory at _qzz_addr as unaddressible and undefined for
   _qzz_len bytes.  Returns an int handle pertaining to the block
   descriptions Valgrind will use in subsequent error messages. */
#define VALGRIND_MAKE_NOACCESS(_qzz_addr,_qzz_len)               \
   (__extension__({unsigned int _qzz_res;                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_NOACCESS,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   }))
      
/* Similarly, mark memory at _qzz_addr as addressible but undefined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_WRITABLE(_qzz_addr,_qzz_len)               \
   (__extension__({unsigned int _qzz_res;                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_WRITABLE,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   }))

/* Similarly, mark memory at _qzz_addr as addressible and defined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_READABLE(_qzz_addr,_qzz_len)               \
   (__extension__({unsigned int _qzz_res;                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__MAKE_READABLE,           \
                            _qzz_addr, _qzz_len, 0, 0);          \
    _qzz_res;                                                    \
   }))

/* Discard a block-description-handle obtained from the above three
   macros.  After this, Valgrind will no longer be able to relate
   addressing errors to the user-defined block associated with the
   handle.  The permissions settings associated with the handle remain
   in place.  Returns 1 for an invalid handle, 0 for a valid
   handle. */
#define VALGRIND_DISCARD(_qzz_blkindex)                          \
   (__extension__ ({unsigned int _qzz_res;                       \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,    \
                            VG_USERREQ__DISCARD,                 \
                            0, _qzz_blkindex, 0, 0);             \
    _qzz_res;                                                    \
   }))


/* Client-code macros to check the state of memory. */

/* Check that memory at _qzz_addr is addressible for _qzz_len bytes.
   If suitable addressibility is not established, Valgrind prints an
   error message and returns the address of the first offending byte.
   Otherwise it returns zero. */
#define VALGRIND_CHECK_WRITABLE(_qzz_addr,_qzz_len)                \
   (__extension__({unsigned int _qzz_res;                          \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__CHECK_WRITABLE,            \
                            _qzz_addr, _qzz_len, 0, 0);            \
    _qzz_res;                                                      \
   }))

/* Check that memory at _qzz_addr is addressible and defined for
   _qzz_len bytes.  If suitable addressibility and definedness are not
   established, Valgrind prints an error message and returns the
   address of the first offending byte.  Otherwise it returns zero. */
#define VALGRIND_CHECK_READABLE(_qzz_addr,_qzz_len)                \
   (__extension__({unsigned int _qzz_res;                          \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__CHECK_READABLE,            \
                            _qzz_addr, _qzz_len, 0, 0);            \
    _qzz_res;                                                      \
   }))

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

/* Mark a block of memory as having been allocated by a malloc()-like
   function.  `addr' is the start of the usable block (ie. after any
   redzone) `rzB' is redzone size if the allocator can apply redzones;
   use '0' if not.  Adding redzones makes it more likely Valgrind will spot
   block overruns.  `is_zeroed' indicates if the memory is zeroed, as it is
   for calloc().  Put it immediately after the point where a block is
   allocated. 
   
   If you're allocating memory via superblocks, and then handing out small
   chunks of each superblock, if you don't have redzones on your small
   blocks, it's worth marking the superblock with VALGRIND_MAKE_NOACCESS
   when it's created, so that block overruns are detected.  But if you can
   put redzones on, it's probably better to not do this, so that messages
   for small overruns are described in terms of the small block rather than
   the superblock (but if you have a big overrun that skips over a redzone,
   you could miss an error this way).  See memcheck/tests/custom_alloc.c
   for an example.

   Nb: block must be freed via a free()-like function specified
   with VALGRIND_FREELIKE_BLOCK or mismatch errors will occur. */
#define VALGRIND_MALLOCLIKE_BLOCK(addr, sizeB, rzB, is_zeroed)     \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__MALLOCLIKE_BLOCK,          \
                            addr, sizeB, rzB, is_zeroed);          \
   }

/* Mark a block of memory as having been freed by a free()-like function.
   `rzB' is redzone size;  it must match that given to
   VALGRIND_MALLOCLIKE_BLOCK.  Memory not freed will be detected by the leak
   checker.  Put it immediately after the point where the block is freed. */
#define VALGRIND_FREELIKE_BLOCK(addr, rzB)                         \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__FREELIKE_BLOCK,            \
                            addr, rzB, 0, 0);                      \
   }

/* Do a memory leak check mid-execution.  */
#define VALGRIND_DO_LEAK_CHECK                                     \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DO_LEAK_CHECK,             \
                            0, 0, 0, 0);                           \
   }

/* Return number of leaked, dubious, reachable and suppressed bytes found by
   all previous leak checks.  They must be lvalues. */
#define VALGRIND_COUNT_LEAKS(leaked, dubious, reachable, suppressed)    \
   {unsigned int _qzz_res;                                              \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                                \
                            VG_USERREQ__COUNT_LEAKS,                    \
                            &leaked, &dubious, &reachable, &suppressed);\
   }

#endif


/* Get in zzvbits the validity data for the zznbytes starting at
   zzsrc.  Return values:
      0   if not running on valgrind
      1   success
      2   if zzsrc/zzvbits arrays are not aligned 0 % 4, or
          zznbytes is not 0 % 4.
      3   if any parts of zzsrc/zzvbits are not addressible.
   The metadata is not copied in cases 0, 2 or 3 so it should be
   impossible to segfault your system by using this call.
*/
#define VALGRIND_GET_VBITS(zzsrc,zzvbits,zznbytes)               \
   (__extension__({unsigned int _qzz_res;                        \
    char* czzsrc   = (char*)zzsrc;                               \
    char* czzvbits = (char*)zzvbits;                             \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                         \
                            VG_USERREQ__GET_VBITS,               \
                            czzsrc, czzvbits, zznbytes,0 );      \
    _qzz_res;                                                    \
   }))

/* Apply the validity data in zzvbits to the zznbytes starting at
   zzdst.  Return values:
      0   if not running on valgrind
      1   success
      2   if zzdst/zzvbits arrays are not aligned 0 % 4, or
          zznbytes is not 0 % 4.
      3   if any parts of zzdst/zzvbits are not addressible.
   The metadata is not copied in cases 0, 2 or 3 so it should be
   impossible to segfault your system by using this call.
*/
#define VALGRIND_SET_VBITS(zzdst,zzvbits,zznbytes)               \
   (__extension__({unsigned int _qzz_res;                        \
    char* czzdst   = (char*)zzdst;                               \
    char* czzvbits = (char*)zzvbits;                             \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                         \
                            VG_USERREQ__SET_VBITS,               \
                            czzdst, czzvbits, zznbytes,0 );      \
    _qzz_res;                                                    \
   }))
