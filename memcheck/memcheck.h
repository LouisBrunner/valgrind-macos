
/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (memcheck.h) only.  The entire rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2.  See
   the COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2005 Julian Seward.  All rights reserved.

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

#include "valgrind.h"

typedef
   enum { 
      VG_USERREQ__MAKE_NOACCESS = VG_USERREQ_TOOL_BASE('M','C'),
      VG_USERREQ__MAKE_WRITABLE,
      VG_USERREQ__MAKE_READABLE,
      VG_USERREQ__DISCARD,
      VG_USERREQ__CHECK_WRITABLE,
      VG_USERREQ__CHECK_READABLE,
      VG_USERREQ__DO_LEAK_CHECK,
      VG_USERREQ__COUNT_LEAKS,

      /* These two have been moved into core, because they are useful for
         any tool that tracks heap blocks.  Hence the suffix.  But they're
         still here for backwards compatibility, although Valgrind will
         abort with an explanatory message if you use them. */
      VG_USERREQ__MALLOCLIKE_BLOCK__OLD_DO_NOT_USE,
      VG_USERREQ__FREELIKE_BLOCK__OLD_DO_NOT_USE,

      VG_USERREQ__GET_VBITS,
      VG_USERREQ__SET_VBITS,

      VG_USERREQ__CREATE_BLOCK,

      /* This is just for memcheck's internal use - don't use it */
      _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR 
         = VG_USERREQ_TOOL_BASE('M','C') + 256
   } Vg_MemCheckClientRequest;



/* Client-code macros to manipulate the state of memory. */

/* Mark memory at _qzz_addr as unaddressible and undefined for
   _qzz_len bytes.   */
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

/* Create a block-description handle.  The description is an ascii
   string which is included in any messages pertaining to addresses
   within the specified memory range.  Has no other effect on the
   properties of the memory range. */
#define VALGRIND_CREATE_BLOCK(_qzz_addr,_qzz_len, _qzz_desc)	\
	(__extension__({unsigned int _qzz_res;			\
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* default return */,	\
                            VG_USERREQ__CREATE_BLOCK,           \
                            _qzz_addr, _qzz_len, _qzz_desc, 0);	\
    _qzz_res;							\
   }))

/* Discard a block-description-handle. Returns 1 for an
   invalid handle, 0 for a valid handle. */
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
   VALGRIND_CHECK_READABLE(                                        \
      (volatile unsigned char *)&(__lvalue),                       \
                      (unsigned int)(sizeof (__lvalue)))

/* Do a memory leak check mid-execution.  */
#define VALGRIND_DO_LEAK_CHECK                                     \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DO_LEAK_CHECK,             \
                            0, 0, 0, 0);                           \
   }

/* Just display summaries of leaked memory, rather than all the
   details */
#define VALGRIND_DO_QUICK_LEAK_CHECK				   \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DO_LEAK_CHECK,             \
                            1, 0, 0, 0);                           \
   }

/* Return number of leaked, dubious, reachable and suppressed bytes found by
   all previous leak checks.  They must be lvalues. */
#define VALGRIND_COUNT_LEAKS(leaked, dubious, reachable, suppressed)    \
   {unsigned int _qzz_res;                                              \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                                \
                            VG_USERREQ__COUNT_LEAKS,                    \
                            &leaked, &dubious, &reachable, &suppressed);\
   }


/* These two have been moved to valgrind.h;  still here so that a warning can
   be printed out for any programs using the old ones. */
#define VALGRIND_MALLOCLIKE_BLOCK__OLD_DO_NOT_USE(addr, sizeB, rzB, is_zeroed)\
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__MALLOCLIKE_BLOCK,          \
                            addr, sizeB, rzB, is_zeroed);          \
   }
#define VALGRIND_FREELIKE_BLOCK__OLD_DO_NOT_USE(addr, rzB)         \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__FREELIKE_BLOCK,            \
                            addr, rzB, 0, 0);                      \
   }


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

#endif

