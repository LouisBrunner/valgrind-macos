
/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (valgrind.h) only.  The entire rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2.  See
   the COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

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
   (valgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ---------------------------------------------------------------- 
*/


#ifndef __VALGRIND_H
#define __VALGRIND_H


/* This file is for inclusion into client (your!) code.

   You can use these macros to manipulate and query memory permissions
   inside your own programs.

   The resulting executables will still run without Valgrind, just a
   little bit more slowly than they otherwise would, but otherwise
   unchanged.  

   When run on Valgrind with --client-perms=yes, Valgrind observes
   these macro calls and takes appropriate action.  When run on
   Valgrind with --client-perms=no (the default), Valgrind observes
   these macro calls but does not take any action as a result.  */



/* This defines the magic code sequence which the JITter spots and
   handles magically.  Don't look too closely at this; it will rot
   your brain.  Valgrind dumps the result value in %EDX, so we first
   copy the default value there, so that it is returned when not
   running on Valgrind.  Since %EAX points to a block of mem
   containing the args, you can pass as many args as you want like
   this.  Currently this is set up to deal with 4 args since that's
   the max that we appear to need (pthread_create).  
*/
#define VALGRIND_MAGIC_SEQUENCE(                                        \
        _zzq_rlval,   /* result lvalue */                               \
        _zzq_default, /* result returned when running on real CPU */    \
        _zzq_request, /* request code */                                \
        _zzq_arg1,    /* request first param */                         \
        _zzq_arg2,    /* request second param */                        \
        _zzq_arg3,    /* request third param */                         \
        _zzq_arg4     /* request fourth param */ )                      \
                                                                        \
  { volatile unsigned int _zzq_args[5];                                 \
    _zzq_args[0] = (volatile unsigned int)(_zzq_request);               \
    _zzq_args[1] = (volatile unsigned int)(_zzq_arg1);                  \
    _zzq_args[2] = (volatile unsigned int)(_zzq_arg2);                  \
    _zzq_args[3] = (volatile unsigned int)(_zzq_arg3);                  \
    _zzq_args[4] = (volatile unsigned int)(_zzq_arg4);                  \
    asm volatile("movl %1, %%eax\n\t"                                   \
                 "movl %2, %%edx\n\t"                                   \
                 "roll $29, %%eax ; roll $3, %%eax\n\t"                 \
                 "rorl $27, %%eax ; rorl $5, %%eax\n\t"                 \
                 "roll $13, %%eax ; roll $19, %%eax\n\t"                \
                 "movl %%edx, %0\t"                                     \
                 : "=r" (_zzq_rlval)                                    \
                 : "r" (&_zzq_args[0]), "r" (_zzq_default)              \
                 : "eax", "edx", "cc", "memory"                         \
                );                                                      \
  }


/* Some request codes.  There are many more of these, but most are not
   exposed to end-user view.  These are the public ones, all of the
   form 0x1000 + small_number. 
*/

#define VG_USERREQ__MAKE_NOACCESS        0x1001
#define VG_USERREQ__MAKE_WRITABLE        0x1002
#define VG_USERREQ__MAKE_READABLE        0x1003
#define VG_USERREQ__DISCARD              0x1004
#define VG_USERREQ__CHECK_WRITABLE       0x1005
#define VG_USERREQ__CHECK_READABLE       0x1006
#define VG_USERREQ__MAKE_NOACCESS_STACK  0x1007
#define VG_USERREQ__RUNNING_ON_VALGRIND  0x1008
#define VG_USERREQ__DO_LEAK_CHECK        0x1009 /* untested */
#define VG_USERREQ__DISCARD_TRANSLATIONS 0x100A


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


/* Returns 1 if running on Valgrind, 0 if running on the real CPU. 
   Currently implemented but untested. */
#define RUNNING_ON_VALGRIND                                        \
   ({unsigned int _qzz_res;                                        \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0 /* returned if not */,     \
                            VG_USERREQ__RUNNING_ON_VALGRIND,       \
                            0, 0, 0, 0);                           \
    _qzz_res;                                                      \
   })


/* Mark memory, intended to be on the client's stack, at _qzz_addr as
   unaddressible and undefined for _qzz_len bytes.  Does not return a
   value.  The record associated with this setting will be
   automatically removed by Valgrind when the containing routine
   exits.  

   Currently implemented but untested.
*/
#define VALGRIND_DO_LEAK_CHECK                                     \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DO_LEAK_CHECK,             \
                            0, 0, 0, 0);                           \
   }


/* Discard translation of code in the range [_qzz_addr .. _qzz_addr +
   _qzz_len - 1].  Useful if you are debugging a JITter or some such,
   since it provides a way to make sure valgrind will retranslate the
   invalidated area.  Returns no value. */
#define VALGRIND_DISCARD_TRANSLATIONS(_qzz_addr,_qzz_len)          \
   {unsigned int _qzz_res;                                         \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0,                           \
                            VG_USERREQ__DISCARD_TRANSLATIONS,      \
                            _qzz_addr, _qzz_len, 0, 0);            \
   }


#endif
