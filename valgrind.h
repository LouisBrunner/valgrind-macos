
/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org

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

   The GNU General Public License is contained in the file LICENSE.
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
