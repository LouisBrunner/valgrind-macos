
/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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
   your brain.  
*/
#define VALGRIND_MAGIC_SEQUENCE(_zzq_res,_zzq_code,_zzq_addr,_zzq_len)  \
  asm volatile("movl %1, %%eax\n\t"                                     \
               "movl %2, %%ebx\n\t"                                     \
               "movl %3, %%ecx\n\t"                                     \
               "roll $29, %%eax ; roll $3, %%eax\n\t"                   \
               "roll $27, %%eax ; roll $5, %%eax\n\t"                   \
               "movl %%eax, %0\t"                                       \
               : "=r" (_zzq_res)                                        \
               : "r" (_zzq_code), "r" (_zzq_addr), "r" (_zzq_len)       \
               : "eax", "ebx", "ecx", "cc", "memory"                    \
              );



/* Client-code macros to manipulate the state of memory. */

/* Mark memory at _qzz_addr as unaddressible and undefined for
   _qzz_len bytes.  Returns an int handle pertaining to the block
   descriptions Valgrind will use in subsequent error messages. */
#define VALGRIND_MAKE_NOACCESS(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,1001,_qzz_addr,_qzz_len);   \
    _qzz_res;                                                    \
   })

/* Similarly, mark memory at _qzz_addr as addressible but undefined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_WRITABLE(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,1002,_qzz_addr,_qzz_len);   \
    _qzz_res;                                                    \
   })

/* Similarly, mark memory at _qzz_addr as addressible and defined
   for _qzz_len bytes. */
#define VALGRIND_MAKE_READABLE(_qzz_addr,_qzz_len)               \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,1003,_qzz_addr,_qzz_len);   \
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
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,2004,0,_qzz_blkindex);      \
    _qzz_res;                                                    \
   })



/* Client-code macros to check the state of memory. */

/* Check that memory at _qzz_addr is addressible for _qzz_len bytes.
   If suitable addressibility is not established, Valgrind prints an
   error message and returns the address of the first offending byte.
   Otherwise it returns zero. */
#define VALGRIND_CHECK_WRITABLE(_qzz_addr,_qzz_len)              \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,2002,_qzz_addr,_qzz_len);   \
    _qzz_res;                                                    \
   })

/* Check that memory at _qzz_addr is addressible and defined for
   _qzz_len bytes.  If suitable addressibility and definedness are not
   established, Valgrind prints an error message and returns the
   address of the first offending byte.  Otherwise it returns zero. */
#define VALGRIND_CHECK_READABLE(_qzz_addr,_qzz_len)              \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,2003,_qzz_addr,_qzz_len);   \
    _qzz_res;                                                    \
   })


/* Use this macro to force the definedness and addressibility of a
   value to be checked.  If suitable addressibility and definedness
   are not established, Valgrind prints an error message and returns
   the address of the first offending byte.  Otherwise it returns
   zero. */
#define VALGRIND_CHECK_DEFINED(__lvalue)                         \
   (void)                                                        \
   VALGRIND_CHECK_READABLE(                                      \
      (volatile unsigned char *)&(__lvalue),                     \
                      (unsigned int)(sizeof (__lvalue)))



/* Mark memory, intended to be on the client's stack, at _qzz_addr as
   unaddressible and undefined for _qzz_len bytes.  Does not return a
   value.  The record associated with this setting will be
   automatically removed by Valgrind when the containing routine
   exits. */
#define VALGRIND_MAKE_NOACCESS_STACK(_qzz_addr,_qzz_len)         \
   ({unsigned int _qzz_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qzz_res,3001,_qzz_addr,_qzz_len);   \
    _qzz_res;                                                    \
   })


#endif
