/*
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (helgrind.h) only.  The entire rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2.  See
   the COPYING file in the source distribution for details.

   ----------------------------------------------------------------

   This file is part of helgrind, a Valgrind skin for detecting
   data races in threaded programs.

   Copyright (C) 2002-2003 Nicholas Nethercote.  All rights reserved.

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
      VG_USERREQ__HG_CLEAN_MEMORY = VG_USERREQ_SKIN_BASE('H','G'),
      VG_USERREQ__HG_KNOWN_RACE
   } Vg_HelgrindClientRequest;

/* Clean memory state.  This makes Helgrind forget everything it knew
   about the specified memory range, and resets it to virgin.  This is
   particularly useful for memory allocators who wish to recycle
   memory. */
#define VALGRIND_HG_CLEAN_MEMORY(_qzz_start, _qzz_len)			\
   do {									\
     unsigned int _qzz_res;						\
     VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__HG_CLEAN_MEMORY,	\
			     _qzz_start, _qzz_len, 0, 0);		\
     (void)0;								\
   } while(0)

/* Mark memory as known racy.  This puts the memory range specified
   into the error state, so that data race errors are not reported
   against it. */
#define VALGRIND_HG_KNOWN_RACE(_qzz_start, _qzz_len)			\
   do {									\
     unsigned int _qzz_res;						\
     VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__HG_KNOWN_RACE,	\
			     _qzz_start, _qzz_len, 0, 0);		\
     (void)0;								\
   } while(0)

#endif /* __HELGRIND_H */
