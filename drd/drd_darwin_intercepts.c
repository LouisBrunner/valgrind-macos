/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2013 Bart Van Assche <bvanassche@acm.org>.

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

#include <stdint.h>
#include <stdio.h>
#include "pub_tool_clreq.h"
#include "drd.h"
#include "pub_tool_redir.h"

/*
 * On Mac OS X shared library functions are lazily bound. The binding mechanism
 * uses self-modifying code. Intercept fastBindLazySymbol() in order to suppress
 * the data accesses involved in this mechanism.
 *
 * See also the Mac OS X ABI Dynamic Loader Reference (http://developer.apple.com/library/mac/#documentation/DeveloperTools/Reference/MachOReference/Reference/reference.html#//apple_ref/c/func/dyld_stub_binding_helper).
 * See also the dyld_stub_binder() source code (http://www.opensource.apple.com/source/dyld/dyld-132.13/src/dyld_stub_binder.s).
 * See also the dyld::fastBindLazySymbol() source code (http://opensource.apple.com/source/dyld/dyld-132.13/src/dyld.cpp).
 */
void* VG_WRAP_FUNCTION_ZZ(dyld, ZuZZN4dyld18fastBindLazySymbolEPP11ImageLoaderm)
     (void** imageLoaderCache, uintptr_t lazyBindingInfoOffset);
void* VG_WRAP_FUNCTION_ZZ(dyld, ZuZZN4dyld18fastBindLazySymbolEPP11ImageLoaderm)
     (void** imageLoaderCache, uintptr_t lazyBindingInfoOffset)
{
   void* res;
   OrigFn fn;

   VALGRIND_GET_ORIG_FN(fn);

   ANNOTATE_IGNORE_READS_AND_WRITES_BEGIN();
   CALL_FN_W_WW(res, fn, imageLoaderCache, lazyBindingInfoOffset);
   ANNOTATE_IGNORE_READS_AND_WRITES_END();

   return res;
}
