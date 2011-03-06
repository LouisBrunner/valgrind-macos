#include <stdint.h>
#include <stdio.h>
#include "valgrind.h"
#include "pub_tool_redir.h"

#if 0

/*
 * On Mac OS X shared library functions are lazily bound. The binding mechanism
 * uses self-modifying code. Intercept dyld_stub_binder() in order to suppress
 * the data accesses involved in this mechanism.
 *
 * See also the Mac OS X ABI Dynamic Loader Reference (http://developer.apple.com/library/mac/#documentation/DeveloperTools/Reference/MachOReference/Reference/reference.html#//apple_ref/c/func/dyld_stub_binding_helper).
 * See also the dyld_stub_binder() source code (http://www.opensource.apple.com/source/dyld/dyld-132.13/src/dyld_stub_binder.s).
 */
void* VG_WRAP_FUNCTION_ZZ(libSystemZdZaZddylib, dyldZustubZubinder)
     (void** imageLoaderCache, uintptr_t lazyBindingInfoOffset);
void* VG_WRAP_FUNCTION_ZZ(libSystemZdZaZddylib, dyldZustubZubinder)
     (void** imageLoaderCache, uintptr_t lazyBindingInfoOffset)
{
  void* res;
  OrigFn fn;
  
  VALGRIND_GET_ORIG_FN(fn);

  CALL_FN_W_WW(res, fn, imageLoaderCache, lazyBindingInfoOffset);

  return res;
}

#endif
