#include <stdint.h>
#include <stdio.h>
#include "valgrind.h"
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
